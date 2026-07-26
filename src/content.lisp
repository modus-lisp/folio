;;;; src/content.lisp — the content-stream interpreter (ISO 32000-1 §8, §9).
;;;;
;;;; A page's decoded content bytes are a sequence of operators in postfix form:
;;;; operands (numbers, strings, names, arrays, inline-image dicts) accumulate on
;;;; a stack until a bareword *operator* is read, which consumes them.  We drive
;;;; gesso's 2D context (the same engine weft paints SVG/<canvas> through): the
;;;; PDF graphics state maps almost one-for-one onto gesso's — CTM -> transform,
;;;; path ops -> move-to/line-to/bezier-curve-to, paint -> fill-path/stroke-path,
;;;; W/W* -> clip, device colours -> set-fill/set-stroke.
;;;;
;;;; Milestone 1 renders VECTOR graphics only.  Text operators (§9) and the
;;;; XObject/inline-image/shading operators are PARSED so a real page's stream
;;;; runs to completion, but their marks are deferred to later milestones.

(in-package #:folio)

;;; ---- PDF graphics state (the parts gesso does not itself track) ------------
;;; gesso's gstate already carries the CTM, fill/stroke colour, line width, dash
;;; and clip (saved/restored by gesso:save / gesso:restore).  Alongside it we keep
;;; a parallel stack for PDF-specific state: a mirror of the CTM (so we can derive
;;; the device line-width scale without a public accessor), the *user-space* line
;;; width and dash (PDF measures them in user space; gesso wants device space),
;;; and the selected colour spaces (so sc/scn operands are interpreted correctly).

(defstruct (gstate2 (:constructor %make-gstate2) (:copier %copy-gstate2))
  (ctm (gesso:mat-identity))            ; mirror of gesso's CTM (user->device)
  (line-width 1d0)                      ; user-space line width
  (dash nil)                            ; user-space dash array (or NIL = solid)
  (dash-phase 0d0)
  (line-cap 0) (line-join 0) (miter 10d0)
  (fill-space :gray)                    ; :gray :rgb :cmyk :other
  (stroke-space :gray)
  (fill-base nil)                       ; for Indexed: (base-space . lookup-bytes)
  (stroke-base nil))

(defun copy-gstate2 (s)
  (let ((n (%copy-gstate2 s)))
    (setf (gstate2-ctm n) (copy-list (gstate2-ctm s)))
    n))

(defstruct interp
  ctx                                   ; the gesso context we paint through
  state                                 ; the current gstate2
  (gstack nil)                          ; stack of saved gstate2 (parallel to gesso)
  resources                             ; the page /Resources dict (fonts/xobjects)
  doc                                   ; owning document (for resolving indirects)
  ;; pending clip: NIL, :nonzero or :evenodd, applied AFTER the next paint op
  (pending-clip nil)
  ;; text object state (parsed, not yet rendered — Milestone 2)
  (in-text nil))

;;; ---- numeric coercion -----------------------------------------------------
(declaim (inline num))
(defun num (x) (if (numberp x) (float x 1d0) 0d0))

;;; ---- colour ---------------------------------------------------------------
(defun cmyk->rgb (c m y k)
  "Naive CMYK -> RGB (ISO 32000-1 §8.6.4.4): r = (1-c)(1-k), etc."
  (flet ((ch (x) (max 0 (min 255 (round (* 255d0 (* (- 1d0 x) (- 1d0 k))))))))
    (list (ch c) (ch m) (ch y))))

(defun gray->rgb (g)
  (let ((v (max 0 (min 255 (round (* 255d0 g)))))) (list v v v)))

(defun rgb255 (r g b)
  (flet ((ch (x) (max 0 (min 255 (round (* 255d0 x))))))
    (list (ch r) (ch g) (ch b))))

(defun name-color-space (nm)
  "Map a device colour-space name to a keyword; unknown families -> :other."
  (cond ((or (string= nm "DeviceGray") (string= nm "G") (string= nm "CalGray")) :gray)
        ((or (string= nm "DeviceRGB") (string= nm "RGB") (string= nm "CalRGB")
             (string= nm "Lab")) :rgb)
        ((or (string= nm "DeviceCMYK") (string= nm "CMYK")) :cmyk)
        ((string= nm "Pattern") :pattern)
        (t :other)))

(defun resolve-color-space (ip name)
  "Resolve a CS/cs operand NAME to (values kind base) where KIND is a colour-space
keyword.  Named device spaces resolve directly; otherwise look the name up in the
page's /Resources /ColorSpace and inspect the array (ICCBased -> /N components,
Indexed -> :indexed, Separation/DeviceN -> alternate).  Unknown -> :other."
  (let ((kind (name-color-space name)))
    (unless (eq kind :other)
      (return-from resolve-color-space (values kind nil)))
    ;; look up in resources
    (let* ((doc (interp-doc ip))
           (csd (and (interp-resources ip)
                     (resolve doc (dict-get (interp-resources ip) "ColorSpace"))))
           (spec (and (hash-table-p csd) (resolve doc (dict-get csd name)))))
      (cond
        ((pdf-name-p spec) (values (name-color-space (pdf-name-string spec)) nil))
        ((pdf-array-p spec)
         (let ((head (and (plusp (length spec)) (resolve doc (aref spec 0)))))
           (cond
             ((and (pdf-name-p head) (string= (pdf-name-string head) "ICCBased"))
              (let* ((strm (resolve doc (aref spec 1)))
                     (n (and (pdf-stream-p strm)
                             (resolve doc (dict-get (pdf-stream-dict strm) "N")))))
                (values (case n (1 :gray) (4 :cmyk) (t :rgb)) nil)))
             ((and (pdf-name-p head)
                   (member (pdf-name-string head) '("CalRGB" "Lab") :test #'string=))
              (values :rgb nil))
             ((and (pdf-name-p head) (string= (pdf-name-string head) "CalGray"))
              (values :gray nil))
             ((and (pdf-name-p head)
                   (member (pdf-name-string head) '("Separation" "DeviceN") :test #'string=))
              ;; a tint transform we don't evaluate; treat 1 - tint as gray
              (values :separation nil))
             ((and (pdf-name-p head) (string= (pdf-name-string head) "Indexed"))
              (multiple-value-bind (bk) (indexed-lookup ip doc spec)
                (values :indexed bk)))
             ((and (pdf-name-p head) (string= (pdf-name-string head) "Pattern"))
              (values :pattern nil))
             (t (values :other nil)))))
        (t (values kind nil))))))

(defun indexed-lookup (ip doc spec)
  "For an [/Indexed base hival lookup] array, return (values (base-kind . lut))."
  (declare (ignore ip))
  (let* ((base (resolve doc (aref spec 1)))
         (lut (resolve doc (aref spec 3)))
         (base-kind (cond ((pdf-name-p base) (name-color-space (pdf-name-string base)))
                          ((pdf-array-p base)
                           (let ((h (resolve doc (aref base 0))))
                             (cond ((and (pdf-name-p h)
                                         (string= (pdf-name-string h) "ICCBased")) :rgb)
                                   (t :rgb))))
                          (t :rgb)))
         (bytes (cond ((pdf-string-p lut) (pdf-string-bytes lut))
                      ((pdf-stream-p lut)
                       (ignore-errors (decode-stream lut :resolver (lambda (v) (resolve doc v)))))
                      (t nil))))
    (cons base-kind bytes)))

(defun components-per (kind)
  (case kind (:gray 1) (:rgb 3) (:cmyk 4) (:separation 1) (:indexed 1) (t 3)))

(defun interpret-color (kind base comps)
  "Turn numeric colour COMPS under colour-space KIND into an 8-bit (r g b) list."
  (case kind
    (:gray (gray->rgb (num (first comps))))
    (:rgb  (rgb255 (num (first comps)) (num (second comps)) (num (third comps))))
    (:cmyk (cmyk->rgb (num (first comps)) (num (second comps))
                      (num (third comps)) (num (fourth comps))))
    (:separation (gray->rgb (- 1d0 (num (first comps))))) ; ink amount -> darkness
    (:indexed
     (let* ((idx (max 0 (round (num (first comps)))))
            (bk (car base)) (lut (cdr base))
            (nc (components-per (or bk :rgb))))
       (if (and lut (<= (* (1+ idx) nc) (length lut)))
           (let ((vals (loop for i below nc
                             collect (/ (aref lut (+ (* idx nc) i)) 255d0))))
             (interpret-color (or bk :rgb) nil vals))
           '(0 0 0))))
    (t ;; :other / unknown: interpret by component count as a best effort
     (case (length comps)
       (1 (gray->rgb (num (first comps))))
       (3 (rgb255 (num (first comps)) (num (second comps)) (num (third comps))))
       (4 (cmyk->rgb (num (first comps)) (num (second comps))
                     (num (third comps)) (num (fourth comps))))
       (t '(0 0 0))))))

;;; ---- state stack ----------------------------------------------------------
;;; NB: `q` and `Q` differ only in case; as function names they would read as the
;;; SAME symbol (the reader upcases), so we give them distinct names.
(defun op-save (ip)
  (gesso:save (interp-ctx ip))
  (push (copy-gstate2 (interp-state ip)) (interp-gstack ip)))

(defun op-restore (ip)
  (gesso:restore (interp-ctx ip))
  (when (interp-gstack ip)
    (setf (interp-state ip) (pop (interp-gstack ip)))))

(defun op-cm (ip a b c d e f)
  (let* ((m (list (num a) (num b) (num c) (num d) (num e) (num f)))
         (s (interp-state ip)))
    (setf (gstate2-ctm s) (gesso:mat-mul (gstate2-ctm s) m))
    (gesso:transform (interp-ctx ip) (num a) (num b) (num c) (num d) (num e) (num f))))

;;; ---- line width in device space -------------------------------------------
(defun ctm-scale (m)
  "A scalar approximating the CTM's linear scale (sqrt of the |determinant|)."
  (destructuring-bind (a b c d e f) m
    (declare (ignore e f))
    (sqrt (max 1d-9 (abs (- (* a d) (* b c)))))))

(defun apply-line-width (ip)
  "Set gesso's device line width from the user-space width * the CTM scale.
A user width of 0 means the thinnest device line (1 px), per §8.4.3.2."
  (let* ((s (interp-state ip))
         (sc (ctm-scale (gstate2-ctm s)))
         (dw (* (gstate2-line-width s) sc)))
    (gesso:set-line-width (interp-ctx ip) (if (<= dw 0d0) 1d0 dw))
    (when (gstate2-dash s)
      (gesso:set-line-dash (interp-ctx ip)
                           (mapcar (lambda (x) (* (num x) sc)) (gstate2-dash s))
                           (* (gstate2-dash-phase s) sc)))))

;;; ---- painting -------------------------------------------------------------
(defun do-paint (ip &key close fill stroke (fill-rule :nonzero))
  "Execute a path-painting operator, then honour a pending W/W* clip and reset
the path.  CLOSE closes the current subpath first; FILL / STROKE select ops."
  (let ((ctx (interp-ctx ip)))
    (when close (gesso:close-path ctx))
    (when fill
      (gesso:set-fill-rule ctx fill-rule)
      (gesso:fill-path ctx))
    (when stroke
      (apply-line-width ip)
      (gesso:stroke-path ctx))
    ;; §8.5.4: the clip is intersected AFTER the path is painted.
    (when (interp-pending-clip ip)
      (gesso:clip ctx (interp-pending-clip ip))
      (setf (interp-pending-clip ip) nil))
    (gesso:begin-path ctx)))

;;; ---- inline images (BI ... ID <bytes> EI) — parsed & skipped (M4) ----------
(defun skip-inline-image (ip lx)
  "LX is positioned just after the BI keyword.  Consume the parameter dict up to
ID, then skip the binary sample data up to the terminating EI."
  (declare (ignore ip))
  ;; consume key/value tokens until the ID keyword
  (loop for tok = (lex-next lx)
        until (or (eq (token-type tok) :eof)
                  (and (eq (token-type tok) :keyword)
                       (string= (token-value tok) "ID")))
        ;; values may be arrays/dicts; skip them structurally
        do (case (token-type tok)
             (:array-open (loop for x = (lex-next lx)
                                until (member (token-type x) '(:array-close :eof))))
             (:dict-open (loop for x = (lex-next lx)
                               until (member (token-type x) '(:dict-close :eof))))))
  ;; the byte after ID is a single whitespace separator; then raw sample data.
  (let* ((buf (lexer-buf lx)) (end (lexer-end lx)) (p (lexer-pos lx)))
    (when (and (< p end) (ws-byte-p (aref buf p))) (incf p))
    ;; scan for a whitespace-delimited EI
    (loop while (< (+ p 1) end) do
      (if (and (= (aref buf p) 69) (= (aref buf (+ p 1)) 73) ; E I
               (or (= p 0) (ws-byte-p (aref buf (1- p))))
               (or (>= (+ p 2) end) (ws-byte-p (aref buf (+ p 2)))
                   (delim-byte-p (aref buf (+ p 2)))))
          (progn (setf (lexer-pos lx) (+ p 2) (lexer-peeked lx) nil) (return))
          (incf p))
          finally (setf (lexer-pos lx) end (lexer-peeked lx) nil))))

;;; ---- the operand reader ---------------------------------------------------
(defun read-operand-array (lx)
  "Read a `[ ... ]` operand (numbers/strings/names/nested arrays); LX just past
the `[`.  Returns a Lisp list (used by text ops we currently skip)."
  (let ((out '()))
    (loop for tok = (lex-next lx) do
      (case (token-type tok)
        (:array-close (return))
        (:eof (return))
        (:int (push (token-value tok) out))
        (:real (push (token-value tok) out))
        (:string (push (make-pdf-string (token-value tok)) out))
        (:name (push (make-pdf-name (token-value tok)) out))
        (:array-open (push (read-operand-array lx) out))
        (:dict-open (push (read-operand-dict lx) out))
        (t nil)))
    (nreverse out)))

(defun read-operand-dict (lx)
  "Read a `<< ... >>` operand (inline-image / marked-content property dict)."
  (let ((h (make-hash-table :test 'equal)))
    (loop for ktok = (lex-next lx) do
      (case (token-type ktok)
        (:dict-close (return))
        (:eof (return))
        (:name (let ((v (read-one-operand lx)))
                 (setf (gethash (token-value ktok) h) v)))
        (t nil)))
    h))

(defun read-one-operand (lx)
  "Read a single operand value (for a dict value); returns a Lisp value."
  (let ((tok (lex-next lx)))
    (case (token-type tok)
      (:int (token-value tok))
      (:real (token-value tok))
      (:string (make-pdf-string (token-value tok)))
      (:name (make-pdf-name (token-value tok)))
      (:array-open (read-operand-array lx))
      (:dict-open (read-operand-dict lx))
      (:keyword (let ((s (token-value tok)))
                  (cond ((string= s "true") :true)
                        ((string= s "false") :false)
                        ((string= s "null") (pdf-null))
                        (t (make-pdf-name s)))))
      (t (pdf-null)))))

;;; ---- the main loop --------------------------------------------------------
(defparameter *text-ops*
  '("BT" "ET" "Td" "TD" "Tm" "T*" "Tc" "Tw" "Tz" "TL" "Tf" "Tr" "Ts"
    "Tj" "TJ" "'" "\"" "d0" "d1")
  "Text-showing / text-state operators (ISO 32000-1 §9).  Parsed but not painted
until Milestone 2; their operands are consumed and discarded here.")

(defun run-content (ip bytes)
  "Interpret the operator stream BYTES against interpreter IP."
  (let ((lx (make-lexer bytes))
        (ops '()))                       ; accumulated operands (reversed)
    (progn
      (loop for tok = (lex-next lx)
            until (eq (token-type tok) :eof) do
        (case (token-type tok)
          (:int  (push (token-value tok) ops))
          (:real (push (token-value tok) ops))
          (:string (push (make-pdf-string (token-value tok)) ops))
          (:name (push (make-pdf-name (token-value tok)) ops))
          (:array-open (push (read-operand-array lx) ops))
          (:dict-open (push (read-operand-dict lx) ops))
          (:keyword
           (let ((kw (token-value tok)))
             (cond
               ((string= kw "true") (push :true ops))
               ((string= kw "false") (push :false ops))
               ((string= kw "null") (push (pdf-null) ops))
               ((or (string= kw "{") (string= kw "}")) nil) ; PostScript calc — ignore
               ((string= kw "BI") (skip-inline-image ip lx) (setf ops '()))
               (t (ignore-errors (exec-op ip kw (nreverse ops)))
                  (setf ops '())))))
          (t (setf ops '())))))))

(defun exec-op (ip kw args)
  "Dispatch operator KW with its (in-order) operand list ARGS."
  (let ((ctx (interp-ctx ip)) (s (interp-state ip)))
    (macrolet ((a (i) `(nth ,i args)))
      (cond
        ;; ---- graphics state ----
        ((string= kw "q") (op-save ip))
        ((string= kw "Q") (op-restore ip))
        ((string= kw "cm") (op-cm ip (a 0) (a 1) (a 2) (a 3) (a 4) (a 5)))
        ((string= kw "w") (setf (gstate2-line-width s) (num (a 0))))
        ((string= kw "J") (setf (gstate2-line-cap s) (round (num (a 0)))))
        ((string= kw "j") (setf (gstate2-line-join s) (round (num (a 0)))))
        ((string= kw "M") (setf (gstate2-miter s) (num (a 0))))
        ((string= kw "d") (setf (gstate2-dash s) (and (consp (a 0)) (a 0))
                                (gstate2-dash-phase s) (num (a 1))))
        ((string= kw "ri") nil)
        ((string= kw "i") nil)
        ((string= kw "gs") nil)          ; ExtGState (alpha/blend) — M-later

        ;; ---- path construction ----
        ((string= kw "m") (gesso:move-to ctx (num (a 0)) (num (a 1))))
        ((string= kw "l") (gesso:line-to ctx (num (a 0)) (num (a 1))))
        ((string= kw "c") (gesso:bezier-curve-to ctx (num (a 0)) (num (a 1))
                                                 (num (a 2)) (num (a 3))
                                                 (num (a 4)) (num (a 5))))
        ((string= kw "v")                ; first control point = current point
         (let ((p (or (gesso::context-cur-user ctx) (cons (num (a 0)) (num (a 1))))))
           (gesso:bezier-curve-to ctx (car p) (cdr p)
                                  (num (a 0)) (num (a 1)) (num (a 2)) (num (a 3)))))
        ((string= kw "y")                ; second control point = end point
         (gesso:bezier-curve-to ctx (num (a 0)) (num (a 1))
                                (num (a 2)) (num (a 3)) (num (a 2)) (num (a 3))))
        ((string= kw "re")
         (gesso:rect ctx (num (a 0)) (num (a 1)) (num (a 2)) (num (a 3))))
        ((string= kw "h") (gesso:close-path ctx))

        ;; ---- path painting ----
        ((string= kw "S") (do-paint ip :stroke t))
        ((string= kw "s") (do-paint ip :close t :stroke t))
        ((or (string= kw "f") (string= kw "F")) (do-paint ip :fill t))
        ((string= kw "f*") (do-paint ip :fill t :fill-rule :evenodd))
        ((string= kw "B") (do-paint ip :fill t :stroke t))
        ((string= kw "B*") (do-paint ip :fill t :stroke t :fill-rule :evenodd))
        ((string= kw "b") (do-paint ip :close t :fill t :stroke t))
        ((string= kw "b*") (do-paint ip :close t :fill t :stroke t :fill-rule :evenodd))
        ((string= kw "n") (do-paint ip))  ; no-op paint (path used only for clip)

        ;; ---- clipping ----
        ((string= kw "W") (setf (interp-pending-clip ip) :nonzero))
        ((string= kw "W*") (setf (interp-pending-clip ip) :evenodd))

        ;; ---- device colour ----
        ((string= kw "g") (setf (gstate2-fill-space s) :gray)
         (gesso:set-fill ctx (gray->rgb (num (a 0)))))
        ((string= kw "G") (setf (gstate2-stroke-space s) :gray)
         (gesso:set-stroke ctx (gray->rgb (num (a 0)))))
        ((string= kw "rg") (setf (gstate2-fill-space s) :rgb)
         (gesso:set-fill ctx (rgb255 (num (a 0)) (num (a 1)) (num (a 2)))))
        ((string= kw "RG") (setf (gstate2-stroke-space s) :rgb)
         (gesso:set-stroke ctx (rgb255 (num (a 0)) (num (a 1)) (num (a 2)))))
        ((string= kw "k") (setf (gstate2-fill-space s) :cmyk)
         (gesso:set-fill ctx (cmyk->rgb (num (a 0)) (num (a 1)) (num (a 2)) (num (a 3)))))
        ((string= kw "K") (setf (gstate2-stroke-space s) :cmyk)
         (gesso:set-stroke ctx (cmyk->rgb (num (a 0)) (num (a 1)) (num (a 2)) (num (a 3)))))
        ((string= kw "cs") (op-set-space ip (a 0) :fill))
        ((string= kw "CS") (op-set-space ip (a 0) :stroke))
        ((or (string= kw "sc") (string= kw "scn")) (op-set-color ip args :fill))
        ((or (string= kw "SC") (string= kw "SCN")) (op-set-color ip args :stroke))

        ;; ---- XObjects / shadings — deferred (M4) ----
        ((string= kw "Do") nil)          ; Form/Image XObjects
        ((string= kw "sh") nil)          ; shading fills

        ;; ---- marked content / compatibility — no-op ----
        ((member kw '("BMC" "BDC" "EMC" "BX" "EX" "MP" "DP") :test #'string=) nil)

        ;; ---- text (§9): parsed, not rendered until Milestone 2 ----
        ((member kw *text-ops* :test #'string=)
         (cond ((string= kw "BT") (setf (interp-in-text ip) t))
               ((string= kw "ET") (setf (interp-in-text ip) nil))))

        ;; ---- unknown: operands already discarded by the caller ----
        (t nil)))))

(defun op-set-space (ip name-obj which)
  "Handle cs/CS: select a colour space and reset the colour to its initial value."
  (let ((name (if (pdf-name-p name-obj) (pdf-name-string name-obj) "DeviceGray"))
        (s (interp-state ip)) (ctx (interp-ctx ip)))
    (multiple-value-bind (kind base) (resolve-color-space ip name)
      (if (eq which :fill)
          (setf (gstate2-fill-space s) kind (gstate2-fill-base s) base)
          (setf (gstate2-stroke-space s) kind (gstate2-stroke-base s) base))
      ;; initial colour: black (0 for additive, full ink for subtractive)
      (let ((c (case kind (:cmyk '(0 0 0)) (t '(0 0 0)))))
        (if (eq which :fill) (gesso:set-fill ctx c) (gesso:set-stroke ctx c))))))

(defun op-set-color (ip args which)
  "Handle sc/scn/SC/SCN under the current colour space.  A trailing name operand
(pattern) with no numeric components paints a mid-grey placeholder (M-later)."
  (let* ((s (interp-state ip)) (ctx (interp-ctx ip))
         (kind (if (eq which :fill) (gstate2-fill-space s) (gstate2-stroke-space s)))
         (base (if (eq which :fill) (gstate2-fill-base s) (gstate2-stroke-base s)))
         (nums (remove-if-not #'numberp args))
         (has-name (some #'pdf-name-p args)))
    (let ((c (cond ((and has-name (null nums)) '(128 128 128)) ; pattern placeholder
                   ((eq kind :pattern) '(128 128 128))
                   (t (interpret-color kind base nums)))))
      (if (eq which :fill) (gesso:set-fill ctx c) (gesso:set-stroke ctx c)))))
