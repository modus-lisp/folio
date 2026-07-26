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
  (stroke-base nil)
  ;; ---- text state (ISO 32000-1 §9.3), part of the graphics state ----
  (char-spacing 0d0)                    ; Tc  (unscaled text-space units)
  (word-spacing 0d0)                    ; Tw
  (h-scale 1d0)                         ; Th = Tz/100
  (leading 0d0)                         ; TL
  (font nil)                            ; selected PDF-FONT (or NIL)
  (font-size 0d0)                       ; Tfs
  (render-mode 0)                       ; Tr (0..7)
  (rise 0d0)                            ; Ts
  ;; ---- constant alpha from ExtGState (§11.6.4.4) ----
  (fill-alpha 1d0)                      ; ca  (non-stroking alpha)
  (stroke-alpha 1d0)                    ; CA  (stroking alpha)
  ;; ---- pattern colour (§8.7): the pattern object selected by scn/SCN under a
  ;; Pattern colour space (a shading pattern paints a gradient into the filled path;
  ;; a tiling pattern paints a replicated cell).  For an UNCOLORED (PaintType 2)
  ;; tiling pattern the *-pattern-color slot holds the (r g b) the stencil paints. ----
  (fill-pattern nil)
  (stroke-pattern nil)
  (fill-pattern-color nil)
  (stroke-pattern-color nil))

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
  base-ctm                              ; the page base CTM (default coord system;
                                        ; shading patterns map pattern space through it)
  ;; pending clip: NIL, :nonzero or :evenodd, applied AFTER the next paint op
  (pending-clip nil)
  ;; text object state (§9.4): the text matrix and the text-line matrix, valid
  ;; between BT and ET (both reset to identity by BT).
  (in-text nil)
  (tm (gesso:mat-identity))             ; text matrix Tm
  (tlm (gesso:mat-identity))            ; text-line matrix Tlm
  (font-cache (make-hash-table :test 'eq)) ; font-dict -> PDF-FONT
  ;; form-XObject recursion (§8.10.1): a depth counter + a stack of the form
  ;; streams currently being interpreted, to break reference cycles.
  (form-depth 0)
  (form-stack nil)
  ;; when non-NIL (rendering an uncolored PaintType-2 tiling-pattern cell), colour
  ;; operators in the content stream are ignored — the cell is a stencil painted in
  ;; the single colour supplied with scn (§8.7.3.3).
  (uncolored-color nil))

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
              ;; [/Pattern base]: BASE (if present) is the underlying colour space
              ;; an UNCOLORED (PaintType 2) pattern's scn operands are given in.
              (values :pattern (and (>= (length spec) 2) (resolve doc (aref spec 1)))))
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

(defun synth-bold-width (ip)
  "Device stroke width for synthetic bold: ~3% of the device em, floored so a
small font still thickens visibly."
  (let* ((s (interp-state ip))
         (fs (gstate2-font-size s))
         (sc (ctm-scale (gstate2-ctm s)))
         (dev-em (* (abs fs) (abs (gstate2-h-scale s)) sc)))
    (max 0.5d0 (* dev-em 0.03d0))))

;;; ---- painting -------------------------------------------------------------
(defun do-paint (ip &key close fill stroke (fill-rule :nonzero))
  "Execute a path-painting operator, then honour a pending W/W* clip and reset
the path.  CLOSE closes the current subpath first; FILL / STROKE select ops."
  (let ((ctx (interp-ctx ip)) (s (interp-state ip)))
    (when close (gesso:close-path ctx))
    (when fill
      (let* ((pobj (and (eq (gstate2-fill-space s) :pattern) (gstate2-fill-pattern s)))
             (shpat (and pobj (shading-pattern-obj (interp-doc ip) pobj)))
             (tilepat (and pobj (not shpat)
                           (tiling-pattern-obj (interp-doc ip) pobj))))
        (cond
          ;; a shading pattern: paint its gradient clipped to the path (the path
          ;; is left intact for a following stroke / pending clip)
          (shpat (fill-with-shading-pattern ip shpat fill-rule))
          ;; a tiling pattern: replicate its cell across the path region
          (tilepat (fill-with-tiling-pattern ip tilepat fill-rule
                                             (gstate2-fill-pattern-color s)))
          (t
           (gesso:set-global-alpha ctx (gstate2-fill-alpha s))
           (gesso:set-fill-rule ctx fill-rule)
           (gesso:fill-path ctx)))))
    (when stroke
      (gesso:set-global-alpha ctx (gstate2-stroke-alpha s))
      (apply-line-width ip)
      (gesso:stroke-path ctx))
    (gesso:set-global-alpha ctx 1d0)
    ;; §8.5.4: the clip is intersected AFTER the path is painted.
    (when (interp-pending-clip ip)
      (gesso:clip ctx (interp-pending-clip ip))
      (setf (interp-pending-clip ip) nil))
    (gesso:begin-path ctx)))

;;; Inline images (BI ... ID <bytes> EI) are parsed and painted by DO-INLINE-IMAGE
;;; in image.lisp (invoked from the main loop's BI branch).

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
               ((string= kw "BI") (ignore-errors (do-inline-image ip lx)) (setf ops '()))
               (t (ignore-errors (exec-op ip kw (nreverse ops)))
                  (setf ops '())))))
          (t (setf ops '())))))))

(defun exec-op (ip kw args)
  "Dispatch operator KW with its (in-order) operand list ARGS."
  (let ((ctx (interp-ctx ip)) (s (interp-state ip)))
    (macrolet ((a (i) `(nth ,i args)))
      (cond
        ;; ---- uncolored (PaintType 2) tiling-pattern cell: colour operators are
        ;; ignored; the cell is a stencil painted in the scn-supplied colour ----
        ((and (interp-uncolored-color ip)
              (member kw '("g" "G" "rg" "RG" "k" "K" "cs" "CS" "sc" "scn" "SC" "SCN")
                      :test #'string=))
         nil)

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
        ((string= kw "gs") (op-gs ip (a 0)))  ; ExtGState — constant alpha (§11.6.4.4)

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

        ;; ---- XObjects (§8.10) ----
        ((string= kw "Do") (op-do ip (a 0)))   ; Form / Image XObjects
        ((string= kw "sh") (op-sh ip (a 0)))    ; shading fills (§8.7.4.5)

        ;; ---- marked content / compatibility — no-op ----
        ((member kw '("BMC" "BDC" "EMC" "BX" "EX" "MP" "DP") :test #'string=) nil)

        ;; ---- text objects + state (§9.4, §9.3) ----
        ((string= kw "BT") (setf (interp-in-text ip) t
                                 (interp-tm ip) (gesso:mat-identity)
                                 (interp-tlm ip) (gesso:mat-identity)))
        ((string= kw "ET") (setf (interp-in-text ip) nil))
        ((string= kw "Tc") (setf (gstate2-char-spacing s) (num (a 0))))
        ((string= kw "Tw") (setf (gstate2-word-spacing s) (num (a 0))))
        ((string= kw "Tz") (setf (gstate2-h-scale s) (/ (num (a 0)) 100d0)))
        ((string= kw "TL") (setf (gstate2-leading s) (num (a 0))))
        ((string= kw "Ts") (setf (gstate2-rise s) (num (a 0))))
        ((string= kw "Tr") (setf (gstate2-render-mode s) (round (num (a 0)))))
        ((string= kw "Tf") (op-set-font ip (a 0) (num (a 1))))
        ;; ---- text positioning (§9.4.2) ----
        ((string= kw "Td") (op-text-move ip (num (a 0)) (num (a 1))))
        ((string= kw "TD") (setf (gstate2-leading s) (- (num (a 1))))
         (op-text-move ip (num (a 0)) (num (a 1))))
        ((string= kw "Tm")
         (let ((m (list (num (a 0)) (num (a 1)) (num (a 2))
                        (num (a 3)) (num (a 4)) (num (a 5)))))
           (setf (interp-tm ip) m (interp-tlm ip) (copy-list m))))
        ((string= kw "T*") (op-text-move ip 0d0 (- (gstate2-leading s))))
        ;; ---- text showing (§9.4.3) ----
        ((string= kw "Tj") (when (pdf-string-p (a 0)) (show-text ip (pdf-string-bytes (a 0)))))
        ((string= kw "TJ") (op-show-tj ip (a 0)))
        ((string= kw "'")
         (op-text-move ip 0d0 (- (gstate2-leading s)))
         (when (pdf-string-p (a 0)) (show-text ip (pdf-string-bytes (a 0)))))
        ((string= kw "\"")
         (setf (gstate2-word-spacing s) (num (a 0))
               (gstate2-char-spacing s) (num (a 1)))
         (op-text-move ip 0d0 (- (gstate2-leading s)))
         (when (pdf-string-p (a 2)) (show-text ip (pdf-string-bytes (a 2)))))
        ((or (string= kw "d0") (string= kw "d1")) nil) ; Type3 glyph metrics

        ;; ---- unknown: operands already discarded by the caller ----
        (t nil)))))

;;; Blend-mode names (§11.3.5) -> gesso keywords: the 12 separable modes plus the
;;; four NON-separable modes (Hue/Saturation/Color/Luminosity, §11.3.5.3), which
;;; gesso composites via the Lum/SetLum/Sat/SetSat colour machinery.
(defparameter +blend-mode-map+
  '(("Normal" . :normal) ("Compatible" . :normal)
    ("Multiply" . :multiply) ("Screen" . :screen) ("Overlay" . :overlay)
    ("Darken" . :darken) ("Lighten" . :lighten)
    ("ColorDodge" . :color-dodge) ("ColorBurn" . :color-burn)
    ("HardLight" . :hard-light) ("SoftLight" . :soft-light)
    ("Difference" . :difference) ("Exclusion" . :exclusion)
    ;; non-separable (§11.3.5.3)
    ("Hue" . :hue) ("Saturation" . :saturation) ("Color" . :color)
    ("Luminosity" . :luminosity)))

(defun parse-blend-mode (obj)
  "Map a /BM operand (a name, or an array of names — the first the reader supports)
to a gesso blend-mode keyword.  Unknown / absent -> :normal."
  (let ((name (cond ((pdf-name-p obj) (pdf-name-string obj))
                    ((pdf-array-p obj)
                     (loop for x across obj
                           for n = (and (pdf-name-p x) (pdf-name-string x))
                           when (and n (assoc n +blend-mode-map+ :test #'string=))
                             return n))
                    (t nil))))
    (or (cdr (assoc (or name "Normal") +blend-mode-map+ :test #'string=)) :normal)))

(defun op-gs (ip name-obj)
  "Apply a named graphics-state parameter dict (§8.4.5) from /Resources /ExtGState:
the constant alphas /ca (fill) and /CA (stroke), the blend mode /BM (§11.3.5) and
the soft mask /SMask (§11.6.5.2).  Transfer functions are still deferred."
  (let* ((doc (interp-doc ip)) (s (interp-state ip)) (ctx (interp-ctx ip))
         (name (and (pdf-name-p name-obj) (pdf-name-string name-obj)))
         (egs (and name (interp-resources ip)
                   (resolve doc (dict-get (interp-resources ip) "ExtGState"))))
         (g (and (hash-table-p egs) (resolve doc (dict-get egs name)))))
    (when (hash-table-p g)
      (let ((fill-a (resolve doc (dict-get g "ca")))
            (stroke-a (resolve doc (dict-get g "CA"))))
        (when (numberp fill-a) (setf (gstate2-fill-alpha s) (max 0d0 (min 1d0 (num fill-a)))))
        (when (numberp stroke-a) (setf (gstate2-stroke-alpha s) (max 0d0 (min 1d0 (num stroke-a))))))
      ;; ---- blend mode /BM ----
      (let ((bm (resolve doc (dict-get g "BM"))))
        (when bm (gesso:set-blend-mode ctx (parse-blend-mode bm))))
      ;; ---- soft mask /SMask (a mask dict, or /None to clear) ----
      (let ((sm (resolve doc (dict-get g "SMask"))))
        (cond
          ((null sm) nil)                                  ; entry absent: unchanged
          ((and (pdf-name-p sm) (string= (pdf-name-string sm) "None"))
           (gesso:set-soft-mask ctx nil))                  ; /None clears the mask
          ((hash-table-p sm)
           (gesso:set-soft-mask ctx (ignore-errors (build-soft-mask ip sm)))))))))

(defun op-set-space (ip name-obj which)
  "Handle cs/CS: select a colour space and reset the colour to its initial value."
  (let ((name (if (pdf-name-p name-obj) (pdf-name-string name-obj) "DeviceGray"))
        (s (interp-state ip)) (ctx (interp-ctx ip)))
    (multiple-value-bind (kind base) (resolve-color-space ip name)
      (if (eq which :fill)
          (setf (gstate2-fill-space s) kind (gstate2-fill-base s) base
                (gstate2-fill-pattern s) nil)
          (setf (gstate2-stroke-space s) kind (gstate2-stroke-base s) base
                (gstate2-stroke-pattern s) nil))
      ;; initial colour: black (0 for additive, full ink for subtractive)
      (let ((c (case kind (:cmyk '(0 0 0)) (t '(0 0 0)))))
        (if (eq which :fill) (gesso:set-fill ctx c) (gesso:set-stroke ctx c))))))

(defun op-set-color (ip args which)
  "Handle sc/scn/SC/SCN under the current colour space.  Under a Pattern space, a
trailing name operand selects a pattern: a shading pattern is painted as a gradient
when the path is filled (do-paint), so here we just record it and set a
representative solid colour as a fallback (used for strokes / tiling patterns)."
  (let* ((s (interp-state ip)) (ctx (interp-ctx ip))
         (kind (if (eq which :fill) (gstate2-fill-space s) (gstate2-stroke-space s)))
         (base (if (eq which :fill) (gstate2-fill-base s) (gstate2-stroke-base s)))
         (nums (remove-if-not #'numberp args))
         (name-arg (find-if #'pdf-name-p args)))
    (cond
      ((and (eq kind :pattern) name-arg)
       (let* ((pat (resolve-pattern ip (pdf-name-string name-arg)))
              ;; an uncolored (PaintType 2) tiling pattern carries its stencil colour
              ;; as the numeric operands before the name, interpreted in the Pattern
              ;; space's base colour space (stored in *-base at cs/CS time).
              (ucolor (and nums (uncolored-pattern-p (interp-doc ip) pat)
                           (ignore-errors (pattern-uncolored-rgb ip base nums))))
              (c (or ucolor
                     (ignore-errors (shading-pattern-average ip pat))
                     '(128 128 128))))
         (if (eq which :fill)
             (setf (gstate2-fill-pattern s) pat (gstate2-fill-pattern-color s) ucolor)
             (setf (gstate2-stroke-pattern s) pat (gstate2-stroke-pattern-color s) ucolor))
         (if (eq which :fill) (gesso:set-fill ctx c) (gesso:set-stroke ctx c))))
      (t
       (if (eq which :fill)
           (setf (gstate2-fill-pattern s) nil (gstate2-fill-pattern-color s) nil)
           (setf (gstate2-stroke-pattern s) nil (gstate2-stroke-pattern-color s) nil))
       (let ((c (if (eq kind :pattern) '(128 128 128) (interpret-color kind base nums))))
         (if (eq which :fill) (gesso:set-fill ctx c) (gesso:set-stroke ctx c)))))))

;;; ---- text (§9): font selection, positioning, glyph painting ---------------

(defun op-set-font (ip name-obj size)
  "Tf: select the font named NAME-OBJ from /Resources /Font and set its size."
  (let* ((s (interp-state ip))
         (name (and (pdf-name-p name-obj) (pdf-name-string name-obj))))
    (setf (gstate2-font-size s) (num size))
    (when name
      (let* ((doc (interp-doc ip))
             (fonts (and (interp-resources ip)
                         (resolve doc (dict-get (interp-resources ip) "Font"))))
             (fref (and (hash-table-p fonts) (dict-get fonts name)))
             (fdict (and fref (resolve doc fref))))
        (when (hash-table-p fdict)
          (setf (gstate2-font s)
                (or (gethash fdict (interp-font-cache ip))
                    (setf (gethash fdict (interp-font-cache ip))
                          (load-font doc fdict)))))))))

(defun op-text-move (ip tx ty)
  "Td: Tlm <- translate(tx,ty) x Tlm ; Tm <- Tlm."
  (let ((new (gesso:mat-mul (interp-tlm ip) (gesso:mat-translate tx ty))))
    (setf (interp-tlm ip) new (interp-tm ip) (copy-list new))))

(defun op-show-tj (ip items)
  "TJ: show strings, with numeric elements adjusting the pen (kerning).  A number
n moves the pen left by n/1000 * Tfs * Th of text space."
  (let ((s (interp-state ip)))
    (when (or (listp items) (pdf-array-p items))
      (map nil
           (lambda (el)
             (cond
               ((pdf-string-p el) (show-text ip (pdf-string-bytes el)))
               ((numberp el)
                (let ((tx (* (* (/ (- (float el 1d0)) 1000d0) (gstate2-font-size s))
                             (gstate2-h-scale s))))
                  (setf (interp-tm ip)
                        (gesso:mat-mul (interp-tm ip) (gesso:mat-translate tx 0d0)))))))
           items))))

(defun glyph-render-matrix (ip font)
  "The glyph-space (scribe font units) -> user-space matrix: scale by 1/upem into
text space, apply the text-space parameters [Tfs*Th 0 0 Tfs 0 Trise], then Tm.
gesso then maps user space through the CTM to device pixels."
  (let* ((s (interp-state ip))
         (fs (gstate2-font-size s))
         (param (list (* fs (gstate2-h-scale s)) 0d0 0d0 fs 0d0 (gstate2-rise s)))
         (upem (max 1d0 (pdf-font-upem font)))
         (scale (gesso:mat-scale (/ 1d0 upem) (/ 1d0 upem))))
    (gesso:mat-mul (gesso:mat-mul (interp-tm ip) param) scale)))

(defun paint-glyph (ip font gid)
  "Fill/stroke GID's outline in the current text render mode.  Mode 3 (and clip-
only mode 7) paint nothing — this is what keeps a scanned page's invisible OCR
text layer from garbaging the image."
  (let* ((s (interp-state ip)) (ctx (interp-ctx ip))
         (mode (gstate2-render-mode s)))
    (when (member mode '(3 7)) (return-from paint-glyph)) ; invisible / clip-only
    (when (zerop gid) (return-from paint-glyph))           ; .notdef: paint nothing
    (let ((contours (font-glyph-contours font gid)))
      (when contours
        (let ((g (glyph-render-matrix ip font)))
          (gesso:begin-path ctx)
          (dolist (c contours)
            (dolist (seg c)
              (ecase (car seg)
                (:move (multiple-value-bind (x y) (gesso:mat-apply g (second seg) (third seg))
                         (gesso:move-to ctx x y)))
                (:line (multiple-value-bind (x y) (gesso:mat-apply g (second seg) (third seg))
                         (gesso:line-to ctx x y)))
                (:quad (multiple-value-bind (cx cy) (gesso:mat-apply g (second seg) (third seg))
                         (multiple-value-bind (x y) (gesso:mat-apply g (fourth seg) (fifth seg))
                           (gesso:quadratic-curve-to ctx cx cy x y))))
                (:cubic (multiple-value-bind (c1x c1y) (gesso:mat-apply g (second seg) (third seg))
                          (multiple-value-bind (c2x c2y) (gesso:mat-apply g (fourth seg) (fifth seg))
                            (multiple-value-bind (x y) (gesso:mat-apply g (sixth seg) (nth 6 seg))
                              (gesso:bezier-curve-to ctx c1x c1y c2x c2y x y))))))))
          (gesso:close-path ctx)
          (when (member mode '(0 2 4 6))        ; fill variants
            (gesso:set-global-alpha ctx (gstate2-fill-alpha s))
            (gesso:set-fill-rule ctx :nonzero)
            (gesso:fill-path ctx)
            ;; synthetic bold: stroke the filled outline in the fill colour with a
            ;; hairline width so a regular substitute face standing in for a bold
            ;; font renders with heavier weight (§ embedded outlines are left alone).
            (when (pdf-font-synth-bold font)
              (gesso:save ctx)
              (gesso:set-stroke ctx (gesso:current-fill ctx))
              (gesso:set-line-width ctx (synth-bold-width ip))
              (gesso:stroke-path ctx)
              (gesso:restore ctx)))
          (when (member mode '(1 2 5 6))        ; stroke variants
            (gesso:set-global-alpha ctx (gstate2-stroke-alpha s))
            (apply-line-width ip)
            (gesso:stroke-path ctx))
          (gesso:set-global-alpha ctx 1d0)
          (gesso:begin-path ctx))))))

(defun font-drawable-p (font)
  "A font has glyph outlines we can paint (an sfnt face or an embedded CFF)."
  (and font (not (pdf-font-type3 font))
       (or (pdf-font-face font) (pdf-font-cff font) (pdf-font-type1 font))))

(defun show-text-composite (ip bytes)
  "Show a Type0 string: decode multi-byte codes to CIDs via the font's CMap, paint
each glyph (CID -> GID), and advance by the per-CID /W width."
  (let* ((s (interp-state ip)) (font (gstate2-font s))
         (cm (pdf-font-cmap font))
         (fs (gstate2-font-size s)) (tc (gstate2-char-spacing s))
         (tw (gstate2-word-spacing s)) (th (gstate2-h-scale s))
         (i 0) (n (length bytes)))
    (loop while (< i n) do
      (multiple-value-bind (code nb) (cmap-next-code cm bytes i)
        (incf i nb)
        (let* ((cid (cmap-code->cid cm code))
               (gid (cid->gid font cid))
               (w0 (/ (cid-width font cid) 1000d0)))
          (paint-glyph ip font gid)
          ;; word spacing applies only to a single-byte code 32 (§9.3.3)
          (let ((tx (* (+ (* w0 fs) tc (if (and (= nb 1) (= code 32)) tw 0d0)) th)))
            (setf (interp-tm ip)
                  (gesso:mat-mul (interp-tm ip) (gesso:mat-translate tx 0d0)))))))))

(defun show-text (ip bytes)
  "Show a byte string: for each code, paint its glyph at the text render matrix
and advance Tm by (w0*Tfs + Tc + Tw?)*Th in text space."
  (let* ((s (interp-state ip)) (font (gstate2-font s)))
    (when (and font (pdf-font-composite font))
      (if (font-drawable-p font)
          (return-from show-text (show-text-composite ip bytes))
          (return-from show-text (advance-only-composite ip font bytes))))
    (unless (and font (font-drawable-p font))
      ;; no drawable face (e.g. Type3 or a failed load): still advance the pen so
      ;; following text lands correctly, using widths if present.
      (when (and font (plusp (length bytes)))
        (advance-only ip font bytes))
      (return-from show-text))
    (let ((fs (gstate2-font-size s))
          (tc (gstate2-char-spacing s))
          (tw (gstate2-word-spacing s))
          (th (gstate2-h-scale s)))
      (loop for code across bytes do
        (let* ((gid (font-code->gid font code))
               (w0 (/ (font-code-width font code) 1000d0)))
          (paint-glyph ip font gid)
          (let ((tx (* (+ (* w0 fs) tc (if (= code 32) tw 0d0)) th)))
            (setf (interp-tm ip)
                  (gesso:mat-mul (interp-tm ip) (gesso:mat-translate tx 0d0)))))))))

(defun advance-only (ip font bytes)
  "Advance Tm across BYTES without painting (font has no drawable face)."
  (let* ((s (interp-state ip)) (fs (gstate2-font-size s))
         (tc (gstate2-char-spacing s)) (tw (gstate2-word-spacing s))
         (th (gstate2-h-scale s)))
    (loop for code across bytes do
      (let* ((w0 (/ (font-code-width font code) 1000d0))
             (tx (* (+ (* w0 fs) tc (if (= code 32) tw 0d0)) th)))
        (setf (interp-tm ip)
              (gesso:mat-mul (interp-tm ip) (gesso:mat-translate tx 0d0)))))))

(defun advance-only-composite (ip font bytes)
  "Advance Tm across a Type0 string without painting (no drawable descendant)."
  (let* ((s (interp-state ip)) (cm (pdf-font-cmap font))
         (fs (gstate2-font-size s)) (tc (gstate2-char-spacing s))
         (th (gstate2-h-scale s)) (i 0) (n (length bytes)))
    (loop while (< i n) do
      (multiple-value-bind (code nb) (cmap-next-code cm bytes i)
        (declare (ignore code))
        (incf i nb)
        (let* ((cid (cmap-code->cid cm (bytes->uint bytes (- i nb) i)))
               (tx (* (+ (* (/ (cid-width font cid) 1000d0) fs) tc) th)))
          (setf (interp-tm ip)
                (gesso:mat-mul (interp-tm ip) (gesso:mat-translate tx 0d0))))))))
