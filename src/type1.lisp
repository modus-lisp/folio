;;;; src/type1.lisp — embedded Adobe Type 1 (FontFile / eexec) font programs.
;;;;
;;;; A /FontDescriptor /FontFile stream is a Type 1 font program: a clear-text
;;;; PostScript header (FontMatrix / Encoding / FontInfo) followed by an
;;;; eexec-encrypted binary Private section carrying the /Subrs and the
;;;; name-keyed /CharStrings.  Each charstring is a Type 1 charstring (Adobe
;;;; "Type 1 Font Format", the Black Book) — a DIFFERENT bytecode from the Type 2
;;;; charstrings cff.lisp interprets (single-segment h/v line/curve ops, hsbw
;;;; sidebearing, seac accent composition, and flex/hint-replacement expressed
;;;; through callothersubr).  This module decrypts the program and interprets a
;;;; charstring into the SAME outline vocabulary the glyph painter already draws:
;;;;   (:move x y) (:line x y) (:cubic c1x c1y c2x c2y x y)
;;;; so an embedded Type 1 subset renders its OWN exact glyphs (its builtin
;;;; encoding intact) instead of a metric-only substitute face.

(in-package #:folio)

;;; ---- byte helpers over an (unsigned-byte 8) vector -------------------------
(declaim (inline t1-ws-p t1-delim-p))
(defun t1-ws-p (b) (member b '(0 9 10 12 13 32)))
(defun t1-delim-p (b) (or (t1-ws-p b) (member b '(40 41 60 62 91 93 123 125 47)))) ; ()<>[]{}/

(defun t1-hexdigit (b)
  (cond ((<= 48 b 57) (- b 48)) ((<= 65 b 70) (+ 10 (- b 65)))
        ((<= 97 b 102) (+ 10 (- b 97))) (t nil)))

;;; ---- eexec / charstring decryption (Type 1 spec §7) ------------------------
(defun t1-decrypt (data start end r skip)
  "Decrypt DATA[START,END) with the Type 1 eexec cipher seeded at R (c1=52845,
c2=22719), dropping the first SKIP plaintext bytes.  Returns a fresh octet vector."
  (let* ((c1 52845) (c2 22719) (r r)
         (n (max 0 (- end start)))
         (out (make-array (max 0 (- n skip)) :element-type '(unsigned-byte 8)))
         (j 0))
    (loop for i from start below end
          for c = (aref data i)
          for p = (logxor c (ash r -8)) do
            (setf r (logand (* (+ c r) c1) #xffff))
            (setf r (logand (+ r c2) #xffff))
            (when (>= (- i start) skip)
              (when (< j (length out)) (setf (aref out j) (logand p #xff)))
              (incf j)))
    out))

;;; ---- PFB de-segmentation ---------------------------------------------------
(defun t1-depfb (bytes)
  "If BYTES is a PFB-wrapped Type 1 program (0x80 segment markers) concatenate the
ascii+binary segments back into a raw program; otherwise return BYTES unchanged.
Returns (values raw-bytes ascii-len binary-len) — the segment lengths let the
caller split clear vs eexec exactly."
  (if (and (>= (length bytes) 6) (= (aref bytes 0) #x80))
      (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0))
            (p 0) (alen 0) (blen 0))
        (loop while (and (< (+ p 6) (length bytes)) (= (aref bytes p) #x80)) do
          (let ((type (aref bytes (1+ p))))
            (when (= type 3) (return))       ; EOF marker
            (let ((len (logior (aref bytes (+ p 2)) (ash (aref bytes (+ p 3)) 8)
                               (ash (aref bytes (+ p 4)) 16) (ash (aref bytes (+ p 5)) 24))))
              (incf p 6)
              (loop for i from 0 below len while (< (+ p i) (length bytes))
                    do (vector-push-extend (aref bytes (+ p i)) out))
              (if (= type 1) (incf alen len) (incf blen len))
              (incf p len))))
        (values (coerce out '(simple-array (unsigned-byte 8) (*))) alen blen))
      (values bytes nil nil)))

;;; ---- search a subvector for an ASCII marker --------------------------------
(defun t1-search (data pat &optional (start 0))
  "Index of ASCII string PAT in DATA at or after START, or NIL."
  (let ((plen (length pat)) (dlen (length data)))
    (loop for i from start to (- dlen plen) do
      (when (loop for k from 0 below plen
                  always (= (aref data (+ i k)) (char-code (char pat k))))
        (return i)))))

;;; ---- tiny byte tokenizer for the (clear or decrypted) PostScript text ------
;;; Reads whitespace-delimited tokens; leaves the caller positioned so it can
;;; grab an exact binary run after an RD / -| operator.
(defstruct (t1lex (:constructor make-t1lex (data pos end))) data pos end)

(defun t1lex-skip-ws (lx)
  (loop while (and (< (t1lex-pos lx) (t1lex-end lx))
                   (t1-ws-p (aref (t1lex-data lx) (t1lex-pos lx))))
        do (incf (t1lex-pos lx))))

(defun t1lex-token (lx)
  "Next whitespace/delimiter-delimited token as a string, or NIL at end.  A lone
delimiter char ( ) < > [ ] { } / is returned as its own one-char token; leaves
POS just past the token."
  (t1lex-skip-ws lx)
  (let ((d (t1lex-data lx)) (p (t1lex-pos lx)) (e (t1lex-end lx)))
    (when (>= p e) (return-from t1lex-token nil))
    (let ((b (aref d p)))
      (cond
        ((= b 47)                            ; '/' literal name: /Name
         (let ((s (1+ p)))
           (loop while (and (< s e) (not (t1-delim-p (aref d s)))) do (incf s))
           (setf (t1lex-pos lx) s)
           (concatenate 'string "/" (t1-bytes->string d (1+ p) s))))
        ((t1-delim-p b)                      ; other single delimiter
         (setf (t1lex-pos lx) (1+ p))
         (string (code-char b)))
        (t (let ((s p))
             (loop while (and (< s e) (not (t1-delim-p (aref d s)))) do (incf s))
             (setf (t1lex-pos lx) s)
             (t1-bytes->string d p s)))))))

(defun t1-bytes->string (d start end)
  (let ((s (make-string (- end start))))
    (dotimes (i (- end start)) (setf (char s i) (code-char (aref d (+ start i)))))
    s))

(defun t1-parse-int (str) (ignore-errors (parse-integer str :junk-allowed t)))

;;; ---- parsed Type 1 font ----------------------------------------------------
(defstruct t1
  (charstrings (make-hash-table :test 'equal))  ; glyph name -> decrypted charstring
  (subrs #())                                    ; index -> decrypted subr charstring
  (encoding nil)                                 ; 256-vec code->name (builtin), or NIL
  (font-matrix nil)                              ; list of 6, or NIL (default .001)
  (names #())                                    ; gid -> glyph name (ordered)
  (name->gid (make-hash-table :test 'equal))     ; glyph name -> gid
  (leniv 4))

(defun t1-upem (t1)
  (let ((m (t1-font-matrix t1)))
    (if (and m (numberp (first m)) (not (zerop (first m))))
        (float (/ 1d0 (abs (first m))) 1d0)
        1000d0)))

(defun t1-name-gid (t1 name) (and name (gethash name (t1-name->gid t1))))

;;; ---- parse the clear-text header: /FontMatrix + builtin /Encoding ----------
(defun t1-parse-clear (data start end)
  "Read the FontMatrix (or NIL) and builtin Encoding (256-vec code->name, or NIL
for StandardEncoding) from the clear-text portion DATA[START,END)."
  (let ((fmatrix nil) (encoding nil))
    ;; FontMatrix [a b c d e f]
    (let ((mi (t1-search data "/FontMatrix" start)))
      (when mi
        (let* ((lb (t1-search data "[" mi))
               (rb (and lb (t1-search data "]" lb))))
          (when (and lb rb (< rb end))
            (let ((lx (make-t1lex data (1+ lb) rb)) (nums '()))
              (loop for tok = (t1lex-token lx) while tok do
                (let ((v (let ((*read-default-float-format* 'double-float))
                           (ignore-errors (read-from-string tok)))))
                  (when (numberp v) (push (float v 1d0) nums))))
              (when (= (length nums) 6) (setf fmatrix (nreverse nums))))))))
    ;; Encoding: StandardEncoding, or a table of `dup <code> /<name> put`
    (let ((ei (t1-search data "/Encoding" start)))
      (when ei
        (let ((std (t1-search data "StandardEncoding" ei)))
          (if (and std (< (- std ei) 40))
              (setf encoding nil)             ; StandardEncoding
              (let ((vec (make-array 256 :initial-element nil))
                    (lx (make-t1lex data (+ ei 9) end)) (any nil))
                (loop for tok = (t1lex-token lx)
                      while (and tok (not (string= tok "readonly"))
                                 (not (string= tok "def"))) do
                  (when (string= tok "dup")
                    (let* ((code (t1-parse-int (or (t1lex-token lx) "")))
                           (nm (t1lex-token lx))
                           (put (t1lex-token lx)))
                      (when (and (integerp code) (<= 0 code 255)
                                 nm (> (length nm) 1) (char= (char nm 0) #\/)
                                 put (string= put "put"))
                        (setf (aref vec code) (subseq nm 1) any t)))))
                (when any (setf encoding vec)))))))
    (values fmatrix encoding)))

;;; ---- parse the decrypted Private section: lenIV, Subrs, CharStrings --------
(defun t1-parse-private (data)
  "Parse the decrypted Private section (octet vector DATA) -> a T1 with its Subrs
vector and CharStrings hash populated (each entry decrypted with R=4330, lenIV
skipped)."
  (let* ((t1 (make-t1))
         (leniv (let ((li (t1-search data "/lenIV")))
                  (or (and li (t1-parse-int
                               (let ((lx (make-t1lex data (+ li 6) (length data))))
                                 (or (t1lex-token lx) ""))))
                      4))))
    (setf (t1-leniv t1) leniv)
    (labels ((read-cs (lx len)
               ;; LX is just past the RD/-| operator; consume ONE space then LEN bytes
               (let ((p (t1lex-pos lx)))
                 (when (and (< p (t1lex-end lx)) (t1-ws-p (aref data p))) (incf p))
                 (let ((raw-start p) (raw-end (min (t1lex-end lx) (+ p len))))
                   (setf (t1lex-pos lx) raw-end)
                   (t1-decrypt data raw-start raw-end 4330 leniv)))))
      ;; ---- Subrs ----
      (let ((si (t1-search data "/Subrs")))
        (when si
          (let* ((lx (make-t1lex data (+ si 6) (length data)))
                 (count (t1-parse-int (or (t1lex-token lx) "")))
                 (subrs (make-array (if (and (integerp count) (< count 100000))
                                        (max 1 count) 1)
                                    :initial-element #()))
                 (cs-start (t1-search data "/CharStrings")))
            ;; scan `dup <i> <len> RD <bin> NP` up to /CharStrings (or EOF)
            (let ((limit (or cs-start (length data))))
              (loop
                (let ((dup (t1-search data "dup" (t1lex-pos lx))))
                  (when (or (not dup) (>= dup limit)) (return))
                  (setf (t1lex-pos lx) (+ dup 3))
                  (let* ((idx (t1-parse-int (or (t1lex-token lx) "")))
                         (len (t1-parse-int (or (t1lex-token lx) "")))
                         (rd  (t1lex-token lx)))
                    (if (and (integerp idx) (integerp len) (>= idx 0) (>= len 0)
                             rd (or (string= rd "RD") (string= rd "-|")))
                        (let ((cs (read-cs lx len)))
                          (when (>= idx (length subrs))
                            (let ((n (make-array (1+ idx) :initial-element #())))
                              (replace n subrs) (setf subrs n)))
                          (setf (aref subrs idx) cs))
                        (return))))))
            (setf (t1-subrs t1) subrs))))
      ;; ---- CharStrings: `/name <len> RD <bin> ND` ----
      (let ((ci (t1-search data "/CharStrings")))
        (when ci
          (let* ((begin (or (t1-search data "begin" ci) ci))
                 (lx (make-t1lex data (+ begin 5) (length data)))
                 (names '()))
            (loop
              (let ((tok (t1lex-token lx)))
                (when (null tok) (return))
                (when (and (string= tok "end") ) (return))
                (when (and (> (length tok) 1) (char= (char tok 0) #\/))
                  (let* ((name (subseq tok 1))
                         (len (t1-parse-int (or (t1lex-token lx) "")))
                         (rd  (t1lex-token lx)))
                    (when (and (integerp len) (>= len 0)
                               rd (or (string= rd "RD") (string= rd "-|")))
                      (let ((cs (read-cs lx len)))
                        (setf (gethash name (t1-charstrings t1)) cs)
                        (push name names)))))))
            ;; ordered gid table: .notdef first, then insertion order
            (let* ((names (nreverse names))
                   (ord (if (member ".notdef" names :test #'string=)
                            (cons ".notdef" (remove ".notdef" names :test #'string=))
                            (cons ".notdef" names)))
                   (vec (coerce ord 'vector)))
              (setf (t1-names t1) vec)
              (let ((h (t1-name->gid t1)))
                (dotimes (g (length vec)) (setf (gethash (aref vec g) h) g))))))))
    t1))

;;; ---- top-level: parse a raw FontFile program -------------------------------
(defun parse-type1 (bytes &optional length1 length2)
  "Parse a Type 1 font program (the decoded /FontFile stream body) into a T1.
LENGTH1/LENGTH2 (the stream's /Length1 /Length2) split clear vs eexec when
present; otherwise the `eexec` marker is located and the encryption type
(binary vs ASCII-hex) is auto-detected.  Returns a T1 or signals on failure."
  (multiple-value-bind (data alen blen) (t1-depfb bytes)
    (declare (ignore blen))
    (let* ((len1 (or alen (and (integerp length1) length1)))
           (eexec (t1-search data "eexec"))
           (clear-end (cond ((and len1 (<= len1 (length data))) len1)
                            (eexec eexec)
                            (t (length data)))))
      ;; --- clear header ---
      (multiple-value-bind (fmatrix encoding) (t1-parse-clear data 0 clear-end)
        ;; --- locate the eexec ciphertext ---
        (let ((p (cond (eexec (+ eexec 5)) (len1 len1) (t 0))))
          ;; skip whitespace after `eexec`
          (loop while (and (< p (length data)) (t1-ws-p (aref data p))) do (incf p))
          ;; ASCII-hex vs binary: hex if the first 4 non-ws bytes are hex digits
          (let* ((hex (let ((cnt 0) (q p))
                        (loop while (and (< q (length data)) (< cnt 4)) do
                          (let ((b (aref data q)))
                            (cond ((t1-ws-p b) (incf q))
                                  ((t1-hexdigit b) (incf cnt) (incf q))
                                  (t (return)))))
                        (= cnt 4)))
                 (cipher-end (cond ((and (integerp length2) (<= (+ p length2) (length data)))
                                    (+ p length2))
                                   (t (length data))))
                 (cipher (if hex
                             (t1-hexdecode data p cipher-end)
                             (subseq data p cipher-end)))
                 (priv (t1-decrypt cipher 0 (length cipher) 55665 4)))
            (let ((t1 (t1-parse-private priv)))
              (setf (t1-font-matrix t1) fmatrix)
              (setf (t1-encoding t1) encoding)
              (when (zerop (hash-table-count (t1-charstrings t1)))
                (error "type1: no charstrings parsed"))
              t1)))))))

(defun t1-hexdecode (data start end)
  "Decode the ASCII-hex run DATA[START,END) into an octet vector (whitespace ok)."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (hi nil))
    (loop for i from start below end
          for h = (t1-hexdigit (aref data i)) do
      (when h
        (if hi (progn (vector-push-extend (logior (ash hi 4) h) out) (setf hi nil))
            (setf hi h))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; ---- Type 1 charstring interpreter -> outline segments ---------------------
(defun t1-glyph-outline (t1 name &optional (depth 0))
  "Interpret the Type 1 charstring named NAME into a list of contours, each a
segment list (:move/:line/:cubic) in font units.  Handles hsbw/sbw, the single-
segment line/curve ops, seac accent composition, and flex + hint replacement
expressed through callothersubr/pop."
  (let ((cs (gethash name (t1-charstrings t1))))
    (when (or (null cs) (> depth 4)) (return-from t1-glyph-outline nil))
    (let* ((subrs (t1-subrs t1))
           (st (make-array 32 :adjustable t :fill-pointer 0))
           (ps '())                          ; PostScript stack for callothersubr/pop
           (x 0d0) (y 0d0) (sbx 0d0)
           (contours '()) (cur '()) (open nil) (done nil)
           (flex nil) (flex-pts '())         ; flex accumulation
           (seac-result nil))
      (labels ((push* (v) (vector-push-extend (float v 1d0) st))
               (clear () (setf (fill-pointer st) 0))
               (nargs () (fill-pointer st))
               (moveto (nx ny)
                 (when open (push (nreverse cur) contours))
                 (setf x nx y ny cur (list (list :move x y)) open t))
               (lineto (nx ny) (push (list :line nx ny) cur) (setf x nx y ny))
               (curveto (c1x c1y c2x c2y nx ny)
                 (push (list :cubic c1x c1y c2x c2y nx ny) cur) (setf x nx y ny))
               (rmove (dx dy)
                 (let ((nx (+ x dx)) (ny (+ y dy)))
                   (if flex
                       (progn (push (cons nx ny) flex-pts) (setf x nx y ny))
                       (moveto nx ny))))
               (do-othersubr (idx args)
                 (case idx
                   (1 (setf flex t flex-pts '()))     ; begin flex
                   (2 nil)                              ; collect flex point (marker)
                   (0                                   ; end flex
                    (setf flex nil)
                    (let ((pts (nreverse flex-pts)))
                      (when (>= (length pts) 7)
                        (let ((p (coerce pts 'vector)))
                          ;; pts[0] reference; two curves from pts[1..6]
                          (flet ((px (i) (car (aref p i))) (py (i) (cdr (aref p i))))
                            (curveto (px 1) (py 1) (px 2) (py 2) (px 3) (py 3))
                            (curveto (px 4) (py 4) (px 5) (py 5) (px 6) (py 6))))))
                    ;; return endpoint for the following `pop pop setcurrentpoint`
                    (let ((ex (if (>= (length args) 3) (elt args 1) x))
                          (ey (if (>= (length args) 3) (elt args 2) y)))
                      (setf ps (list ex ey))))
                   (3 (setf ps (list (if args (elt args 0) 3)))) ; hint replacement
                   (t (setf ps (reverse (coerce args 'list))))))
               (callothersubr ()
                 (let* ((oth (round (vector-pop st)))
                        (nn (round (vector-pop st)))
                        (args (make-array (max 0 nn))))
                   (loop for k from (1- nn) downto 0 do
                     (setf (aref args k) (if (> (nargs) 0) (vector-pop st) 0d0)))
                   (do-othersubr oth args)))
               (do-seac (asb adx ady bchar achar)
                 (let* ((bname (and (< bchar 256) (aref +standard-names+ bchar)))
                        (aname (and (< achar 256) (aref +standard-names+ achar)))
                        (base (and bname (t1-glyph-outline t1 bname (1+ depth))))
                        (acc  (and aname (t1-glyph-outline t1 aname (1+ depth))))
                        (dx (+ (- sbx asb) adx)) (dy ady))
                   (setf seac-result
                         (append base (t1-translate-contours acc dx dy)))))
               (run (code)
                 (let ((p 0) (e (length code)))
                   (loop while (and (< p e) (not done)) do
                     (let ((b (aref code p)))
                       (cond
                         ((>= b 32)
                          (cond ((<= b 246) (push* (- b 139)) (incf p))
                                ((<= b 250) (push* (+ (* (- b 247) 256) (aref code (1+ p)) 108)) (incf p 2))
                                ((<= b 254) (push* (- (- (* (- b 251) 256)) (aref code (1+ p)) 108)) (incf p 2))
                                (t (let ((v (logior (ash (aref code (+ p 1)) 24) (ash (aref code (+ p 2)) 16)
                                                    (ash (aref code (+ p 3)) 8) (aref code (+ p 4)))))
                                     (push* (if (>= v #x80000000) (- v #x100000000) v)) (incf p 5)))))
                         (t (incf p)
                            (case b
                              (13 (setf sbx (aref st 0) x (aref st 0) y 0d0) (clear)) ; hsbw
                              (9  (clear))                                   ; closepath
                              (21 (rmove (aref st 0) (aref st 1)) (clear))   ; rmoveto
                              (22 (rmove (aref st 0) 0d0) (clear))           ; hmoveto
                              (4  (rmove 0d0 (aref st 0)) (clear))           ; vmoveto
                              (5  (lineto (+ x (aref st 0)) (+ y (aref st 1))) (clear)) ; rlineto
                              (6  (lineto (+ x (aref st 0)) y) (clear))      ; hlineto
                              (7  (lineto x (+ y (aref st 0))) (clear))      ; vlineto
                              (8  (let* ((c1x (+ x (aref st 0))) (c1y (+ y (aref st 1)))
                                         (c2x (+ c1x (aref st 2))) (c2y (+ c1y (aref st 3)))
                                         (ex (+ c2x (aref st 4))) (ey (+ c2y (aref st 5))))
                                    (curveto c1x c1y c2x c2y ex ey)) (clear)) ; rrcurveto
                              (30 (let* ((c1x x) (c1y (+ y (aref st 0)))      ; vhcurveto
                                         (c2x (+ c1x (aref st 1))) (c2y (+ c1y (aref st 2)))
                                         (ex (+ c2x (aref st 3))) (ey c2y))
                                    (curveto c1x c1y c2x c2y ex ey)) (clear))
                              (31 (let* ((c1x (+ x (aref st 0))) (c1y y)      ; hvcurveto
                                         (c2x (+ c1x (aref st 1))) (c2y (+ c1y (aref st 2)))
                                         (ex c2x) (ey (+ c2y (aref st 3))))
                                    (curveto c1x c1y c2x c2y ex ey)) (clear))
                              (1  (clear)) (3 (clear))                        ; hstem vstem
                              (10 (let* ((i (round (vector-pop st)))          ; callsubr
                                         (s (and (>= i 0) (< i (length subrs)) (aref subrs i))))
                                    (when s (run s))))
                              (11 (return-from run))                          ; return
                              (14 (when open (push (nreverse cur) contours) (setf open nil))
                                  (setf done t) (return-from run))            ; endchar
                              (12 (let ((op2 (aref code p))) (incf p)
                                    (case op2
                                      (0 (clear))                            ; dotsection
                                      (1 (clear)) (2 (clear))                ; vstem3 hstem3
                                      (6 (do-seac (aref st 0) (aref st 1) (aref st 2)
                                                  (round (aref st 3)) (round (aref st 4)))
                                         (setf done t) (return-from run))     ; seac
                                      (7 (setf sbx (aref st 0) x (aref st 0) y (aref st 1)) (clear)) ; sbw
                                      (12 (let ((bb (vector-pop st)) (aa (vector-pop st))) ; div
                                            (push* (if (zerop bb) 0d0 (/ aa bb)))))
                                      (16 (callothersubr))                    ; callothersubr
                                      (17 (push* (if ps (pop ps) 0d0)))       ; pop
                                      (33 (setf x (aref st 0) y (aref st 1)) (clear)) ; setcurrentpoint
                                      (t (clear)))))
                              (t (clear))))))))))
        (run cs)
        (when seac-result (return-from t1-glyph-outline seac-result))
        (when open (push (nreverse cur) contours))
        (nreverse contours)))))

(defun t1-translate-contours (contours dx dy)
  "Return CONTOURS with every point translated by (DX,DY)."
  (if (and (zerop dx) (zerop dy))
      contours
      (mapcar (lambda (c)
                (mapcar (lambda (seg)
                          (case (car seg)
                            ((:move :line) (list (car seg) (+ (second seg) dx) (+ (third seg) dy)))
                            (:cubic (list :cubic (+ (second seg) dx) (+ (third seg) dy)
                                          (+ (fourth seg) dx) (+ (fifth seg) dy)
                                          (+ (sixth seg) dx) (+ (nth 6 seg) dy)))
                            (t seg)))
                        c))
              contours)))
