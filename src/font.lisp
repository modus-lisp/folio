;;;; src/font.lisp — simple/standard font handling for the text machinery (§9.6).
;;;;
;;;; A PDF simple font maps a single-byte code to a glyph and an advance width.
;;;; We resolve each font dictionary to a PDF-FONT: a concrete scribe face to draw
;;;; through (an embedded TrueType program when present and loadable, otherwise a
;;;; metric-compatible substitute for one of the 14 standard fonts), a 256-entry
;;;; code -> glyph-name encoding table, and a code -> advance-width table (in PDF
;;;; glyph space, 1000 units/em).  Glyphs are located in the face by mapping the
;;;; code's glyph name (via the Adobe Glyph List) to a Unicode scalar and looking
;;;; it up in the face's cmap.

(in-package #:folio)

;;; ---- lookup tables built once from the generated data ----------------------
(defvar *agl*
  (let ((h (make-hash-table :test 'equal)))
    (dolist (pair +agl-data+ h) (setf (gethash (car pair) h) (cdr pair)))))

(defvar *afm-widths*
  (let ((h (make-hash-table :test 'equal)))
    (dolist (entry +afm-width-data+ h)
      (let ((sub (make-hash-table :test 'equal)))
        (dolist (nw (cdr entry)) (setf (gethash (car nw) sub) (cdr nw)))
        (setf (gethash (car entry) h) sub)))))

(defun base-encoding-vector (name)
  "The 256-entry code->glyph-name array for a base encoding NAME string."
  (cond ((string= name "WinAnsiEncoding") +winansi-names+)
        ((string= name "MacRomanEncoding") +macroman-names+)
        ((string= name "StandardEncoding") +standard-names+)
        ((string= name "Symbol") +symbol-names+)
        ((string= name "ZapfDingbats") +zapf-names+)
        (t +standard-names+)))

;;; ---- glyph name -> Unicode -------------------------------------------------
(defun glyph-name->unicode (name)
  "Map an Adobe glyph NAME to a Unicode code point, or NIL.  Handles the AGL
subset plus the algorithmic `uniXXXX` / `uXXXX..` forms and a trailing-dot
variant suffix (e.g. `a.sc`)."
  (when (and (stringp name) (plusp (length name)))
    (or (gethash name *agl*)
        (let ((dot (position #\. name)))
          (when (and dot (plusp dot)) (glyph-name->unicode (subseq name 0 dot))))
        (and (> (length name) 3) (string= (subseq name 0 3) "uni")
             (ignore-errors (parse-integer name :start 3 :end 7 :radix 16)))
        (and (> (length name) 1) (char= (char name 0) #\u)
             (every (lambda (c) (digit-char-p c 16)) (subseq name 1))
             (<= (length name) 7)
             (ignore-errors (parse-integer name :start 1 :radix 16))))))

;;; ---- BaseFont -> substitute face -------------------------------------------
(defun strip-subset-tag (basefont)
  "Drop a `ABCDEF+` subset prefix from a BaseFont name."
  (if (and (>= (length basefont) 7) (char= (char basefont 6) #\+)
           (every #'upper-case-p (subseq basefont 0 6)))
      (subseq basefont 7)
      basefont))

(defun standard-font-info (basefont)
  "From a BaseFont name return (values family-list weight style afm-key symbolic-p).
FAMILY-LIST/WEIGHT/STYLE drive scribe:match-font; AFM-KEY selects the standard
metric table used when /Widths is absent; SYMBOLIC-P marks Symbol/ZapfDingbats."
  (let* ((n (string-downcase (strip-subset-tag (or basefont ""))))
         (bold (or (search "bold" n) (search "black" n) (search "heavy" n)
                   (search "semibold" n)))
         (ital (or (search "italic" n) (search "oblique" n))))
    (flet ((w () (if bold 700 400))
           (s () (if ital :italic :normal)))
      (flet ((has (&rest ss) (some (lambda (x) (search x n)) ss)))
       (cond
        ((has "zapf" "dingbat") (values '("ZapfDingbats") 400 :normal "ZapfDingbats" t))
        ;; math symbol families (Symbol, TeX CMSY/CMEX/CMMI, AMS) — no substitute
        ;; glyphs at these names, but resolved for width/advance
        ((has "symbol" "cmsy" "cmex" "cmmi" "msam" "msbm" "cmr7" ) ; note: cmmi is math italic
         (if (has "cmmi") (values '("Times" "serif") 400 :italic "Times-Italic" nil)
             (values '("Symbol") 400 :normal "Symbol" t)))
        ;; monospace: Courier, generic mono, TeX typewriter (cmtt / sftt)
        ((has "courier" "mono" "cmtt" "sftt" "cmsltt" "consol")
         (values '("Courier" "monospace") (w) (s)
                 (cond ((and bold ital) "Courier-BoldOblique") (bold "Courier-Bold")
                       (ital "Courier-Oblique") (t "Courier")) nil))
        ;; serif: Times/Nimbus Roman/Computer Modern Roman/Palatino/etc.
        ((has "times" "serif" "georgia" "roman" "garamond" "minion" "nimbusrom"
              "cmr" "cmbx" "cmti" "cmsl" "cmcsc" "palatino" "century" "book"
              "ncntr" "charter" "utopia" "libertine")
         (values '("Times" "serif") (w) (s)
                 (cond ((and bold ital) "Times-BoldItalic") (bold "Times-Bold")
                       (ital "Times-Italic") (t "Times-Roman")) nil))
        (t ;; default: Helvetica / Arial / any sans
         (values '("Helvetica" "Arial" "sans-serif") (w) (s)
                 (cond ((and bold ital) "Helvetica-BoldOblique") (bold "Helvetica-Bold")
                       (ital "Helvetica-Oblique") (t "Helvetica")) nil)))))))

;;; ---- the resolved font -----------------------------------------------------
(defstruct pdf-font
  face                 ; scribe open-font to draw through
  (upem 1000d0)
  (embedded nil)       ; T if FACE is the document's own embedded program
  (symbolic nil)       ; symbolic flag (built-in encoding preferred)
  encoding             ; (simple-vector 256) code -> glyph-name (or NIL)
  widths               ; (simple-vector 256) code -> width (1000/em) or NIL
  (missing-width 0d0)  ; default width for codes outside FirstChar..LastChar
  afm-key              ; standard-metrics table name for absent /Widths
  (type3 nil))         ; T for Type3 fonts (not rendered in M2)

(defun %resolve (doc v) (resolve doc v))

(defun read-font-program (doc descriptor)
  "If DESCRIPTOR carries a loadable font program (FontFile2 TrueType, or a
FontFile3 whose Subtype is an sfnt-wrapped OpenType), decode it and OPEN-FONT it.
Returns (values scribe-font kind) or (values NIL kind) where KIND notes what was
present (:truetype :cff :type1 or NIL) so the caller can fall back."
  (when (hash-table-p descriptor)
    (flet ((strm (key) (let ((s (%resolve doc (dict-get descriptor key))))
                         (and (pdf-stream-p s) s))))
      (let ((ff2 (strm "FontFile2")) (ff3 (strm "FontFile3")) (ff1 (strm "FontFile")))
        (cond
          (ff2 (values (ignore-errors
                        (scribe:open-font
                         (decode-stream ff2 :resolver (lambda (v) (%resolve doc v)))))
                       :truetype))
          (ff3 (values (ignore-errors
                        (scribe:open-font
                         (decode-stream ff3 :resolver (lambda (v) (%resolve doc v)))))
                       :cff))
          (ff1 (values nil :type1))
          (t (values nil nil)))))))

(defun build-encoding (doc font-dict base-names)
  "Build the 256-entry code->glyph-name table: start from a base encoding, then
apply any /Encoding /Differences.  /Encoding may be a name or a dict."
  (let* ((enc (%resolve doc (dict-get font-dict "Encoding")))
         (base (cond ((pdf-name-p enc) (base-encoding-vector (pdf-name-string enc)))
                     ((hash-table-p enc)
                      (let ((be (%resolve doc (dict-get enc "BaseEncoding"))))
                        (if (pdf-name-p be)
                            (base-encoding-vector (pdf-name-string be))
                            base-names)))
                     (t base-names)))
         (vec (make-array 256 :initial-contents (coerce base 'list))))
    ;; apply Differences: [ code /name /name ... code /name ... ]
    (when (hash-table-p enc)
      (let ((diffs (%resolve doc (dict-get enc "Differences"))))
        (when (or (pdf-array-p diffs) (listp diffs))
          (let ((cur 0))
            (map nil
                 (lambda (item)
                   (let ((it (%resolve doc item)))
                     (cond ((integerp it) (setf cur it))
                           ((numberp it) (setf cur (round it)))
                           ((pdf-name-p it)
                            (when (< -1 cur 256)
                              (setf (aref vec cur) (pdf-name-string it)))
                            (incf cur)))))
                 diffs)))))
    vec))

(defun build-widths (doc font-dict)
  "Return (values width-vector missing-width).  WIDTH-VECTOR is a 256-slot array
of advance widths (PDF glyph space) indexed by code, from /FirstChar + /Widths;
NIL slots fall back to the AFM/face metrics later."
  (let* ((fc (%resolve doc (dict-get font-dict "FirstChar")))
         (warr (%resolve doc (dict-get font-dict "Widths")))
         (desc (%resolve doc (dict-get font-dict "FontDescriptor")))
         (mw (and (hash-table-p desc) (%resolve doc (dict-get desc "MissingWidth"))))
         (vec (make-array 256 :initial-element nil)))
    (when (and (integerp fc) (or (pdf-array-p warr) (listp warr)))
      (let ((i 0))
        (map nil (lambda (w)
                   (let ((c (+ fc i)) (wv (%resolve doc w)))
                     (when (and (< -1 c 256) (numberp wv))
                       (setf (aref vec c) (float wv 1d0))))
                   (incf i))
             warr)))
    (values vec (if (numberp mw) (float mw 1d0) 0d0))))

(defun load-simple-font (doc font-dict)
  "Resolve a simple font dictionary to a PDF-FONT (never signals: falls back to a
Helvetica substitute on any error)."
  (handler-case
      (let* ((subtype (let ((s (%resolve doc (dict-get font-dict "Subtype"))))
                        (and (pdf-name-p s) (pdf-name-string s))))
             (basefont (let ((b (%resolve doc (dict-get font-dict "BaseFont"))))
                         (and (pdf-name-p b) (pdf-name-string b))))
             (descriptor (%resolve doc (dict-get font-dict "FontDescriptor")))
             (flags (and (hash-table-p descriptor)
                         (%resolve doc (dict-get descriptor "Flags")))))
        (when (equal subtype "Type3")
          (return-from load-simple-font
            (make-pdf-font :type3 t :face nil
                           :encoding (build-encoding doc font-dict +standard-names+))))
        (multiple-value-bind (families weight style afm-key symbolic)
            (standard-font-info basefont)
          (let* ((flag-symbolic (and (integerp flags) (logbitp 2 flags) ; bit 3 = Symbolic
                                     (not (logbitp 5 flags))))           ; not Nonsymbolic
                 (sym (or symbolic flag-symbolic))
                 ;; embedded program (FontFile2 TrueType most commonly)
                 (prog (read-font-program doc descriptor))
                 (face (or prog (scribe:match-font families :weight weight :style style)))
                 (embedded (and prog t))
                 (base-names (cond (symbolic (base-encoding-vector afm-key))
                                   (t +standard-names+))))
            (multiple-value-bind (wvec mw) (build-widths doc font-dict)
              (make-pdf-font
               :face face
               :upem (float (if face (scribe:font-units-per-em face) 1000) 1d0)
               :embedded embedded
               :symbolic sym
               :encoding (build-encoding doc font-dict base-names)
               :widths wvec
               :missing-width mw
               :afm-key afm-key)))))
    (error ()
      (make-pdf-font :face (ignore-errors (scribe:match-font '("Helvetica")))
                     :encoding (make-array 256 :initial-contents
                                           (coerce +standard-names+ 'list))))))

;;; ---- per-code glyph + width ------------------------------------------------
(defun font-code->gid (font code)
  "Glyph id in FONT's face for byte CODE, via the encoding glyph name -> Unicode
-> cmap.  Embedded symbolic TrueType fonts also try the (3,0) `F0xx` and the raw
code cmap slots.  Returns a gid (0 = .notdef / missing)."
  (let* ((face (pdf-font-face font)))
    (unless face (return-from font-code->gid 0))
    (flet ((gi (cp) (let ((g (ignore-errors (scribe:font-glyph-index face cp))))
                      (and g (plusp g) g))))    ; guard: some subset cmaps are unparsable
      (let* ((name (and (pdf-font-encoding font) (aref (pdf-font-encoding font) code)))
             (uni (and name (glyph-name->unicode name))))
        (or
         ;; 1. name -> unicode -> cmap
         (and uni (gi uni))
         ;; 2. symbolic / embedded: microsoft symbol cmap in the F000 block, then raw
         (and (or (pdf-font-symbolic font) (pdf-font-embedded font)) (gi (+ #xF000 code)))
         (gi code)
         0)))))

(defun font-code-width (font code)
  "Advance width of byte CODE in PDF glyph space (1000 units/em)."
  (let ((wvec (pdf-font-widths font)))
    (or (and wvec (aref wvec code))
        ;; AFM metric width by glyph name (standard-14, absent /Widths)
        (let* ((afm (and (pdf-font-afm-key font)
                         (gethash (pdf-font-afm-key font) *afm-widths*)))
               (name (and (pdf-font-encoding font) (aref (pdf-font-encoding font) code)))
               (w (and afm name (gethash name afm))))
          (and w (float w 1d0)))
        ;; face advance scaled from font units to 1000/em
        (let ((face (pdf-font-face font)))
          (when face
            (let ((gid (font-code->gid font code)))
              (* (float (scribe::glyph-advance face gid) 1d0)
                 (/ 1000d0 (max 1d0 (pdf-font-upem font)))))))
        (let ((mw (pdf-font-missing-width font)))
          (if (plusp mw) mw 500d0)))))

(defun font-glyph-contours (font gid)
  "The scribe glyph outline (segment lists in font units) for GID, or NIL."
  (let ((face (pdf-font-face font)))
    (and face (ignore-errors (scribe::glyph-outline face gid)))))
