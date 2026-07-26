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
  face                 ; scribe open-font to draw through (TrueType/sfnt programs)
  (upem 1000d0)
  (embedded nil)       ; T if FACE is the document's own embedded program
  (symbolic nil)       ; symbolic flag (built-in encoding preferred)
  encoding             ; (simple-vector 256) code -> glyph-name (or NIL)
  widths               ; (simple-vector 256) code -> width (1000/em) or NIL
  (missing-width 0d0)  ; default width for codes outside FirstChar..LastChar
  afm-key              ; standard-metrics table name for absent /Widths
  (type3 nil)          ; T for Type3 fonts (not rendered in M2)
  ;; ---- embedded CFF (FontFile3 / Type1C) drawn by the folio interpreter ----
  cff                  ; folio CFF object, or NIL (FACE is used instead)
  ;; ---- embedded Type1 (FontFile / eexec) drawn by the folio interpreter ----
  type1                ; folio T1 object, or NIL
  (has-encoding nil)   ; T when the PDF font dict carried an explicit /Encoding
  (synth-bold nil)     ; embolden a regular substitute standing in for a bold font
  ;; ---- Type0 composite / CID font state (§9.7) ----
  (composite nil)      ; T = Type0 (multi-byte codes, CID indirection)
  cid-kind             ; :type2 (CIDFontType2, TrueType) or :type0 (CIDFontType0, CFF)
  cmap                 ; decoded Encoding CMap (code -> CID), or NIL
  (cid-to-gid :identity) ; :identity or (simple-vector CID -> GID) for CIDFontType2
  (dw 1000d0)          ; /DW default CID width (1000/em)
  cid-widths)          ; hash CID -> width (1000/em) from /W, or NIL

(defun %resolve (doc v) (resolve doc v))

(defun %sfnt-bytes-p (bytes)
  "True when BYTES begin with a recognised sfnt/OpenType signature."
  (and (>= (length bytes) 4)
       (let ((v (logior (ash (aref bytes 0) 24) (ash (aref bytes 1) 16)
                        (ash (aref bytes 2) 8) (aref bytes 3))))
         (member v '(#x00010000 #x4F54544F #x74727565 #x74746366 #x774F4632 #x774F4646)))))

(defun read-font-program (doc descriptor)
  "If DESCRIPTOR carries a loadable font program decode it.  A FontFile2 (or an
sfnt-wrapped FontFile3) is handed to scribe:open-font; a bare FontFile3 CFF /
Type1C program is parsed by folio's own CFF interpreter; a FontFile (Adobe Type 1
/ eexec) program is parsed by folio's own Type 1 interpreter.  Returns
  (values scribe-face cff-object kind type1-object)
where at most one of SCRIBE-FACE / CFF-OBJECT / TYPE1-OBJECT is non-NIL, and KIND
is :truetype :cff :type1 or NIL so the caller can fall back to a substitute."
  (when (hash-table-p descriptor)
    (flet ((bytes (key) (let ((s (%resolve doc (dict-get descriptor key))))
                          (and (pdf-stream-p s)
                               (ignore-errors
                                (decode-stream s :resolver (lambda (v) (%resolve doc v))))))))
      (let ((ff2 (bytes "FontFile2")) (ff3 (bytes "FontFile3"))
            (ff1 (%resolve doc (dict-get descriptor "FontFile"))))
        (cond
          (ff2 (values (ignore-errors (scribe:open-font ff2)) nil :truetype))
          (ff3 (if (%sfnt-bytes-p ff3)
                   (values (ignore-errors (scribe:open-font ff3)) nil :cff)
                   (values nil (ignore-errors (parse-cff ff3)) :cff)))
          ((pdf-stream-p ff1)
           (let* ((body (bytes "FontFile"))
                  (d (pdf-stream-dict ff1))
                  (l1 (%resolve doc (dict-get d "Length1")))
                  (l2 (%resolve doc (dict-get d "Length2")))
                  (t1 (and body (ignore-errors
                                 (parse-type1 body (and (integerp l1) l1)
                                              (and (integerp l2) l2))))))
             (values nil nil :type1 t1)))
          (t (values nil nil nil)))))))

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
                 (base-names (cond (symbolic (base-encoding-vector afm-key))
                                   (t +standard-names+))))
            ;; embedded program: FontFile2 TrueType, bare FontFile3 CFF, or
            ;; FontFile Type 1 (eexec).
            (multiple-value-bind (prog-face prog-cff prog-kind prog-t1)
                (read-font-program doc descriptor)
              (declare (ignore prog-kind))
              (let* ((face (or prog-face
                               (and (not prog-cff) (not prog-t1)
                                    (scribe:match-font families :weight weight :style style))))
                     (embedded (and (or prog-face prog-cff prog-t1) t))
                     (upem (cond (prog-cff (cff-upem prog-cff))
                                 (prog-t1 (t1-upem prog-t1))
                                 (face (float (scribe:font-units-per-em face) 1d0))
                                 (t 1000d0)))
                     (has-enc (let ((e (%resolve doc (dict-get font-dict "Encoding"))))
                                (or (pdf-name-p e) (hash-table-p e))))
                     ;; bold indicated by the descriptor ForceBold flag (bit 19) or
                     ;; a bold-ish BaseFont name; synthesise weight only when we fell
                     ;; back to a regular substitute face (embedded outlines already
                     ;; carry the correct weight).
                     (bold-flag (and (integerp flags) (logbitp 18 flags)))
                     (bold-name (let ((n (string-downcase (strip-subset-tag (or basefont "")))))
                                  (or (search "bold" n) (search "black" n) (search "heavy" n)
                                      (search "semibold" n) (search "medi" n))))
                     ;; only synthesise when scribe has NO distinct bold face for this
                     ;; family (the weight-700 match is EQ to the regular one — e.g.
                     ;; single-weight DejaVu / Symbol / ZapfDingbats); Liberation bold
                     ;; is already the right weight, so leave it alone.
                     (synth-bold (and (or bold-flag bold-name) face (not embedded)
                                      (let ((reg (ignore-errors
                                                  (scribe:match-font families :weight 400 :style style))))
                                        (and reg (eq reg face))))))
                (multiple-value-bind (wvec mw) (build-widths doc font-dict)
                  (make-pdf-font
                   :face face
                   :cff prog-cff
                   :type1 prog-t1
                   :has-encoding has-enc
                   :synth-bold synth-bold
                   :upem upem
                   :embedded embedded
                   :symbolic sym
                   :encoding (build-encoding doc font-dict base-names)
                   :widths wvec
                   :missing-width mw
                   :afm-key afm-key)))))))
    (error ()
      (make-pdf-font :face (ignore-errors (scribe:match-font '("Helvetica")))
                     :encoding (make-array 256 :initial-contents
                                           (coerce +standard-names+ 'list))))))

;;; ---- per-code glyph + width ------------------------------------------------
(defun font-code->gid (font code)
  "Glyph id in FONT's face for byte CODE, via the encoding glyph name -> Unicode
-> cmap.  Embedded symbolic TrueType fonts also try the (3,0) `F0xx` and the raw
code cmap slots.  Returns a gid (0 = .notdef / missing)."
  ;; An embedded Type 1 (FontFile) program is name-keyed.  Prefer the PDF /Encoding
  ;; name when the dict carried one, else the font's own builtin /Encoding; fall back
  ;; to whichever names an actual charstring so a subset's custom encoding is honoured.
  (let ((t1 (pdf-font-type1 font)))
    (when t1
      (let* ((pdf-name (and (pdf-font-encoding font) (aref (pdf-font-encoding font) code)))
             (builtin (t1-encoding t1))
             (bname (and builtin (aref builtin code)))
             (order (if (pdf-font-has-encoding font)
                        (list pdf-name bname) (list bname pdf-name))))
        (dolist (nm order)
          (let ((gid (t1-name-gid t1 nm))) (when gid (return-from font-code->gid gid))))
        (return-from font-code->gid 0))))
  ;; An embedded CFF (FontFile3) is a name-keyed program: map the code's glyph
  ;; name straight to a GID through the CFF charset.
  (let ((cff (pdf-font-cff font)))
    (when cff
      (let* ((name (and (pdf-font-encoding font) (aref (pdf-font-encoding font) code)))
             (gid (and name (cff-name-gid cff name))))
        (return-from font-code->gid (or gid 0)))))
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
  "The glyph outline (segment lists in font units) for GID, or NIL.  An embedded
CFF is interpreted by folio; every other program is drawn through scribe."
  (let ((t1 (pdf-font-type1 font))
        (cff (pdf-font-cff font)))
    (cond
      (t1 (let ((names (t1-names t1)))
            (and (< -1 gid (length names))
                 (ignore-errors (t1-glyph-outline t1 (aref names gid))))))
      (cff (ignore-errors (cff-glyph-outline cff gid)))
      (t (let ((face (pdf-font-face font)))
           (and face (ignore-errors (scribe::glyph-outline face gid))))))))

;;; ===========================================================================
;;; Type0 / CID composite fonts (ISO 32000-1 §9.7)
;;; ===========================================================================
;;; A Type0 font shows text with multi-byte codes.  Its /Encoding is a CMap that
;;; maps a code to a CID; its single /DescendantFonts entry is a CIDFont that maps
;;; the CID to a glyph (a GID, via /CIDToGIDMap for CIDFontType2, or via the CFF
;;; charset for CIDFontType0) and supplies per-CID widths through /W and /DW.

;;; ---- CMap (code -> CID): the CMAPO struct, its parser, the predefined Adobe
;;; ---- CJK CMap bundle and the segmentation/lookup helpers all live in
;;; ---- src/cmap.lisp (loaded before this file).

;;; ---- /W + /DW CID widths ---------------------------------------------------
(defun build-cid-widths (doc cidfont)
  "Parse the descendant CIDFont /W array into a hash CID -> width (1000/em); the
default width /DW is returned as the second value."
  (let ((h (make-hash-table :test 'eql))
        (w (%resolve doc (dict-get cidfont "W")))
        (dw (%resolve doc (dict-get cidfont "DW"))))
    (when (or (pdf-array-p w) (listp w))
      (let ((items (coerce w 'list)))
        (loop while items do
          (let ((c (%resolve doc (pop items))))
            (when (and items (numberp c))
              (let ((nxt (%resolve doc (car items))))
                (cond
                  ((or (pdf-array-p nxt) (listp nxt))     ; c [w1 w2 ...]
                   (pop items)
                   (let ((i 0))
                     (map nil (lambda (wv)
                                (let ((v (%resolve doc wv)))
                                  (when (numberp v)
                                    (setf (gethash (+ (round c) i) h) (float v 1d0))))
                                (incf i))
                          nxt)))
                  ((numberp nxt)                          ; cfirst clast w
                   (pop items)
                   (let ((cl (round nxt)) (wv (%resolve doc (pop items))))
                     (when (numberp wv)
                       (loop for cid from (round c) to cl
                             do (setf (gethash cid h) (float wv 1d0))))))
                  (t (pop items)))))))))
    (values h (if (numberp dw) (float dw 1d0) 1000d0))))

(defun build-cid-to-gid (doc cidfont)
  "CIDFontType2 /CIDToGIDMap: :identity, or a (simple-vector CID -> GID)."
  (let ((m (%resolve doc (dict-get cidfont "CIDToGIDMap"))))
    (cond
      ((pdf-stream-p m)
       (let* ((b (ignore-errors (decode-stream m :resolver (lambda (v) (%resolve doc v)))))
              (n (and b (floor (length b) 2)))
              (v (and n (make-array n :element-type 'fixnum))))
         (when v
           (dotimes (i n) (setf (aref v i) (logior (ash (aref b (* 2 i)) 8) (aref b (1+ (* 2 i)))))))
         (or v :identity)))
      (t :identity))))                  ; /Identity or absent

(defun load-type0-font (doc font-dict)
  "Resolve a Type0 composite font dictionary to a PDF-FONT (composite=T)."
  (let* ((enc (%resolve doc (dict-get font-dict "Encoding")))
         (df (%resolve doc (dict-get font-dict "DescendantFonts")))
         (cidfont (%resolve doc (and (or (pdf-array-p df) (listp df))
                                     (elt (coerce df 'list) 0))))
         (subtype (let ((s (%resolve doc (dict-get cidfont "Subtype"))))
                    (and (pdf-name-p s) (pdf-name-string s))))
         (descriptor (%resolve doc (dict-get cidfont "FontDescriptor")))
         (cmap (cmap-for-encoding doc enc)))
    (multiple-value-bind (prog-face prog-cff kind) (read-font-program doc descriptor)
      (declare (ignore kind))
      (let* ((cid-kind (if (equal subtype "CIDFontType0") :type0 :type2))
             (face prog-face)
             (upem (cond (prog-cff (cff-upem prog-cff))
                         (face (float (scribe:font-units-per-em face) 1d0))
                         (t 1000d0))))
        (multiple-value-bind (wmap dw) (build-cid-widths doc cidfont)
          (make-pdf-font
           :composite t
           :cid-kind cid-kind
           :face face
           :cff prog-cff
           :upem upem
           :embedded (and (or face prog-cff) t)
           :cmap cmap
           :cid-to-gid (if (eq cid-kind :type2) (build-cid-to-gid doc cidfont) :identity)
           :dw dw
           :cid-widths wmap))))))

(defun cid->gid (font cid)
  "Map a CID to a glyph id in FONT's descendant program."
  (if (eq (pdf-font-cid-kind font) :type0)
      (let ((cff (pdf-font-cff font)))
        (if cff (cff-map-cid cff cid) cid))
      (let ((m (pdf-font-cid-to-gid font)))    ; CIDFontType2
        (cond ((eq m :identity) cid)
              ((and (vectorp m) (< cid (length m))) (aref m cid))
              (t 0)))))

(defun cid-width (font cid)
  "Advance width (1000/em) of CID: /W entry, else /DW."
  (or (and (pdf-font-cid-widths font) (gethash cid (pdf-font-cid-widths font)))
      (pdf-font-dw font)))

(defun load-font (doc font-dict)
  "Resolve any font dictionary to a PDF-FONT: Type0 composite or a simple font."
  (let ((s (%resolve doc (dict-get font-dict "Subtype"))))
    (if (and (pdf-name-p s) (string= (pdf-name-string s) "Type0"))
        (handler-case (load-type0-font doc font-dict)
          (error () (load-simple-font doc font-dict)))
        (load-simple-font doc font-dict))))
