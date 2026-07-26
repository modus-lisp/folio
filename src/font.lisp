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
Type1C program is parsed by folio's own CFF interpreter.  Returns
  (values scribe-face cff-object kind)
where exactly one of SCRIBE-FACE / CFF-OBJECT is non-NIL (or both NIL), and KIND
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
          ((pdf-stream-p ff1) (values nil nil :type1))
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
            ;; embedded program: FontFile2 TrueType, or a bare FontFile3 CFF.
            (multiple-value-bind (prog-face prog-cff) (read-font-program doc descriptor)
              (let* ((face (or prog-face
                               (and (not prog-cff)
                                    (scribe:match-font families :weight weight :style style))))
                     (embedded (and (or prog-face prog-cff) t))
                     (upem (cond (prog-cff (cff-upem prog-cff))
                                 (face (float (scribe:font-units-per-em face) 1d0))
                                 (t 1000d0))))
                (multiple-value-bind (wvec mw) (build-widths doc font-dict)
                  (make-pdf-font
                   :face face
                   :cff prog-cff
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
  (let ((cff (pdf-font-cff font)))
    (if cff
        (ignore-errors (cff-glyph-outline cff gid))
        (let ((face (pdf-font-face font)))
          (and face (ignore-errors (scribe::glyph-outline face gid)))))))

;;; ===========================================================================
;;; Type0 / CID composite fonts (ISO 32000-1 §9.7)
;;; ===========================================================================
;;; A Type0 font shows text with multi-byte codes.  Its /Encoding is a CMap that
;;; maps a code to a CID; its single /DescendantFonts entry is a CIDFont that maps
;;; the CID to a glyph (a GID, via /CIDToGIDMap for CIDFontType2, or via the CFF
;;; charset for CIDFontType0) and supplies per-CID widths through /W and /DW.

;;; ---- CMap (code -> CID) ----------------------------------------------------
(defstruct cmapo
  (identity nil)      ; T for Identity-H / Identity-V (2-byte code, CID = code)
  (ranges nil)        ; list of (nbytes lo hi) codespace ranges
  (chars nil)         ; hash code -> CID (from begincidchar)
  (cidranges nil))    ; list of (lo hi cid) (from begincidrange)

(defun bytes->uint (v &optional (start 0) (end (length v)))
  (loop with n = 0 for i from start below end do (setf n (logior (ash n 8) (aref v i)))
        finally (return n)))

(defun parse-embedded-cmap (bytes)
  "Parse a CMap stream body into a CMAPO (codespace + cid mappings).  Uses the PDF
lexer to walk the PostScript tokens; recognises the codespace/cidrange/cidchar
sections that determine code lengths and CID values."
  (let ((lx (make-lexer bytes))
        (cm (make-cmapo :chars (make-hash-table :test 'eql)))
        (pending '()))                  ; recent operands (hex strings / ints), newest first
    (labels ((hexint (tok) (bytes->uint (token-value tok)))
             (hexlen (tok) (length (token-value tok))))
      (loop for tok = (lex-next lx)
            until (eq (token-type tok) :eof) do
        (case (token-type tok)
          ((:string :int :real :name) (push tok pending))
          (:keyword
           (let ((kw (token-value tok)))
             (cond
               ((string= kw "begincodespacerange")
                (loop for lo = (lex-next lx)
                      until (or (eq (token-type lo) :eof)
                                (and (eq (token-type lo) :keyword)
                                     (string= (token-value lo) "endcodespacerange")))
                      do (let ((hi (lex-next lx)))
                           (when (and (eq (token-type lo) :string) (eq (token-type hi) :string))
                             (push (list (hexlen lo) (hexint lo) (hexint hi))
                                   (cmapo-ranges cm))))))
               ((string= kw "begincidrange")
                (loop for lo = (lex-next lx)
                      until (or (eq (token-type lo) :eof)
                                (and (eq (token-type lo) :keyword)
                                     (string= (token-value lo) "endcidrange")))
                      do (let ((hi (lex-next lx)) (cid (lex-next lx)))
                           (when (and (eq (token-type lo) :string) (eq (token-type hi) :string)
                                      (member (token-type cid) '(:int :real)))
                             (push (list (hexint lo) (hexint hi) (round (token-value cid)))
                                   (cmapo-cidranges cm))))))
               ((string= kw "begincidchar")
                (loop for co = (lex-next lx)
                      until (or (eq (token-type co) :eof)
                                (and (eq (token-type co) :keyword)
                                     (string= (token-value co) "endcidchar")))
                      do (let ((cid (lex-next lx)))
                           (when (and (eq (token-type co) :string)
                                      (member (token-type cid) '(:int :real)))
                             (setf (gethash (hexint co) (cmapo-chars cm)) (round (token-value cid)))))))
               ((string= kw "usecmap")
                (let ((nm (find-if (lambda (tk) (eq (token-type tk) :name)) pending)))
                  (when (and nm (search "Identity" (token-value nm)))
                    (setf (cmapo-identity cm) t))))
               (t nil))
             (setf pending '())))
          (t (setf pending '())))))
    (unless (cmapo-ranges cm) (setf (cmapo-ranges cm) (list (list 2 0 #xFFFF))))
    cm))

(defun cmap-for-encoding (doc enc)
  "Build a CMAPO from a Type0 /Encoding value: a predefined name (Identity-H/V,
else best-effort 2-byte identity) or an embedded CMap stream."
  (cond
    ((pdf-name-p enc)
     (let ((n (pdf-name-string enc)))
       (if (or (string= n "Identity-H") (string= n "Identity-V") (search "Identity" n))
           (make-cmapo :identity t :ranges (list (list 2 0 #xFFFF)))
           ;; predefined CJK CMap: table not bundled — fall back to 2-byte identity
           (make-cmapo :identity t :ranges (list (list 2 0 #xFFFF))))))
    ((pdf-stream-p enc)
     (or (ignore-errors
          (parse-embedded-cmap (decode-stream enc :resolver (lambda (v) (%resolve doc v)))))
         (make-cmapo :identity t :ranges (list (list 2 0 #xFFFF)))))
    (t (make-cmapo :identity t :ranges (list (list 2 0 #xFFFF))))))

(defun cmap-next-code (cm bytes i)
  "Return (values code nbytes) for the code beginning at index I of BYTES, using
the codespace ranges to size it."
  (let ((n (length bytes)))
    (when (cmapo-identity cm)
      (let ((nb (min 2 (- n i)))) (return-from cmap-next-code (values (bytes->uint bytes i (+ i nb)) (max 1 nb)))))
    (dolist (r (cmapo-ranges cm))
      (destructuring-bind (nb lo hi) r
        (when (<= (+ i nb) n)
          (let ((code (bytes->uint bytes i (+ i nb))))
            (when (<= lo code hi) (return-from cmap-next-code (values code nb)))))))
    ;; no range matched: consume the first range's width (or one byte)
    (let* ((nb (min (or (first (first (cmapo-ranges cm))) 1) (- n i)))
           (nb (max 1 nb)))
      (values (bytes->uint bytes i (+ i nb)) nb))))

(defun cmap-code->cid (cm code)
  "Map a CODE to a CID through CM."
  (cond ((cmapo-identity cm) code)
        ((gethash code (cmapo-chars cm)))
        (t (dolist (r (cmapo-cidranges cm) 0)
             (destructuring-bind (lo hi cid) r
               (when (<= lo code hi) (return (+ cid (- code lo)))))))))

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
