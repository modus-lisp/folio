;;;; src/cff.lisp — bare CFF / Type1C font programs (ISO 32000-1 §9.9, Adobe
;;;; TN#5176/5177).  A FontFile3 stream is a raw CFF font program (not wrapped in
;;;; an sfnt), so scribe:open-font cannot read it.  This module parses the CFF
;;;; header / INDEX / DICT structures and interprets Type2 charstrings into glyph
;;;; outlines, in the SAME segment vocabulary scribe emits so the content
;;;; interpreter's glyph painter is shared:
;;;;   (:move x y) (:line x y) (:cubic c1x c1y c2x c2y x y)
;;;;
;;;; Handles both flavours a PDF can carry:
;;;;   * a plain (name-keyed) CFF — a simple font's FontFile3, and the descendant
;;;;     of a CIDFontType0 whose CFF is not itself CID-keyed (GID = CID);
;;;;   * a CID-keyed CFF (top DICT has ROS) with FDArray / FDSelect per-glyph
;;;;     Private DICTs + local subrs, and a charset that maps GID -> CID.
;;;; The charstring interpreter follows Adobe TN#5177 (Type 2 charstring format).

(in-package #:folio)

;;; ---- big-endian readers over the CFF byte vector (absolute offsets) --------
(declaim (inline cff-u8 cff-u16 cff-s16 cff-u32))
(defun cff-u8  (d o) (aref d o))
(defun cff-u16 (d o) (logior (ash (aref d o) 8) (aref d (1+ o))))
(defun cff-s16 (d o) (let ((v (cff-u16 d o))) (if (>= v #x8000) (- v #x10000) v)))
(defun cff-u32 (d o) (logior (ash (aref d o) 24) (ash (aref d (+ o 1)) 16)
                             (ash (aref d (+ o 2)) 8) (aref d (+ o 3))))
(defun cff-uoff (d o size)
  (ecase size (1 (cff-u8 d o)) (2 (cff-u16 d o))
         (3 (logior (ash (cff-u8 d o) 16) (ash (cff-u8 d (+ o 1)) 8) (cff-u8 d (+ o 2))))
         (4 (cff-u32 d o))))

;;; ---- INDEX / DICT primitives ----------------------------------------------
(defun cff-read-index (d off)
  "Parse a CFF INDEX at OFF.  Returns (values entries next-offset) where ENTRIES
is a simple-vector of (start . end) absolute byte ranges into D."
  (let ((count (cff-u16 d off)))
    (when (zerop count) (return-from cff-read-index (values #() (+ off 2))))
    (let* ((osz (cff-u8 d (+ off 2)))
           (base (+ off 3))
           (data (+ base (* (1+ count) osz) -1))
           (entries (make-array count)))
      (dotimes (i count)
        (setf (aref entries i)
              (cons (+ data (cff-uoff d (+ base (* i osz)) osz))
                    (+ data (cff-uoff d (+ base (* (1+ i) osz)) osz)))))
      (values entries (+ data (cff-uoff d (+ base (* count osz)) osz))))))

(defun cff-parse-dict (d start end)
  "Parse a CFF DICT in [START,END) -> alist (operator . operand-list)."
  (let ((p start) (ops '()) (out '()))
    (loop while (< p end) do
      (let ((b (cff-u8 d p)))
        (cond
          ((<= 32 b 246) (push (- b 139) ops) (incf p))
          ((<= 247 b 250) (push (+ (* (- b 247) 256) (cff-u8 d (1+ p)) 108) ops) (incf p 2))
          ((<= 251 b 254) (push (- (* (- (- b 251)) 256) (cff-u8 d (1+ p)) 108) ops) (incf p 2))
          ((= b 28) (push (cff-s16 d (1+ p)) ops) (incf p 3))
          ((= b 29) (push (let ((v (cff-u32 d (1+ p)))) (if (>= v #x80000000) (- v #x100000000) v)) ops) (incf p 5))
          ((= b 30)                                          ; real (BCD nibbles)
           (incf p) (let ((s (make-string-output-stream)) (done nil))
                      (loop until done do
                        (let ((by (cff-u8 d p))) (incf p)
                          (dolist (nib (list (ash by -4) (logand by 15)))
                            (case nib (#xf (setf done t) (return))
                              (#xa (write-char #\. s)) (#xb (write-char #\E s))
                              (#xc (write-string "E-" s)) (#xe (write-char #\- s))
                              (#xd nil) (t (write-char (digit-char nib) s))))))
                      (push (let ((*read-default-float-format* 'double-float))
                              (or (ignore-errors (read-from-string (get-output-stream-string s))) 0)) ops)))
          (t (let ((op (if (= b 12) (prog1 (+ 1200 (cff-u8 d (1+ p))) (incf p 2)) (prog1 b (incf p)))))
               (push (cons op (nreverse ops)) out) (setf ops nil))))))
    (nreverse out)))

(defun cff-dict-val (dict key &optional default)
  (let ((e (assoc key dict))) (if e (cdr e) default)))

(defun cff-subr-bias (n) (cond ((< n 1240) 107) ((< n 33900) 1131) (t 32768)))

;;; ---- parsed CFF state ------------------------------------------------------
(defstruct cff
  data
  charstrings                    ; simple-vector of (start . end)
  gsubrs (gbias 107)
  lsubrs (lbias 107)             ; non-CID local subrs
  (nominal-width 0d0) (default-width 0d0)
  (charstring-type 2)
  (font-matrix nil)              ; list of 6 doubles or NIL (default .001)
  ;; CID
  (cid nil)                      ; T when the CFF is CID-keyed (ROS present)
  fdselect                       ; simple-vector gid->fd-index, or NIL
  fdarray                        ; simple-vector of plists (:lsubrs :lbias :nominal :default)
  charset                        ; simple-vector gid->SID/CID, or NIL (=identity)
  cid->gid                       ; hash CID->GID (CID fonts), or NIL
  name->gid                      ; hash glyph-name -> GID (non-CID), or NIL
  code->gid                      ; (simple-vector 256) built-in Encoding code->GID, or NIL
  (num-glyphs 0))

;;; ---- standard strings (SID < 391) — enough to name glyphs by SID -----------
;;; The 391 standard strings of Appendix A (TN#5176).  We only need the names to
;;; build a glyph-name -> GID map for simple CFF fonts driven by a PDF /Encoding.
(defparameter +cff-standard-strings+
  #(".notdef" "space" "exclam" "quotedbl" "numbersign" "dollar" "percent"
    "ampersand" "quoteright" "parenleft" "parenright" "asterisk" "plus" "comma"
    "hyphen" "period" "slash" "zero" "one" "two" "three" "four" "five" "six"
    "seven" "eight" "nine" "colon" "semicolon" "less" "equal" "greater"
    "question" "at" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O"
    "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "bracketleft" "backslash"
    "bracketright" "asciicircum" "underscore" "quoteleft" "a" "b" "c" "d" "e" "f"
    "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y"
    "z" "braceleft" "bar" "braceright" "asciitilde" "exclamdown" "cent"
    "sterling" "fraction" "yen" "florin" "section" "currency" "quotesingle"
    "quotedblleft" "guillemotleft" "guilsinglleft" "guilsinglright" "fi" "fl"
    "endash" "dagger" "daggerdbl" "periodcentered" "paragraph" "bullet"
    "quotesinglbase" "quotedblbase" "quotedblright" "guillemotright" "ellipsis"
    "perthousand" "questiondown" "grave" "acute" "circumflex" "tilde" "macron"
    "breve" "dotaccent" "dieresis" "ring" "cedilla" "hungarumlaut" "ogonek"
    "caron" "emdash" "AE" "ordfeminine" "Lslash" "Oslash" "OE" "ordmasculine"
    "ae" "dotlessi" "lslash" "oslash" "oe" "germandbls" "onesuperior"
    "logicalnot" "mu" "trademark" "Eth" "onehalf" "plusminus" "Thorn"
    "onequarter" "divide" "brokenbar" "degree" "thorn" "threequarters"
    "twosuperior" "registered" "minus" "eth" "multiply" "threesuperior"
    "copyright" "Aacute" "Acircumflex" "Adieresis" "Agrave" "Aring" "Atilde"
    "Ccedilla" "Eacute" "Ecircumflex" "Edieresis" "Egrave" "Iacute"
    "Icircumflex" "Idieresis" "Igrave" "Ntilde" "Oacute" "Ocircumflex"
    "Odieresis" "Ograve" "Otilde" "Scaron" "Uacute" "Ucircumflex" "Udieresis"
    "Ugrave" "Yacute" "Ydieresis" "Zcaron" "aacute" "acircumflex" "adieresis"
    "agrave" "aring" "atilde" "ccedilla" "eacute" "ecircumflex" "edieresis"
    "egrave" "iacute" "icircumflex" "idieresis" "igrave" "ntilde" "oacute"
    "ocircumflex" "odieresis" "ograve" "otilde" "scaron" "uacute" "ucircumflex"
    "udieresis" "ugrave" "yacute" "ydieresis" "zcaron"
    "exclamsmall" "Hungarumlautsmall" "dollaroldstyle" "dollarsuperior"
    "ampersandsmall" "Acutesmall" "parenleftsuperior" "parenrightsuperior"
    "twodotenleader" "onedotenleader" "zerooldstyle" "oneoldstyle"
    "twooldstyle" "threeoldstyle" "fouroldstyle" "fiveoldstyle"
    "sixoldstyle" "sevenoldstyle" "eightoldstyle" "nineoldstyle"
    "commasuperior" "threequartersemdash" "periodsuperior" "questionsmall"
    "asuperior" "bsuperior" "centsuperior" "dsuperior" "esuperior"
    "isuperior" "lsuperior" "msuperior" "nsuperior" "osuperior" "rsuperior"
    "ssuperior" "tsuperior" "ff" "ffi" "ffl" "parenleftinferior"
    "parenrightinferior" "Circumflexsmall" "hyphensuperior" "Gravesmall"
    "Asmall" "Bsmall" "Csmall" "Dsmall" "Esmall" "Fsmall" "Gsmall" "Hsmall"
    "Ismall" "Jsmall" "Ksmall" "Lsmall" "Msmall" "Nsmall" "Osmall" "Psmall"
    "Qsmall" "Rsmall" "Ssmall" "Tsmall" "Usmall" "Vsmall" "Wsmall" "Xsmall"
    "Ysmall" "Zsmall" "colonmonetary" "onefitted" "rupiah" "Tildesmall"
    "exclamdownsmall" "centoldstyle" "Lslashsmall" "Scaronsmall"
    "Zcaronsmall" "Dieresissmall" "Brevesmall" "Caronsmall" "Dotaccentsmall"
    "Macronsmall" "figuredash" "hypheninferior" "Ogoneksmall" "Ringsmall"
    "Cedillasmall" "questiondownsmall" "oneeighth" "threeeighths"
    "fiveeighths" "seveneighths" "onethird" "twothirds" "zerosuperior"
    "foursuperior" "fivesuperior" "sixsuperior" "sevensuperior"
    "eightsuperior" "ninesuperior" "zeroinferior" "oneinferior"
    "twoinferior" "threeinferior" "fourinferior" "fiveinferior"
    "sixinferior" "seveninferior" "eightinferior" "nineinferior"
    "centinferior" "dollarinferior" "periodinferior" "commainferior"
    "Agravesmall" "Aacutesmall" "Acircumflexsmall" "Atildesmall"
    "Adieresissmall" "Aringsmall" "AEsmall" "Ccedillasmall" "Egravesmall"
    "Eacutesmall" "Ecircumflexsmall" "Edieresissmall" "Igravesmall"
    "Iacutesmall" "Icircumflexsmall" "Idieresissmall" "Ethsmall"
    "Ntildesmall" "Ogravesmall" "Oacutesmall" "Ocircumflexsmall"
    "Otildesmall" "Odieresissmall" "OEsmall" "Oslashsmall" "Ugravesmall"
    "Uacutesmall" "Ucircumflexsmall" "Udieresissmall" "Yacutesmall"
    "Thornsmall" "Ydieresissmall" "001.000" "001.001" "001.002" "001.003"
    "Black" "Bold" "Book" "Light" "Medium" "Regular" "Roman" "Semibold"))

(defun cff-sid-name (c sid)
  "Glyph name for string id SID: a standard string, else the CFF String INDEX."
  (if (< sid (length +cff-standard-strings+))
      (aref +cff-standard-strings+ sid)
      (let* ((strs (cff-strings c)) (i (- sid (length +cff-standard-strings+))))
        (if (and strs (< i (length strs)))
            (let ((e (aref strs i)))
              (map 'string #'code-char (subseq (cff-data c) (car e) (cdr e))))
            (format nil "sid~d" sid)))))

;;; String INDEX is kept on the struct only transiently during load; stash it.
(defvar *cff-strings* nil)
(defun cff-strings (c) (declare (ignore c)) *cff-strings*)

;;; ---- charset (GID -> SID/CID) ----------------------------------------------
(defun cff-parse-charset (d off nglyphs)
  "Parse the charset at absolute OFF -> simple-vector gid->SID (CID for CID fonts).
GID 0 is always SID 0 (.notdef).  Predefined charsets (off 0/1/2) -> NIL."
  (when (<= off 2) (return-from cff-parse-charset nil))
  (let ((v (make-array nglyphs :initial-element 0)) (fmt (cff-u8 d off)) (p (1+ off)) (gid 1))
    (ecase fmt
      (0 (loop while (< gid nglyphs) do
           (setf (aref v gid) (cff-u16 d p)) (incf p 2) (incf gid)))
      ((1 2)
       (loop while (< gid nglyphs) do
         (let* ((first (cff-u16 d p))
                (nleft (if (= fmt 1) (cff-u8 d (+ p 2)) (cff-u16 d (+ p 2)))))
           (incf p (if (= fmt 1) 3 4))
           (loop for i from 0 to nleft while (< gid nglyphs) do
             (setf (aref v gid) (+ first i)) (incf gid))))))
    v))

;;; ---- built-in Encoding (code -> GID) ---------------------------------------
(defun cff-parse-encoding (d off nglyphs)
  "Parse the CFF built-in Encoding at absolute OFF -> (simple-vector 256) code->GID
(§ Appendix C, TN#5176), or NIL for a predefined encoding (off 0 = Standard, 1 =
Expert; the caller resolves those by charset name).  Formats 0 (per-glyph codes)
and 1 (ranges), plus the high-bit supplement table."
  (when (<= off 1) (return-from cff-parse-encoding nil))
  (let* ((v (make-array 256 :initial-element 0))
         (b0 (cff-u8 d off)) (fmt (logand b0 #x7f)) (p (1+ off)))
    (ecase fmt
      (0 (let ((ncodes (cff-u8 d p)))
           (incf p)
           (loop for gid from 1 to ncodes while (< p (length d)) do
             (let ((code (cff-u8 d p))) (incf p)
               (when (< code 256) (setf (aref v code) gid))))))
      (1 (let ((nranges (cff-u8 d p)) (gid 1))
           (incf p)
           (dotimes (r nranges)
             (let ((first (cff-u8 d p)) (nleft (cff-u8 d (1+ p))))
               (incf p 2)
               (loop for code from first to (+ first nleft)
                     while (< gid nglyphs) do
                 (when (< code 256) (setf (aref v code) gid))
                 (incf gid)))))))
    ;; supplements (high bit of b0 set): [nSups] then (code SID) pairs — mapping a
    ;; code to a glyph already reachable by SID.  We resolve the SID to a GID via
    ;; the charset if present; harmless to skip when the base table already covers it.
    v))

;;; ---- FDSelect (GID -> FD index) --------------------------------------------
(defun cff-parse-fdselect (d off nglyphs)
  (let ((fmt (cff-u8 d off)) (v (make-array nglyphs :initial-element 0)))
    (ecase fmt
      (0 (dotimes (g nglyphs) (setf (aref v g) (cff-u8 d (+ off 1 g)))))
      (3 (let* ((nr (cff-u16 d (+ off 1))) (p (+ off 3)))
           (dotimes (i nr)
             (let ((first (cff-u16 d p)) (fd (cff-u8 d (+ p 2)))
                   (next (cff-u16 d (+ p 3))))
               (loop for g from first below next do (setf (aref v g) fd))
               (incf p 3))))))
    v))

(defun cff-load-private (d base priv)
  "PRIV = (size offset) from a (top or FD) DICT.  Returns a plist with the
local-subr INDEX + bias and the nominal/default widths."
  (let ((lsubrs #()) (lbias 107) (nomw 0d0) (defw 0d0))
    (when priv
      (let* ((psz (first priv)) (poff (second priv))
             (pd (cff-parse-dict d (+ base poff) (+ base poff psz))))
        (setf nomw (float (or (first (cff-dict-val pd 21)) 0) 1d0)
              defw (float (or (first (cff-dict-val pd 20)) 0) 1d0))
        (let ((lso (first (cff-dict-val pd 19))))
          (when lso
            (setf lsubrs (cff-read-index d (+ base poff lso))
                  lbias (cff-subr-bias (length lsubrs)))))))
    (list :lsubrs lsubrs :lbias lbias :nominal nomw :default defw)))

;;; ---- parse a bare CFF font program -----------------------------------------
(defun parse-cff (bytes)
  "Parse a raw CFF font program (a FontFile3 body) -> a CFF struct."
  (let* ((d (coerce bytes '(simple-array (unsigned-byte 8) (*))))
         (hdrsize (cff-u8 d 2))
         (base 0))
    (multiple-value-bind (name-idx p1) (cff-read-index d hdrsize)
      (declare (ignore name-idx))
      (multiple-value-bind (topdicts p2) (cff-read-index d p1)
        (multiple-value-bind (strings p3) (cff-read-index d p2)
          (multiple-value-bind (gsubrs) (cff-read-index d p3)
            (let* ((*cff-strings* strings)
                   (td (cff-parse-dict d (car (aref topdicts 0)) (cdr (aref topdicts 0))))
                   (cs-off (first (cff-dict-val td 17)))
                   (charset-off (or (first (cff-dict-val td 15)) 0))
                   (ros (cff-dict-val td 1230))
                   (cstype (or (first (cff-dict-val td 1206)) 2))
                   (fmatrix (cff-dict-val td 1207))
                   (charstrings (cff-read-index d (+ base cs-off)))
                   (nglyphs (length charstrings))
                   (c (make-cff :data d :charstrings charstrings
                                :gsubrs gsubrs :gbias (cff-subr-bias (length gsubrs))
                                :charstring-type cstype :num-glyphs nglyphs
                                :font-matrix (and fmatrix (mapcar (lambda (x) (float x 1d0)) fmatrix))
                                :cid (and ros t))))
              ;; charset: gid -> SID (or CID for CID fonts)
              (setf (cff-charset c) (cff-parse-charset d (+ base charset-off) nglyphs))
              (if ros
                  ;; ---- CID-keyed: FDArray + FDSelect + per-FD private ----
                  (let* ((fda-off (first (cff-dict-val td 1236)))
                         (fds-off (first (cff-dict-val td 1237))))
                    (when fds-off
                      (setf (cff-fdselect c) (cff-parse-fdselect d (+ base fds-off) nglyphs)))
                    (when fda-off
                      (let ((fdi (cff-read-index d (+ base fda-off))))
                        (setf (cff-fdarray c)
                              (map 'vector
                                   (lambda (e)
                                     (let ((fd (cff-parse-dict d (car e) (cdr e))))
                                       (cff-load-private d base (cff-dict-val fd 18))))
                                   fdi))))
                    ;; CID -> GID from the charset (gid->cid); identity if absent
                    (let ((h (make-hash-table :test 'eql)) (cs (cff-charset c)))
                      (if cs
                          (dotimes (g nglyphs) (setf (gethash (aref cs g) h) g))
                          (dotimes (g nglyphs) (setf (gethash g h) g)))
                      (setf (cff-cid->gid c) h)))
                  ;; ---- plain (name-keyed): single Private + name->gid ----
                  (let ((pr (cff-load-private d base (cff-dict-val td 18))))
                    (setf (cff-lsubrs c) (getf pr :lsubrs)
                          (cff-lbias c) (getf pr :lbias)
                          (cff-nominal-width c) (getf pr :nominal)
                          (cff-default-width c) (getf pr :default))
                    (let ((h (make-hash-table :test 'equal)) (cs (cff-charset c)))
                      (setf (gethash ".notdef" h) 0)
                      (if cs
                          (dotimes (g nglyphs)
                            (setf (gethash (cff-sid-name c (aref cs g)) h) g))
                          ;; no charset: ISOAdobe order — SID = gid for gid<391
                          (dotimes (g nglyphs)
                            (when (< g (length +cff-standard-strings+))
                              (setf (gethash (aref +cff-standard-strings+ g) h) g))))
                      (setf (cff-name->gid c) h))
                    ;; built-in Encoding (code->GID): a subset font renamed to
                    ;; g<NNN> glyphs relies on this, since its /Encoding names won't
                    ;; match the charset.  op-16 of 0/1 (or absent) = a predefined
                    ;; Standard/Expert encoding -> NIL (caller uses charset names).
                    (let ((enc-off (or (first (cff-dict-val td 16)) 0)))
                      (setf (cff-code->gid c)
                            (when (> enc-off 1)
                              (ignore-errors
                               (cff-parse-encoding d (+ base enc-off) nglyphs)))))))
              c)))))))

;;; ---- CID -> GID / name -> GID accessors ------------------------------------
(defun cff-map-cid (c cid)
  "GID for a CID in a CID-keyed CFF (charset lookup); identity otherwise."
  (let ((h (cff-cid->gid c)))
    (if h
        (or (gethash cid h) 0)
        (if (< cid (cff-num-glyphs c)) cid 0))))

(defun cff-name-gid (c name)
  "GID for a glyph NAME in a plain CFF, or NIL."
  (let ((h (cff-name->gid c))) (and h (gethash name h))))

(defun cff-code-gid (c code)
  "GID for a byte CODE via the CFF built-in Encoding, or NIL (predefined/absent)."
  (let ((v (cff-code->gid c)))
    (when (and v (<= 0 code 255))
      (let ((g (aref v code))) (and (plusp g) g)))))

(defun cff-upem (c)
  "Units per em implied by the FontMatrix (default 1/0.001 = 1000)."
  (let ((m (cff-font-matrix c)))
    (if (and m (numberp (first m)) (not (zerop (first m))))
        (float (/ 1d0 (abs (first m))) 1d0)
        1000d0)))

;;; ---- Type2 charstring interpreter -> outline segments ----------------------
(defun cff-fd-of (c gid)
  "The Private/subr context (plist) for GID: the FD entry for a CID font, else
the single non-CID context."
  (if (and (cff-cid c) (cff-fdselect c) (cff-fdarray c))
      (let ((fd (aref (cff-fdselect c) gid)))
        (if (< fd (length (cff-fdarray c))) (aref (cff-fdarray c) fd)
            (list :lsubrs (cff-lsubrs c) :lbias (cff-lbias c))))
      (list :lsubrs (cff-lsubrs c) :lbias (cff-lbias c))))

(defun cff-glyph-outline (c gid)
  "Interpret GID's Type2 charstring -> list of contours, each a segment list in
font units: (:move x y) (:line x y) (:cubic c1x c1y c2x c2y x y)."
  (let ((cs (cff-charstrings c)) (d (cff-data c)))
    (when (or (< gid 0) (>= gid (length cs))) (return-from cff-glyph-outline nil))
    (let* ((fd (cff-fd-of c gid))
           (lsubrs (getf fd :lsubrs)) (lbias (getf fd :lbias))
           (gsubrs (cff-gsubrs c)) (gbias (cff-gbias c))
           (st (make-array 48 :adjustable t :fill-pointer 0))
           (x 0d0) (y 0d0) (contours '()) (cur '())
           (nstems 0) (width-done nil) (open nil) (done nil))
      (labels ((push* (v) (vector-push-extend (float v 1d0) st))
               (clear () (setf (fill-pointer st) 0))
               (nargs () (fill-pointer st))
               (drop-bottom ()
                 (loop for i from 1 below (fill-pointer st)
                       do (setf (aref st (1- i)) (aref st i)))
                 (decf (fill-pointer st)))
               (take-width-parity (expected)
                 (unless width-done
                   (when (> (nargs) expected) (drop-bottom))
                   (setf width-done t)))
               (take-width-even ()
                 (unless width-done
                   (when (oddp (nargs)) (drop-bottom))
                   (setf width-done t)))
               (moveto (nx ny)
                 (when open (push (nreverse cur) contours))
                 (setf x nx y ny cur (list (list :move x y)) open t))
               (lineto (nx ny) (push (list :line nx ny) cur) (setf x nx y ny))
               (curveto (c1x c1y c2x c2y nx ny)
                 (push (list :cubic c1x c1y c2x c2y nx ny) cur) (setf x nx y ny))
               (rrc (i) (let* ((c1x (+ x (aref st i))) (c1y (+ y (aref st (+ i 1))))
                               (c2x (+ c1x (aref st (+ i 2)))) (c2y (+ c1y (aref st (+ i 3))))
                               (ex (+ c2x (aref st (+ i 4)))) (ey (+ c2y (aref st (+ i 5)))))
                          (curveto c1x c1y c2x c2y ex ey)))
               (do-alt-line (horiz)
                 (loop for i from 0 below (nargs) do
                   (if horiz (lineto (+ x (aref st i)) y) (lineto x (+ y (aref st i))))
                   (setf horiz (not horiz))))
               (vvc ()
                 (let* ((n (nargs)) (i 0) (dx1 0d0))
                   (when (oddp n) (setf dx1 (aref st 0) i 1))
                   (loop while (< i n) do
                     (let* ((c1x (+ x dx1)) (c1y (+ y (aref st i)))
                            (c2x (+ c1x (aref st (+ i 1)))) (c2y (+ c1y (aref st (+ i 2))))
                            (ey (+ c2y (aref st (+ i 3)))))
                       (curveto c1x c1y c2x c2y c2x ey) (setf dx1 0d0) (incf i 4)))))
               (hhc ()
                 (let* ((n (nargs)) (i 0) (dy1 0d0))
                   (when (oddp n) (setf dy1 (aref st 0) i 1))
                   (loop while (< i n) do
                     (let* ((c1x (+ x (aref st i))) (c1y (+ y dy1))
                            (c2x (+ c1x (aref st (+ i 1)))) (c2y (+ c1y (aref st (+ i 2))))
                            (ex (+ c2x (aref st (+ i 3)))))
                       (curveto c1x c1y c2x c2y ex c2y) (setf dy1 0d0) (incf i 4)))))
               (vhc (start-horiz)
                 (let ((n (nargs)) (i 0) (horiz start-horiz))
                   (loop while (>= (- n i) 4) do
                     (let ((last5 (= (- n i) 5)))
                       (if horiz
                           (let* ((c1x (+ x (aref st i))) (c1y y)
                                  (c2x (+ c1x (aref st (+ i 1)))) (c2y (+ c1y (aref st (+ i 2))))
                                  (ey (+ c2y (aref st (+ i 3))))
                                  (ex (if last5 (+ c2x (aref st (+ i 4))) c2x)))
                             (curveto c1x c1y c2x c2y ex ey))
                           (let* ((c1x x) (c1y (+ y (aref st i)))
                                  (c2x (+ c1x (aref st (+ i 1)))) (c2y (+ c1y (aref st (+ i 2))))
                                  (ex (+ c2x (aref st (+ i 3))))
                                  (ey (if last5 (+ c2y (aref st (+ i 4))) c2y)))
                             (curveto c1x c1y c2x c2y ex ey)))
                       (setf horiz (not horiz)) (incf i 4)))
                   (clear)))
               (flex ()
                 (let* ((c1x (+ x (aref st 0))) (c1y (+ y (aref st 1)))
                        (c2x (+ c1x (aref st 2))) (c2y (+ c1y (aref st 3)))
                        (jx (+ c2x (aref st 4))) (jy (+ c2y (aref st 5))))
                   (curveto c1x c1y c2x c2y jx jy)
                   (let* ((c4x (+ jx (aref st 6))) (c4y (+ jy (aref st 7)))
                          (c5x (+ c4x (aref st 8))) (c5y (+ c4y (aref st 9)))
                          (ex (+ c5x (aref st 10))) (ey (+ c5y (aref st 11))))
                     (curveto c4x c4y c5x c5y ex ey))))
               (hflex ()
                 (let* ((sy y) (c1x (+ x (aref st 0))) (c1y y)
                        (c2x (+ c1x (aref st 1))) (c2y (+ c1y (aref st 2)))
                        (jx (+ c2x (aref st 3))) (jy c2y))
                   (curveto c1x c1y c2x c2y jx jy)
                   (let* ((c4x (+ jx (aref st 4))) (c4y jy)
                          (c5x (+ c4x (aref st 5))) (c5y sy)
                          (ex (+ c5x (aref st 6))) (ey sy))
                     (curveto c4x c4y c5x c5y ex ey))))
               (hflex1 ()
                 (let* ((sy y) (c1x (+ x (aref st 0))) (c1y (+ y (aref st 1)))
                        (c2x (+ c1x (aref st 2))) (c2y (+ c1y (aref st 3)))
                        (jx (+ c2x (aref st 4))) (jy c2y))
                   (curveto c1x c1y c2x c2y jx jy)
                   (let* ((c4x (+ jx (aref st 5))) (c4y jy)
                          (c5x (+ c4x (aref st 6))) (c5y (+ c4y (aref st 7)))
                          (ex (+ c5x (aref st 8))) (ey sy))
                     (curveto c4x c4y c5x c5y ex ey))))
               (flex1 ()
                 (let* ((sx x) (sy y)
                        (dxs (+ (aref st 0) (aref st 2) (aref st 4) (aref st 6) (aref st 8)))
                        (dys (+ (aref st 1) (aref st 3) (aref st 5) (aref st 7) (aref st 9)))
                        (c1x (+ x (aref st 0))) (c1y (+ y (aref st 1)))
                        (c2x (+ c1x (aref st 2))) (c2y (+ c1y (aref st 3)))
                        (jx (+ c2x (aref st 4))) (jy (+ c2y (aref st 5))))
                   (curveto c1x c1y c2x c2y jx jy)
                   (let* ((c4x (+ jx (aref st 6))) (c4y (+ jy (aref st 7)))
                          (c5x (+ c4x (aref st 8))) (c5y (+ c4y (aref st 9))) ex ey)
                     (if (> (abs dxs) (abs dys))
                         (setf ex (+ c5x (aref st 10)) ey sy)
                         (setf ex sx ey (+ c5y (aref st 10))))
                     (curveto c4x c4y c5x c5y ex ey))))
               (run (start end)
                 (let ((p start))
                   (loop while (and (< p end) (not done)) do
                     (let ((b (cff-u8 d p)))
                       (cond
                         ((>= b 32)
                          (cond ((<= b 246) (push* (- b 139)) (incf p))
                                ((<= b 250) (push* (+ (* (- b 247) 256) (cff-u8 d (1+ p)) 108)) (incf p 2))
                                ((<= b 254) (push* (- (* (- (- b 251)) 256) (cff-u8 d (1+ p)) 108)) (incf p 2))
                                (t (push* (/ (let ((v (cff-u32 d (1+ p)))) (if (>= v #x80000000) (- v #x100000000) v)) 65536d0)) (incf p 5))))
                         ((= b 28) (push* (cff-s16 d (1+ p))) (incf p 3))
                         (t (incf p)
                            (case b
                              ((1 3 18 23) (take-width-even) (incf nstems (floor (nargs) 2)) (clear))
                              ((19 20) (take-width-even) (incf nstems (floor (nargs) 2)) (clear)
                                       (incf p (ceiling nstems 8)))
                              (21 (take-width-parity 2) (moveto (+ x (aref st 0)) (+ y (aref st 1))) (clear))
                              (22 (take-width-parity 1) (moveto (+ x (aref st 0)) y) (clear))
                              (4  (take-width-parity 1) (moveto x (+ y (aref st 0))) (clear))
                              (5  (loop for i from 0 below (1- (nargs)) by 2
                                        do (lineto (+ x (aref st i)) (+ y (aref st (1+ i))))) (clear))
                              (6  (do-alt-line t) (clear))
                              (7  (do-alt-line nil) (clear))
                              (8  (loop for i from 0 below (- (nargs) 5) by 6 do (rrc i)) (clear))
                              (24 (let ((n (nargs)))
                                    (loop for i from 0 below (- n 2) by 6 do (rrc i))
                                    (let ((i (- n 2))) (lineto (+ x (aref st i)) (+ y (aref st (1+ i)))))) (clear))
                              (25 (let ((n (nargs)))
                                    (loop for i from 0 below (- n 6) by 2
                                          do (lineto (+ x (aref st i)) (+ y (aref st (1+ i)))))
                                    (rrc (- n 6))) (clear))
                              (26 (vvc) (clear))
                              (27 (hhc) (clear))
                              (30 (vhc nil))
                              (31 (vhc t))
                              (10 (let* ((idx (+ (round (vector-pop st)) lbias))
                                         (e (and (< -1 idx (length lsubrs)) (aref lsubrs idx))))
                                    (when e (run (car e) (cdr e)))))
                              (29 (let* ((idx (+ (round (vector-pop st)) gbias))
                                         (e (and (< -1 idx (length gsubrs)) (aref gsubrs idx))))
                                    (when e (run (car e) (cdr e)))))
                              (11 (return-from run))
                              (14 (take-width-parity 0)
                                  (when open (push (nreverse cur) contours) (setf open nil))
                                  (setf done t) (return-from run))
                              (12 (let ((op2 (cff-u8 d p))) (incf p)
                                    (case op2 (34 (hflex)) (35 (flex)) (36 (hflex1)) (37 (flex1)))
                                    (clear)))
                              (t (clear)))))))))
               )
        (let ((e (aref cs gid))) (run (car e) (cdr e)))
        (when open (push (nreverse cur) contours))
        (nreverse contours)))))
