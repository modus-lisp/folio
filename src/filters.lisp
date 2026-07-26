;;;; src/filters.lisp — stream filter (decode) chain (ISO 32000-1 §7.4).
;;;;
;;;; decode-stream applies the /Filter chain with /DecodeParms.  Implemented:
;;;; FlateDecode (+ PNG/TIFF predictors), LZWDecode (+ predictors),
;;;; ASCIIHexDecode, ASCII85Decode, RunLengthDecode.  Image-only filters
;;;; (DCTDecode / CCITTFax / JBIG2 / JPXDecode) are left undecoded — their raw
;;;; bytes are handed back for the future image module.

(in-package #:folio)

(defparameter *image-filters*
  '("DCTDecode" "DCT" "CCITTFaxDecode" "CCF" "JBIG2Decode" "JPXDecode")
  "Filters folio leaves undecoded in M0 (compressed image data).")

(defun %as-list (x)
  "Normalize a name-or-array value into a Lisp list of elements."
  (cond ((null x) nil)
        ((pdf-array-p x) (coerce x 'list))
        (t (list x))))

(defun %filter-name (x)
  (cond ((pdf-name-p x) (pdf-name-string x))
        ((stringp x) x)
        (t nil)))

(defun decode-stream (stream &key resolver)
  "Decode a PDF-STREAM through its /Filter chain, honouring /DecodeParms.
RESOLVER (a function of a value -> resolved value) is used for indirect
/Filter, /DecodeParms and /Length entries.  Returns (values bytes filters-left)
where FILTERS-LEFT is the list of trailing image filters not applied (NIL when
fully decoded)."
  (let* ((res (or resolver #'identity))
         (dict (pdf-stream-dict stream))
         (data (pdf-stream-raw stream))
         (filters (mapcar #'%filter-name
                          (%as-list (funcall res (or (dict-get dict "Filter")
                                                     (dict-get dict "F"))))))
         (parms (%as-list (funcall res (or (dict-get dict "DecodeParms")
                                           (dict-get dict "DP"))))))
    (loop for i from 0
          for f in filters
          for p = (funcall res (nth i parms))
          do (cond ((null f) nil)
                   ((member f *image-filters* :test #'string=)
                    ;; stop: remaining filters are image codecs
                    (return-from decode-stream
                      (values data (nthcdr i filters))))
                   (t (setf data (apply-filter f data p :resolver res)))))
    (values data nil)))

(defun apply-filter (name data parms &key resolver)
  "Apply a single named filter to DATA (octet vector).  PARMS is the matching
DecodeParms dict (or NIL)."
  (let ((res (or resolver #'identity)))
    (cond
      ((or (string= name "FlateDecode") (string= name "Fl"))
       (predict (flate-decode data) parms res))
      ((or (string= name "LZWDecode") (string= name "LZW"))
       (predict (lzw-decode data (let ((e (and parms (dict-get parms "EarlyChange"))))
                                    (if (integerp e) e 1)))
                parms res))
      ((or (string= name "ASCIIHexDecode") (string= name "AHx"))
       (ascii-hex-decode data))
      ((or (string= name "ASCII85Decode") (string= name "A85"))
       (ascii85-decode data))
      ((or (string= name "RunLengthDecode") (string= name "RL"))
       (run-length-decode data))
      (t data))))                       ; unknown/identity: pass through

;;; ---- FlateDecode -------------------------------------------------------

(defun flate-decode (data)
  "zlib/DEFLATE inflate via cram; tolerate a raw (headerless) DEFLATE stream
and a few bytes of leading garbage."
  (or (ignore-errors (values (cram:zlib-decompress data)))
      (ignore-errors (values (cram:deflate-decompress data)))
      ;; some producers pad a byte or two before the zlib header
      (loop for skip in '(1 2)
            thereis (ignore-errors
                      (values (cram:zlib-decompress (subseq data skip)))))
      (make-array 0 :element-type '(unsigned-byte 8))))

;;; ---- predictors (PNG & TIFF, §7.4.4.4) --------------------------------

(defun predict (data parms res)
  "Apply the /Predictor post-filter described by PARMS to DATA."
  (if (not (hash-table-p parms))
      data
      (let* ((pred    (let ((v (funcall res (dict-get parms "Predictor")))) (if (integerp v) v 1)))
             (colors  (let ((v (funcall res (dict-get parms "Colors")))) (if (integerp v) v 1)))
             (bpc     (let ((v (funcall res (dict-get parms "BitsPerComponent")))) (if (integerp v) v 8)))
             (columns (let ((v (funcall res (dict-get parms "Columns")))) (if (integerp v) v 1))))
        (cond ((<= pred 1) data)
              ((= pred 2) (tiff-predict-2 data colors bpc columns))
              ((>= pred 10) (png-unpredict data colors bpc columns))
              (t data)))))

(defun %row-bytes (colors bpc columns)
  (values (ceiling (* colors bpc columns) 8)          ; bytes per row (data)
          (max 1 (ceiling (* colors bpc) 8))))        ; bytes per pixel

(defun png-unpredict (data colors bpc columns)
  "Undo PNG predictors 10-15.  Each row is prefixed by a filter-type byte."
  (multiple-value-bind (rowlen bpp) (%row-bytes colors bpc columns)
    (let* ((stride (1+ rowlen))
           (nrows (floor (length data) stride))
           (out (make-array (* nrows rowlen) :element-type '(unsigned-byte 8)))
           (prev (make-array rowlen :element-type '(unsigned-byte 8) :initial-element 0)))
      (dotimes (r nrows)
        (let ((ft (aref data (* r stride)))
              (cur (make-array rowlen :element-type '(unsigned-byte 8))))
          (dotimes (i rowlen)
            (let* ((x (aref data (+ (* r stride) 1 i)))
                   (a (if (>= (- i bpp) 0) (aref cur (- i bpp)) 0))
                   (b (aref prev i))
                   (c (if (>= (- i bpp) 0) (aref prev (- i bpp)) 0))
                   (v (ecase ft
                        (0 x)
                        (1 (+ x a))
                        (2 (+ x b))
                        (3 (+ x (floor (+ a b) 2)))
                        (4 (+ x (paeth a b c))))))
              (setf (aref cur i) (logand v #xff))))
          (replace out cur :start1 (* r rowlen))
          (setf prev cur)))
      out)))

(defun paeth (a b c)
  (let* ((p (- (+ a b) c))
         (pa (abs (- p a))) (pb (abs (- p b))) (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a)
          ((<= pb pc) b)
          (t c))))

(defun tiff-predict-2 (data colors bpc columns)
  "TIFF Predictor 2: horizontal differencing (8-bit component path)."
  (if (/= bpc 8)
      data                              ; only the common 8-bit case in M0
      (multiple-value-bind (rowlen bpp) (%row-bytes colors bpc columns)
        (declare (ignore bpp))
        (let ((out (copy-seq data))
              (nrows (floor (length data) rowlen)))
          (dotimes (r nrows)
            (loop for i from colors below rowlen
                  for idx = (+ (* r rowlen) i)
                  do (setf (aref out idx)
                           (logand (+ (aref out idx) (aref out (- idx colors))) #xff))))
          out))))

;;; ---- ASCIIHexDecode (§7.4.2) ------------------------------------------

(defun ascii-hex-decode (data)
  (let ((out (make-array (ceiling (length data) 2)
                         :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (hi nil))
    (loop for b across data
          do (cond ((= b 62) (return))               ; '>' EOD
                   ((ws-byte-p b))
                   ((hex-byte-p b)
                    (let ((v (hex-val b)))
                      (if hi (progn (vector-push-extend (+ (* 16 hi) v) out) (setf hi nil))
                          (setf hi v))))))
    (when hi (vector-push-extend (* 16 hi) out))
    (subseq-octets out)))

;;; ---- ASCII85Decode (§7.4.3) -------------------------------------------

(defun ascii85-decode (data)
  (let ((out (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (tuple 0) (count 0) (i 0) (n (length data)))
    ;; optional leading "<~"
    (when (and (>= n 2) (= (aref data 0) 60) (= (aref data 1) 126)) (setf i 2))
    (loop while (< i n)
          for b = (aref data i)
          do (incf i)
             (cond ((= b 126) (return))                ; '~' -> EOD ("~>")
                   ((ws-byte-p b))
                   ((and (= b 122) (= count 0))        ; 'z' shorthand
                    (dotimes (k 4) (vector-push-extend 0 out)))
                   ((<= 33 b 117)                       ; '!'..'u'
                    (setf tuple (+ (* tuple 85) (- b 33)))
                    (incf count)
                    (when (= count 5)
                      (vector-push-extend (logand (ash tuple -24) #xff) out)
                      (vector-push-extend (logand (ash tuple -16) #xff) out)
                      (vector-push-extend (logand (ash tuple -8) #xff) out)
                      (vector-push-extend (logand tuple #xff) out)
                      (setf tuple 0 count 0)))))
    (when (> count 1)                                   ; flush partial group
      (dotimes (k (- 5 count)) (setf tuple (+ (* tuple 85) 84)))
      (dotimes (k (1- count))
        (vector-push-extend (logand (ash tuple (* -8 (- 3 k))) #xff) out)))
    (subseq-octets out)))

;;; ---- RunLengthDecode (§7.4.5) -----------------------------------------

(defun run-length-decode (data)
  (let ((out (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (i 0) (n (length data)))
    (loop while (< i n)
          for len = (aref data i)
          do (incf i)
             (cond ((= len 128) (return))              ; EOD
                   ((< len 128)                         ; copy len+1 literally
                    (dotimes (k (1+ len))
                      (when (< i n) (vector-push-extend (aref data i) out) (incf i))))
                   (t                                   ; repeat next byte 257-len
                    (when (< i n)
                      (let ((b (aref data i)))
                        (incf i)
                        (dotimes (k (- 257 len)) (vector-push-extend b out)))))))
    (subseq-octets out)))

;;; ---- LZWDecode (§7.4.4.2) ---------------------------------------------

(defun lzw-decode (data early-change)
  "Variable-width (9-12 bit) LZW as used in PDF.  Clear=256, EOD=257."
  (let ((out (make-array 256 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (dict (make-array 4096 :initial-element nil))
        (next 258) (width 9)
        (bitbuf 0) (bitcnt 0) (prev nil))
    (flet ((reset-dict ()
             (setf next 258 width 9)
             (dotimes (k 256) (setf (aref dict k) (vector k)))))
      (reset-dict)
      (loop for b across data do
        (setf bitbuf (logior (ash bitbuf 8) b))
        (incf bitcnt 8)
        (loop while (>= bitcnt width) do
          (decf bitcnt width)
          (let ((code (logand (ash bitbuf (- bitcnt)) (1- (ash 1 width)))))
            (cond
              ((= code 256) (reset-dict) (setf prev nil))
              ((= code 257) (return-from lzw-decode (subseq-octets out)))
              (t
               (let ((entry
                       (cond ((and (< code next) (aref dict code)) (aref dict code))
                             ((and prev (= code next))
                              (concatenate 'vector prev (vector (aref prev 0))))
                             (t nil))))
                 (when entry
                   (loop for v across entry do (vector-push-extend v out))
                   (when (and prev (< next 4096))
                     (setf (aref dict next)
                           (concatenate 'vector prev (vector (aref entry 0))))
                     (incf next))
                   (setf prev entry)
                   ;; widen code just before the table fills (EarlyChange=1)
                   (when (and (>= (+ next early-change) (ash 1 width)) (< width 12))
                     (incf width))))))))))
    (subseq-octets out)))
