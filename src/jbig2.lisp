;;;; src/jbig2.lisp — JBIG2 image codec (ISO/IEC 14492) for the /JBIG2Decode filter.
;;;;
;;;; Decodes the bilevel image data produced by the PDF JBIG2Decode filter
;;;; (PDF §7.4.7) into a packed 1-bit-per-pixel DeviceGray sample buffer that the
;;;; ordinary BPC-1 image path (`decode-image-to-rgba`) consumes exactly like a
;;;; Flate- or CCITT-decoded 1-bit image.
;;;;
;;;; PDF embeds JBIG2 in the "embedded organisation" (Annex D.2): NO file header,
;;;; sequential segments only.  A /DecodeParms /JBIG2Globals stream may carry
;;;; shared segments (symbol dictionaries) processed before the image's segments.
;;;;
;;;; Implemented: segment-header parsing; the MQ arithmetic decoder (Annex E, the
;;;; JBIG2/JPEG2000 QM-coder); generic region decoding (§6.2, arithmetic GB
;;;; templates 0-3 with adaptive AT pixels and TPGDON typical prediction, plus the
;;;; MMR=1 path reusing the T.6/G4 decoder in ccitt.lisp); generic refinement
;;;; region decoding (§6.3); arithmetic symbol dictionaries (§6.5) and text
;;;; regions (§6.4) with arithmetic integer decoding (IADH/IADW/IAEX/IAID/…);
;;;; page information (§7.4.8) and page composition of immediate generic/text
;;;; regions (combination operators OR/XOR).
;;;;
;;;; Deferred (noted as gaps): Huffman-coded symbol/text regions, halftone
;;;; regions (§6.6) + pattern dictionaries, standalone refinement-region segments.
;;;; These are rare in scanned PDFs, which are dominated by arithmetic generic +
;;;; symbol/text regions.
;;;;
;;;; Output polarity (verified vs pdfium): in JBIG2 a pixel value of 1 is BLACK
;;;; (foreground) — the opposite of the PDF DeviceGray convention.  We therefore
;;;; INVERT when packing: a black JBIG2 pixel becomes sample bit 0 (renders black
;;;; on a BPC-1 DeviceGray read) and a white pixel becomes bit 1.

(in-package #:folio)

;;; ===========================================================================
;;; MQ arithmetic decoder (Annex E / ITU-T T.88 — the QM-coder, C.3 of JPEG2000)
;;; Ported from the well-tested pdf.js ArithmeticDecoder (Apache-2.0).
;;; ===========================================================================

(defparameter +jb-qe+
  (coerce '(#x5601 #x3401 #x1801 #x0ac1 #x0521 #x0221 #x5601 #x5401 #x4801 #x3801
            #x3001 #x2401 #x1c01 #x1601 #x5601 #x5401 #x5101 #x4801 #x3801 #x3401
            #x3001 #x2801 #x2401 #x2201 #x1c01 #x1801 #x1601 #x1401 #x1201 #x1101
            #x0ac1 #x09c1 #x08a1 #x0521 #x0441 #x02a1 #x0221 #x0141 #x0111 #x0085
            #x0049 #x0025 #x0015 #x0009 #x0005 #x0001 #x5601)
          '(simple-array (unsigned-byte 16) (*))))

(defparameter +jb-nmps+
  (coerce '(1 2 3 4 5 38 7 8 9 10 11 12 13 29 15 16 17 18 19 20 21 22 23 24 25 26
            27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 45 46)
          '(simple-array (unsigned-byte 8) (*))))

(defparameter +jb-nlps+
  (coerce '(1 6 9 12 29 33 6 14 14 14 17 18 20 21 14 14 15 16 17 18 19 19 20 21 22
            23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 46)
          '(simple-array (unsigned-byte 8) (*))))

(defparameter +jb-switch+
  (coerce '(1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0 0 0)
          '(simple-array (unsigned-byte 8) (*))))

(defstruct (mq (:constructor %make-mq))
  (data (make-array 0 :element-type '(unsigned-byte 8))
        :type (simple-array (unsigned-byte 8) (*)))
  (len 0 :type fixnum)
  (bp 0 :type fixnum)
  (end 0 :type fixnum)
  (chigh 0 :type fixnum)
  (clow 0 :type fixnum)
  (a 0 :type fixnum)
  (ct 0 :type fixnum))

(declaim (inline mq-byte))
(defun mq-byte (mq i)
  (declare (fixnum i))
  (if (< i (mq-len mq)) (aref (mq-data mq) i) #xff))

(defun mq-byte-in (mq)
  (declare (optimize (speed 3) (safety 1)))
  (let ((bp (mq-bp mq)))
    (declare (fixnum bp))
    (cond
      ((= (mq-byte mq bp) #xff)
       (if (> (mq-byte mq (1+ bp)) #x8f)
           (progn (incf (mq-clow mq) #xff00) (setf (mq-ct mq) 8))
           (progn (incf bp)
                  (incf (mq-clow mq) (ash (mq-byte mq bp) 9))
                  (setf (mq-ct mq) 7 (mq-bp mq) bp))))
      (t
       (incf bp)
       (incf (mq-clow mq) (if (< bp (mq-end mq)) (ash (mq-byte mq bp) 8) #xff00))
       (setf (mq-ct mq) 8 (mq-bp mq) bp)))
    (when (> (mq-clow mq) #xffff)
      (incf (mq-chigh mq) (ash (mq-clow mq) -16))
      (setf (mq-clow mq) (logand (mq-clow mq) #xffff)))))

(defun mq-init (data start end)
  (let ((mq (%make-mq :data data :len (length data) :end end :bp start)))
    (setf (mq-chigh mq) (mq-byte mq start)
          (mq-clow mq) 0)
    (mq-byte-in mq)
    (setf (mq-chigh mq) (logior (logand (ash (mq-chigh mq) 7) #xffff)
                                (logand (ash (mq-clow mq) -9) #x7f)))
    (setf (mq-clow mq) (logand (ash (mq-clow mq) 7) #xffff))
    (decf (mq-ct mq) 7)
    (setf (mq-a mq) #x8000)
    mq))

(defun mq-decode (mq contexts pos)
  "DECODE one binary decision using context CONTEXTS[POS]; returns 0/1."
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 8) (*)) contexts)
           (fixnum pos))
  (let* ((cx (aref contexts pos))
         (i (ash cx -1))
         (mps (logand cx 1))
         (qe (aref +jb-qe+ i))
         (a (- (mq-a mq) qe))
         (d 0))
    (declare (fixnum cx i mps qe a d))
    (cond
      ((< (mq-chigh mq) qe)
       ;; LPS exchange
       (if (< a qe)
           (setf a qe d mps i (aref +jb-nmps+ i))
           (progn (setf a qe d (logxor 1 mps))
                  (when (= 1 (aref +jb-switch+ i)) (setf mps d))
                  (setf i (aref +jb-nlps+ i)))))
      (t
       (setf (mq-chigh mq) (- (mq-chigh mq) qe))
       (if (/= 0 (logand a #x8000))
           (progn (setf (mq-a mq) a)
                  (return-from mq-decode mps))
           ;; MPS exchange
           (if (< a qe)
               (progn (setf d (logxor 1 mps))
                      (when (= 1 (aref +jb-switch+ i)) (setf mps d))
                      (setf i (aref +jb-nlps+ i)))
               (setf d mps i (aref +jb-nmps+ i))))))
    ;; RENORMD
    (loop
      (when (= (mq-ct mq) 0) (mq-byte-in mq))
      (setf a (ash a 1))
      (setf (mq-chigh mq)
            (logior (logand (ash (mq-chigh mq) 1) #xffff)
                    (logand (ash (mq-clow mq) -15) 1)))
      (setf (mq-clow mq) (logand (ash (mq-clow mq) 1) #xffff))
      (decf (mq-ct mq))
      (when (/= 0 (logand a #x8000)) (return)))
    (setf (mq-a mq) a)
    (setf (aref contexts pos) (logior (ash i 1) mps))
    d))

;;; ===========================================================================
;;; Decoding context: the MQ decoder + a lazily-allocated context-array cache.
;;; ===========================================================================

(defstruct (dc (:constructor %make-dc))
  data
  (start 0 :type fixnum)
  (end 0 :type fixnum)
  mq
  (cache (make-hash-table :test 'eq)))

(defun make-dc (data start end)
  (%make-dc :data data :start start :end end :mq (mq-init data start end)))

(defun dc-ctx (dc key size)
  "Fetch (creating on first use) the context-state array for KEY."
  (or (gethash key (dc-cache dc))
      (setf (gethash key (dc-cache dc))
            (make-array size :element-type '(unsigned-byte 8) :initial-element 0))))

;;; ---- Annex A: arithmetic integer decoding ---------------------------------

(defun dc-int (dc proc)
  "Decode an integer with procedure PROC (a keyword).  Returns an INTEGER, or the
keyword :OOB for the out-of-band value."
  (let ((contexts (dc-ctx dc proc 512))
        (mq (dc-mq dc))
        (prev 1))
    (declare (fixnum prev))
    (flet ((rb (len)
             (declare (fixnum len))
             (let ((v 0))
               (declare (integer v))
               (dotimes (i len)
                 (let ((bit (mq-decode mq contexts prev)))
                   (setf prev (if (< prev 256)
                                  (logior (ash prev 1) bit)
                                  (logior (logand (logior (ash prev 1) bit) 511) 256)))
                   (setf v (logior (ash v 1) bit))))
               v)))
      (let* ((sign (rb 1))
             (value (if (= 1 (rb 1))
                        (if (= 1 (rb 1))
                            (if (= 1 (rb 1))
                                (if (= 1 (rb 1))
                                    (if (= 1 (rb 1))
                                        (+ (rb 32) 4436)
                                        (+ (rb 12) 340))
                                    (+ (rb 8) 84))
                                (+ (rb 6) 20))
                            (+ (rb 4) 4))
                        (rb 2))))
        (cond ((= sign 0) value)
              ((> value 0) (- value))
              (t :oob))))))

(defun dc-iaid (dc code-len)
  "Decode a symbol ID of CODE-LEN bits (Annex A.3)."
  (declare (fixnum code-len))
  (let ((contexts (dc-ctx dc :iaid (ash 1 (1+ (max 0 code-len)))))
        (mq (dc-mq dc))
        (prev 1))
    (declare (fixnum prev))
    (dotimes (i code-len)
      (setf prev (logior (ash prev 1) (mq-decode mq contexts prev))))
    (logand prev (1- (ash 1 code-len)))))

(defun jb-log2 (x)
  "Ceil(log2 x) with log2(1)=0, matching the JBIG2 SBSYMCODELEN convention."
  (if (<= x 1)
      0
      (let ((n 1) (i 0))
        (loop while (> x n) do (setf n (ash n 1)) (incf i))
        i)))

;;; ===========================================================================
;;; Bilevel bitmap (one byte per pixel; value 1 = black/foreground).
;;; ===========================================================================

(defstruct (jb (:constructor %make-jb))
  (w 0 :type fixnum)
  (h 0 :type fixnum)
  (bits (make-array 0 :element-type '(unsigned-byte 8))
        :type (simple-array (unsigned-byte 8) (*))))

(defun make-jb (w h &optional (fill 0))
  (let ((w (max 0 w)) (h (max 0 h)))
    (%make-jb :w w :h h
              :bits (make-array (* w h) :element-type '(unsigned-byte 8)
                                        :initial-element fill))))

(declaim (inline jb-get jb-set))
(defun jb-get (b x y)
  (declare (fixnum x y))
  (aref (jb-bits b) (+ (the fixnum (* y (jb-w b))) x)))
(defun jb-set (b x y v)
  (declare (fixnum x y v))
  (setf (aref (jb-bits b) (+ (the fixnum (* y (jb-w b))) x)) v))

;;; ===========================================================================
;;; Generic region decoding (§6.2)
;;; ===========================================================================

;;; Coding templates 0-3: lists of (x . y) neighbour offsets (§6.2.5.3).
(defparameter +jb-coding-templates+
  (vector
   '((-1 . -2) (0 . -2) (1 . -2)
     (-2 . -1) (-1 . -1) (0 . -1) (1 . -1) (2 . -1)
     (-4 . 0) (-3 . 0) (-2 . 0) (-1 . 0))
   '((-1 . -2) (0 . -2) (1 . -2) (2 . -2)
     (-2 . -1) (-1 . -1) (0 . -1) (1 . -1) (2 . -1)
     (-3 . 0) (-2 . 0) (-1 . 0))
   '((-1 . -2) (0 . -2) (1 . -2)
     (-2 . -1) (-1 . -1) (0 . -1) (1 . -1)
     (-2 . 0) (-1 . 0))
   '((-3 . -1) (-2 . -1) (-1 . -1) (0 . -1) (1 . -1)
     (-4 . 0) (-3 . 0) (-2 . 0) (-1 . 0))))

;;; TPGDON pseudo-pixel contexts, §6.2.5.7 (matched to the sorted template order).
(defparameter +jb-reused-contexts+ #(#x9b25 #x0795 #x00e5 #x0195))

(defun jbig2-decode-bitmap (dc mmr width height template prediction at)
  "Decode a generic region (§6.2) into a JB bitmap.  When MMR, reuse the T.6/G4
decoder; otherwise arithmetic-decode with the GB templates + optional TPGDON."
  (declare (fixnum width height template))
  (when (or (<= width 0) (<= height 0)) (return-from jbig2-decode-bitmap (make-jb (max 0 width) (max 0 height))))
  (if mmr
      (jbig2-mmr-bitmap (dc-data dc) (dc-start dc) (dc-end dc) width height)
      (let* ((mq (dc-mq dc))
             (contexts (dc-ctx dc :gb (ash 1 16)))
             ;; template + adaptive AT pixels, sorted by (y, x) — matches the
             ;; canonical order the TPGDON pseudo-pixel constants assume.
             (tmpl (sort (append (copy-list (aref +jb-coding-templates+ template))
                                 (copy-list at))
                         (lambda (p q)
                           (or (< (cdr p) (cdr q))
                               (and (= (cdr p) (cdr q)) (< (car p) (car q)))))))
             (tlen (length tmpl))
             (tx (make-array tlen :element-type 'fixnum))
             (ty (make-array tlen :element-type 'fixnum))
             (pseudo (if (< template 4) (aref +jb-reused-contexts+ template) 0))
             (b (make-jb width height))
             (ltp 0))
        (declare (fixnum tlen pseudo ltp))
        (loop for k below tlen for p in tmpl
              do (setf (aref tx k) (car p) (aref ty k) (cdr p)))
        (dotimes (i height)
          (declare (fixnum i))
          (when prediction
            (setf ltp (logxor ltp (mq-decode mq contexts pseudo))))
          (if (and prediction (= ltp 1))
              (when (> i 0)                      ; duplicate the previous row
                (dotimes (x width) (jb-set b x i (jb-get b x (1- i)))))
              (dotimes (j width)
                (declare (fixnum j))
                (let ((cl 0) (shift (1- tlen)))
                  (declare (fixnum cl shift))
                  (dotimes (k tlen)
                    (let ((j0 (+ j (aref tx k))))
                      (declare (fixnum j0))
                      (when (and (>= j0 0) (< j0 width))
                        (let ((i0 (+ i (aref ty k))))
                          (declare (fixnum i0))
                          (when (and (>= i0 0) (= 1 (jb-get b j0 i0)))
                            (setf cl (logior cl (ash 1 shift)))))))
                    (decf shift))
                  (jb-set b j i (mq-decode mq contexts cl))))))
        b)))

(defun jbig2-mmr-bitmap (data start end width height)
  "Decode an MMR (T.6 / Group-4) generic region by reusing ccitt-decode, then
unpack to a JB bitmap (1 = black)."
  (declare (fixnum start end width height))
  (let* ((sub (subseq data start (min end (length data))))
         (packed (ccitt-decode sub :columns width :rows height :k -1 :black-is-1 t))
         (b (make-jb width height))
         (rb (ceiling width 8)))
    (declare (type (simple-array (unsigned-byte 8) (*)) packed))
    (dotimes (y height)
      (let ((base (* y rb)))
        (dotimes (x width)
          (when (< (+ base (ash x -3)) (length packed))
            (jb-set b x y (logand (ash (aref packed (+ base (ash x -3)))
                                       (- (- 7 (logand x 7)))) 1))))))
    b))

;;; ===========================================================================
;;; Generic refinement region decoding (§6.3)
;;; ===========================================================================

(defparameter +jb-refinement-coding+
  (vector '((0 . -1) (1 . -1) (-1 . 0))
          '((-1 . -1) (0 . -1) (1 . -1) (-1 . 0))))
(defparameter +jb-refinement-reference+
  (vector '((0 . -1) (1 . -1) (-1 . 0) (0 . 0) (1 . 0) (-1 . 1) (0 . 1) (1 . 1))
          '((0 . -1) (-1 . 0) (0 . 0) (1 . 0) (0 . 1) (1 . 1))))
(defparameter +jb-refinement-reused+ #(#x0020 #x0008))

(defun jbig2-decode-refinement (dc width height template ref offx offy at
                                &optional tpgron)
  "Decode a generic refinement region (§6.3) against reference bitmap REF.  When
TPGRON, honour typical-prediction (a uniform 3x3 reference neighbourhood copies
through without an arithmetic-coded pixel)."
  (declare (fixnum width height template offx offy))
  (let* ((mq (dc-mq dc))
         (contexts (dc-ctx dc :gr (ash 1 16)))
         (coding (append (copy-list (aref +jb-refinement-coding+ template))
                         (if (= template 0) (list (first at)) nil)))
         (reference (append (copy-list (aref +jb-refinement-reference+ template))
                            (if (= template 0) (list (second at)) nil)))
         (rw (jb-w ref)) (rh (jb-h ref))
         (pseudo (aref +jb-refinement-reused+ template))
         (b (make-jb width height))
         (ltp 0))
    (declare (fixnum rw rh pseudo ltp))
    (flet ((refpix (x y) (if (or (< x 0) (>= x rw) (< y 0) (>= y rh)) 0 (jb-get ref x y))))
      (dotimes (i height)
        (declare (fixnum i))
        (when tpgron
          (setf ltp (logxor ltp (mq-decode mq contexts pseudo))))
        (dotimes (j width)
          (declare (fixnum j))
          (let ((rx (- j offx)) (ry (- i offy)))
            (declare (fixnum rx ry))
            (if (and tpgron (= ltp 1)
                     ;; uniform 3x3 reference neighbourhood -> copy it through.
                     (let ((c (refpix (1- rx) (1- ry))) (uniform t))
                       (loop for dy from -1 to 1 while uniform do
                         (loop for dx from -1 to 1 while uniform do
                           (when (/= (refpix (+ rx dx) (+ ry dy)) c) (setf uniform nil))))
                       (and uniform (progn (jb-set b j i (refpix rx ry)) t))))
                nil                          ; predicted: already set
                (let ((cl 0))
                  (declare (fixnum cl))
                  (dolist (p coding)
                    (let ((i0 (+ i (cdr p))) (j0 (+ j (car p))))
                      (setf cl (if (or (< i0 0) (< j0 0) (>= j0 width))
                                   (ash cl 1)
                                   (logior (ash cl 1) (jb-get b j0 i0))))))
                  (dolist (p reference)
                    (let ((i0 (- (+ i (cdr p)) offy)) (j0 (- (+ j (car p)) offx)))
                      (setf cl (if (or (< i0 0) (>= i0 rh) (< j0 0) (>= j0 rw))
                                   (ash cl 1)
                                   (logior (ash cl 1) (jb-get ref j0 i0))))))
                  (jb-set b j i (mq-decode mq contexts cl))))))))
    b))

(defun jb-subregion (page x y w h)
  "Extract the W×H sub-bitmap of PAGE at (X,Y) (out-of-bounds -> 0)."
  (declare (fixnum x y w h))
  (let ((b (make-jb w h)) (pw (jb-w page)) (ph (jb-h page)))
    (declare (fixnum pw ph))
    (dotimes (i h)
      (dotimes (j w)
        (let ((px (+ x j)) (py (+ y i)))
          (when (and (>= px 0) (< px pw) (>= py 0) (< py ph))
            (jb-set b j i (jb-get page px py))))))
    b))

;;; ===========================================================================
;;; Symbol dictionary (§6.5) + text region (§6.4) — arithmetic variant.
;;; ===========================================================================

(defun jbig2-decode-symbol-dict (dc refinement input-symbols num-new num-exported
                                 template at ref-template ref-at)
  "Decode a symbol dictionary (§6.5).  INPUT-SYMBOLS is a vector of imported JBs.
Returns a vector of EXPORTED JBs.  Arithmetic (non-Huffman) coding only."
  (declare (fixnum num-new num-exported template))
  (let* ((mq (dc-mq dc))
         (new (make-array 0 :adjustable t :fill-pointer 0))
         (n-in (length input-symbols))
         (code-len (jb-log2 (+ n-in num-new)))
         (current-height 0)
         (guard 0))
    (declare (fixnum n-in code-len current-height guard))
    (declare (ignorable mq num-exported))
    (flet ((sym-at (id)
             (if (< id n-in) (aref input-symbols id)
                 (let ((k (- id n-in)))
                   (if (< k (fill-pointer new)) (aref new k) (make-jb 1 1))))))
      (when (> num-new 1000000) (return-from jbig2-decode-symbol-dict #()))
      (loop while (and (< (fill-pointer new) num-new) (< (incf guard) (+ num-new 64))) do
        (let ((dh (dc-int dc :iadh)))
          (when (eq dh :oob) (return))
          (incf current-height dh)
          (when (or (<= current-height 0) (> current-height 8192)) (return))
          (let ((current-width 0) (wguard 0))
            (declare (fixnum current-width wguard))
            (loop
              (when (> (incf wguard) (+ num-new 64)) (return))
              (let ((dw (dc-int dc :iadw)))
                (when (eq dw :oob) (return))     ; height class done
                ;; Stop cleanly once all symbols are decoded: the next non-OOB
                ;; IADW indicates a corrupt/unsupported stream (e.g. the
                ;; context-retention feature) — bail rather than run away.
                (when (>= (fill-pointer new) num-new) (return))
                (incf current-width dw)
                (when (or (<= current-width 0) (> current-width 8192)) (return))
                (let ((bmp
                        (if refinement
                            (let ((ninst (dc-int dc :iaai)))
                              (if (and (integerp ninst) (> ninst 1))
                                  (jbig2-decode-text-region
                                   dc t current-width current-height 0 ninst 1
                                   (jbig2-combined-symbols input-symbols new)
                                   code-len 0 0 1 0 ref-template ref-at 0)
                                  (let* ((sid (dc-iaid dc code-len))
                                         (rdx (dc-int dc :iardx))
                                         (rdy (dc-int dc :iardy)))
                                    (jbig2-decode-refinement
                                     dc current-width current-height ref-template
                                     (sym-at sid)
                                     (if (integerp rdx) rdx 0)
                                     (if (integerp rdy) rdy 0)
                                     ref-at))))
                            (jbig2-decode-bitmap dc nil current-width current-height
                                                 template nil at))))
                  (vector-push-extend bmp new)))))))
      ;; §6.5.10 exported-symbol flags via IAEX run lengths.
      (let* ((total (+ n-in num-new))
             (flags (make-array total :initial-element nil))
             (idx 0) (cur nil) (eguard 0))
        (declare (fixnum idx eguard))
        (loop while (and (< idx total) (< (incf eguard) (+ total 100000))) do
          (let ((run (dc-int dc :iaex)))
            (unless (integerp run) (return))
            (when (< run 0) (return))
            (dotimes (k run)
              (when (< idx total) (setf (aref flags idx) cur) (incf idx)))
            (setf cur (not cur))))
        (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
          (dotimes (i total)
            (when (aref flags i)
              (vector-push-extend
               (if (< i n-in) (aref input-symbols i)
                   (let ((k (- i n-in)))
                     (if (< k (fill-pointer new)) (aref new k) (make-jb 1 1))))
               out)))
          (coerce out 'simple-vector))))))

(defun jbig2-combined-symbols (input new)
  "Concatenate the imported vector INPUT and the accumulated NEW vector."
  (let ((out (make-array (+ (length input) (fill-pointer new)))))
    (dotimes (i (length input)) (setf (aref out i) (aref input i)))
    (dotimes (i (fill-pointer new)) (setf (aref out (+ (length input) i)) (aref new i)))
    out))

(defun jbig2-decode-text-region (dc refinement width height default-pixel
                                 num-instances strip-size input-symbols code-len
                                 transposed ds-offset reference-corner combop
                                 ref-template ref-at log-strip-size)
  "Decode a text region (§6.4): place symbol instances onto a WIDTH×HEIGHT bitmap.
Arithmetic (non-Huffman) coding only.  Returns a JB bitmap."
  (declare (fixnum width height num-instances strip-size code-len
                   reference-corner combop log-strip-size ds-offset)
           (ignorable ref-template ref-at))
  (when (> num-instances 10000000)
    (return-from jbig2-decode-text-region (make-jb (max 1 width) (max 1 height))))
  (let* ((bmp (make-jb (max 1 width) (max 1 height) (if (/= 0 default-pixel) 1 0)))
         (nsym (length input-symbols))
         (strip-t (let ((v (dc-int dc :iadt))) (if (integerp v) (- v) 0)))
         (i 0) (first-s 0) (guard 0))
    (declare (fixnum nsym strip-t i first-s guard))
    (loop while (and (< i num-instances) (< (incf guard) (+ num-instances 1000))) do
      (let ((dt (dc-int dc :iadt)))
        (unless (integerp dt) (return))
        (incf strip-t dt)
        (let ((dfs (dc-int dc :iafs)))
          (unless (integerp dfs) (return))
          (incf first-s dfs)
          (let ((current-s first-s) (sguard 0))
            (declare (fixnum current-s sguard))
            (loop
              (when (>= i num-instances) (return))
              (when (> (incf sguard) (+ num-instances 1000)) (return))
              (let ((current-t 0))
                (declare (fixnum current-t))
                (when (> strip-size 1)
                  (let ((v (dc-int dc :iait))) (when (integerp v) (setf current-t v))))
                (let* ((tt (+ (* strip-size strip-t) current-t))
                       (sid (dc-iaid dc code-len))
                       (apply-ref (and refinement
                                       (let ((v (dc-int dc :iari)))
                                         (and (integerp v) (/= v 0)))))
                       (sym (if (< sid nsym) (aref input-symbols sid) (make-jb 1 1)))
                       (sw (jb-w sym)) (sh (jb-h sym)))
                  (declare (fixnum tt sid sw sh))
                  (when apply-ref
                    (let ((rdw (dc-int dc :iardw)) (rdh (dc-int dc :iardh))
                          (rdx (dc-int dc :iardx)) (rdy (dc-int dc :iardy)))
                      (when (and (integerp rdw) (integerp rdh)
                                 (integerp rdx) (integerp rdy)
                                 (<= 0 (+ sw rdw) 8192) (<= 0 (+ sh rdh) 8192))
                        (setf sw (+ sw rdw) sh (+ sh rdh))
                        (setf sym (jbig2-decode-refinement
                                   dc sw sh ref-template sym
                                   (+ (ash rdw -1) rdx) (+ (ash rdh -1) rdy) ref-at)))))
                  ;; place SYM honouring transposition + reference corner (§6.4.5,
                  ;; steps vi/vii/viii/xi — the CURS pre/post advance is corner-
                  ;; and transposition-dependent).
                  (let ((right (logbitp 1 reference-corner))
                        (top (logbitp 0 reference-corner)))
                    ;; vi) pre-advance
                    (when (and (= transposed 0) right) (incf current-s (1- sw)))
                    (when (and (/= transposed 0) (not top)) (incf current-s (1- sh)))
                    ;; vii/viii) draw top-left location
                    (let ((si current-s) (ti tt))
                      (declare (fixnum si ti))
                      (when (/= transposed 0) (rotatef si ti))
                      (when right (decf si (1- sw)))
                      (when (not top) (decf ti (1- sh)))
                      (dotimes (yy2 sh)
                        (let ((y (+ ti yy2)))
                          (when (and (>= y 0) (< y height))
                            (dotimes (xx2 sw)
                              (let ((x (+ si xx2)))
                                (when (and (>= x 0) (< x width))
                                  (jbig2-blit bmp x y (jb-get sym xx2 yy2) combop))))))))
                    ;; xi) post-advance
                    (when (and (= transposed 0) (not right)) (incf current-s (1- sw)))
                    (when (and (/= transposed 0) top) (incf current-s (1- sh))))
                  (incf i)
                  (let ((ds (dc-int dc :iads)))
                    (when (eq ds :oob) (return))    ; end of strip
                    (unless (integerp ds) (return))
                    (incf current-s (+ ds ds-offset))))))))))
    bmp))

(declaim (inline jb-combine jbig2-blit))
(defun jb-combine (cur s op)
  "Combine current bit CUR with source bit S per JBIG2 operator OP (§8.2 / Table)."
  (declare (fixnum cur s op))
  (case op
    (1 (logand cur s))               ; AND
    (2 (logxor cur s))               ; XOR
    (3 (logxor 1 (logxor cur s)))    ; XNOR
    (4 s)                            ; REPLACE
    (t (logior cur s))))             ; OR (default 0)

(defun jbig2-blit (bmp x y v combop)
  (declare (fixnum x y v combop))
  (when (and (< x (jb-w bmp)) (< y (jb-h bmp)))
    (jb-set bmp x y (jb-combine (jb-get bmp x y) v combop))))

;;; ===========================================================================
;;; Segment headers (§7.2) + page composition (§7.4.8, §8.2)
;;; ===========================================================================

(declaim (inline rd-u32 rd-u16 rd-i8))
(defun rd-u32 (d p)
  (declare (fixnum p))
  (logior (ash (aref d p) 24) (ash (aref d (+ p 1)) 16)
          (ash (aref d (+ p 2)) 8) (aref d (+ p 3))))
(defun rd-u16 (d p)
  (declare (fixnum p))
  (logior (ash (aref d p) 8) (aref d (+ p 1))))
(defun rd-i8 (d p)
  (declare (fixnum p))
  (let ((b (aref d p))) (if (>= b 128) (- b 256) b)))

;; segment type -> keyword (only the types we act on are named).
(defun jbig2-segment-kind (type)
  (case type
    (0 :symbol-dictionary)
    ((4 6 7) :text-region)          ; intermediate/immediate text region
    (16 :pattern-dictionary)
    ((20 22 23) :halftone-region)
    ((36 38 39) :generic-region)
    ((40 42 43) :refinement-region)
    (48 :page-info)
    (49 :end-of-page)
    (50 :end-of-stripe)
    (51 :end-of-file)
    (53 :tables)
    (62 :extension)
    (t :unknown)))

(defstruct jbseg number type kind retain referred page-assoc start end)

(defun jbig2-read-segment-header (d p)
  "Parse a segment header at byte offset P.  Returns (values jbseg header-end)."
  (declare (fixnum p))
  (let* ((number (rd-u32 d p))
         (flags (aref d (+ p 4)))
         (type (logand flags #x3f))
         (page-assoc-4 (logbitp 6 flags))
         (referred-flags (aref d (+ p 5)))
         (ref-count (ash referred-flags -5))
         (position (+ p 6)))
    (declare (fixnum number flags type referred-flags ref-count position))
    (cond
      ((= ref-count 7)                 ; long form (7.2.4)
       (setf ref-count (logand (rd-u32 d (1- position)) #x1fffffff))
       (incf position 3)
       (incf position (ash (+ ref-count 7) -3)))    ; skip retain bits
      (t nil))
    (let* ((ref-size (cond ((<= number 256) 1) ((<= number 65536) 2) (t 4)))
           (referred '()))
      (dotimes (i ref-count)
        (push (case ref-size
                (1 (aref d position))
                (2 (rd-u16 d position))
                (t (rd-u32 d position)))
              referred)
        (incf position ref-size))
      (setf referred (nreverse referred))
      (let ((page-assoc (if page-assoc-4 (rd-u32 d position) (aref d position))))
        (incf position (if page-assoc-4 4 1))
        (let ((length (rd-u32 d position)))
          (incf position 4)
          (values (make-jbseg :number number :type type
                              :kind (jbig2-segment-kind type)
                              :referred referred :page-assoc page-assoc
                              :end length)         ; :end temporarily holds LENGTH
                  position))))))

;;; visitor / page-composition state
(defstruct viz
  (symbols (make-hash-table))     ; segment-number -> vector of exported JBs
  page                            ; the composed page JB (1 = black)
  (page-w 0) (page-h 0)
  (combop 0) (combop-override nil))

(defun jbig2-region-info (d p)
  "§7.4.1 region segment information field.  Returns (values w h x y combop next)."
  (declare (fixnum p))
  (values (rd-u32 d p) (rd-u32 d (+ p 4)) (rd-u32 d (+ p 8)) (rd-u32 d (+ p 12))
          (logand (aref d (+ p 16)) 7) (+ p 17)))

(defun jbig2-draw (viz x y region combop)
  "Composite REGION (a JB) onto the page at (X,Y) with COMBOP (§7.4.8.5 / Table 34:
OR/AND/XOR/XNOR/REPLACE).  Every pixel in the region footprint is combined."
  (declare (fixnum x y combop))
  (let ((page (viz-page viz)))
    (when page
      (let ((pw (jb-w page)) (ph (jb-h page)))
        (declare (fixnum pw ph))
        (dotimes (i (jb-h region))
          (let ((yy (+ y i)))
            (when (and (>= yy 0) (< yy ph))
              (dotimes (j (jb-w region))
                (let ((xx (+ x j)))
                  (when (and (>= xx 0) (< xx pw))
                    (jb-set page xx yy
                            (jb-combine (jb-get page xx yy) (jb-get region j i) combop))))))))))))

(defun jbig2-collect-symbols (viz referred)
  "Gather all exported symbols from the REFERRED segments into one vector."
  (let ((out (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (r referred)
      (let ((syms (gethash r (viz-symbols viz))))
        (when syms (loop for s across syms do (vector-push-extend s out)))))
    (coerce out 'simple-vector)))

(defun jbig2-process-segment (viz seg d)
  "Decode+dispatch one segment's data (SEG-START .. SEG-END in D)."
  (let ((p (jbseg-start seg)) (end (jbseg-end seg)))
    (declare (fixnum p end))
    (case (jbseg-kind seg)
      (:page-info
       (let* ((w (rd-u32 d p)) (h (rd-u32 d (+ p 4)))
              (flags (aref d (+ p 16)))
              (default (logand (ash flags -2) 1)))
         (when (= h #xffffffff) (setf h 0))
         (setf (viz-combop viz) (logand (ash flags -3) 3)
               (viz-combop-override viz) (logbitp 6 flags)
               (viz-page-w viz) w (viz-page-h viz) h)
         (when (and (> w 0) (> h 0) (< (* w h) 200000000))
           (setf (viz-page viz) (make-jb w h (if (= default 1) 1 0))))))
      (:generic-region
       (multiple-value-bind (w h x y rcombop np) (jbig2-region-info d p)
         (declare (fixnum w h x y rcombop np))
         (let* ((flags (aref d np))
                (mmr (logbitp 0 flags))
                (template (logand (ash flags -1) 3))
                (prediction (logbitp 3 flags))
                (pos (1+ np))
                (at '()))
           (unless mmr
             (dotimes (i (if (= template 0) 4 1))
               (push (cons (rd-i8 d pos) (rd-i8 d (1+ pos))) at)
               (incf pos 2))
             (setf at (nreverse at)))
           (let* ((dc (make-dc d pos end))
                  (bmp (jbig2-decode-bitmap dc mmr w h template prediction at)))
             (jbig2-draw viz x y bmp
                         (if (viz-combop-override viz) rcombop (viz-combop viz)))))))
      (:symbol-dictionary
       (let* ((flags (rd-u16 d p))
              (huffman (logbitp 0 flags))
              (refinement (logbitp 1 flags))
              (template (logand (ash flags -10) 3))
              (ref-template (logand (ash flags -12) 1))
              (pos (+ p 2))
              (at '()) (ref-at '()))
         (when huffman
           (error "JBIG2 Huffman symbol dictionary not supported"))
         (dotimes (i (if (= template 0) 4 1))
           (push (cons (rd-i8 d pos) (rd-i8 d (1+ pos))) at) (incf pos 2))
         (setf at (nreverse at))
         (when (and refinement (= ref-template 0))
           (dotimes (i 2)
             (push (cons (rd-i8 d pos) (rd-i8 d (1+ pos))) ref-at) (incf pos 2))
           (setf ref-at (nreverse ref-at)))
         (let ((num-exported (rd-u32 d pos))
               (num-new (rd-u32 d (+ pos 4))))
           (incf pos 8)
           (let* ((input (jbig2-collect-symbols viz (jbseg-referred seg)))
                  (dc (make-dc d pos end))
                  (exported (jbig2-decode-symbol-dict
                             dc refinement input num-new num-exported
                             template at ref-template ref-at)))
             (setf (gethash (jbseg-number seg) (viz-symbols viz)) exported)))))
      (:text-region
       (multiple-value-bind (w h x y rcombop np) (jbig2-region-info d p)
         (declare (ignore rcombop) (fixnum w h x y np))
         (let* ((flags (rd-u16 d np))
                (huffman (logbitp 0 flags))
                (refinement (logbitp 1 flags))
                (log-strip (logand (ash flags -2) 3))
                (strip-size (ash 1 log-strip))
                (reference-corner (logand (ash flags -4) 3))
                (transposed (if (logbitp 6 flags) 1 0))
                (combop (logand (ash flags -7) 3))
                (default-pixel (logand (ash flags -9) 1))
                ;; SBDSOFFSET is a signed 5-bit field (bits 10-14).
                (ds-offset (let ((v (logand (ash flags -10) #x1f)))
                             (if (>= v 16) (- v 32) v)))
                (ref-template (logand (ash flags -15) 1))
                (pos (+ np 2))
                (ref-at '()))
           (when huffman
             (error "JBIG2 Huffman text region not supported"))
           (when (and refinement (= ref-template 0))
             (dotimes (i 2)
               (push (cons (rd-i8 d pos) (rd-i8 d (1+ pos))) ref-at) (incf pos 2))
             (setf ref-at (nreverse ref-at)))
           (let ((num-instances (rd-u32 d pos)))
             (incf pos 4)
             (let* ((input (jbig2-collect-symbols viz (jbseg-referred seg)))
                    (code-len (jb-log2 (length input)))
                    (dc (make-dc d pos end))
                    (bmp (jbig2-decode-text-region
                          dc refinement w h default-pixel num-instances strip-size
                          input code-len transposed ds-offset reference-corner
                          combop ref-template ref-at log-strip)))
               (jbig2-draw viz x y bmp
                           (if (viz-combop-override viz) combop (viz-combop viz))))))))
      (:refinement-region
       ;; §7.4.7 generic refinement region: refine the page (the reference is the
       ;; current page contents at the region rectangle) in place.
       (multiple-value-bind (w h x y rcombop np) (jbig2-region-info d p)
         (declare (fixnum w h x y rcombop np))
         (let* ((flags (aref d np))
                (template (logand flags 1))
                (tpgron (logbitp 1 flags))
                (pos (1+ np))
                (at '()))
           (when (= template 0)
             (dotimes (i 2)
               (push (cons (rd-i8 d pos) (rd-i8 d (1+ pos))) at) (incf pos 2))
             (setf at (nreverse at)))
           (when (viz-page viz)
             (let* ((ref (jb-subregion (viz-page viz) x y w h))
                    (dc (make-dc d pos end))
                    (result (jbig2-decode-refinement dc w h template ref 0 0 at tpgron)))
               ;; the refined bitmap replaces the page region.
               (jbig2-draw viz x y result 4))))))
      ((:halftone-region :pattern-dictionary)
       (error "JBIG2 segment ~A not supported" (jbseg-kind seg)))
      (t nil))))                          ; end-of-*, tables, extension, unknown

(defparameter +jbig2-file-magic+
  (coerce '(#x97 #x4a #x42 #x32 #x0d #x0a #x1a #x0a) '(vector (unsigned-byte 8))))

(defun jbig2-file-header-p (d)
  (and (>= (length d) 8)
       (loop for i below 8 always (= (aref d i) (aref +jbig2-file-magic+ i)))))

(defun jbig2-process-stream (viz d)
  "Read the segments in octet vector D (either the PDF header-less embedded
organisation, or a full JBIG2 file with the 8-byte magic + file header) and
process each in order.  Handles both sequential and random-access organisation."
  (let ((position 0) (len (length d)) (random-access nil))
    (declare (fixnum position len))
    (when (jbig2-file-header-p d)
      ;; 7.2 / D.4 file header: magic(8) + flags(1) + [numberOfPages(4)].
      (let ((flags (aref d 8)))
        (setf random-access (not (logbitp 0 flags)))    ; bit0 set => sequential
        (setf position 9)
        (unless (logbitp 1 flags) (incf position 4))))   ; known-length page count
    ;; pass 1: read all segment headers.
    (let ((segs '()) (guard 0))
      (loop while (and (< position len) (< (incf guard) 1000000)) do
        (multiple-value-bind (seg header-end) (jbig2-read-segment-header d position)
          (let ((slen (jbseg-end seg)))       ; :end temporarily holds LENGTH
            (setf (jbseg-start seg) header-end)
            (when (= slen #xffffffff) (return))  ; unknown-length: bail (rare)
            (cond
              (random-access
               ;; headers are contiguous; data blocks follow after all headers.
               (setf (jbseg-end seg) slen)      ; keep LENGTH here for pass 2
               (setf position header-end))
              (t
               (setf (jbseg-end seg) (min len (+ header-end slen)))
               (setf position (jbseg-end seg))))
            (push seg segs)
            (when (eq (jbseg-kind seg) :end-of-file) (return)))))
      (setf segs (nreverse segs))
      (when random-access
        ;; assign data ranges sequentially after the header block.
        (dolist (seg segs)
          (let ((slen (jbseg-end seg)))
            (setf (jbseg-start seg) position
                  (jbseg-end seg) (min len (+ position slen)))
            (setf position (jbseg-end seg)))))
      ;; pass 2: process.
      (dolist (seg segs)
        (handler-case (jbig2-process-segment viz seg d)
          (error () nil))))))

;;; ===========================================================================
;;; Top level
;;; ===========================================================================

(defun jbig2-decode (data globals width height)
  "Decode embedded-organisation JBIG2 DATA (with optional GLOBALS segment stream)
into a packed 1-bit-per-pixel DeviceGray sample buffer (row-major, MSB-first,
byte-padded rows) of the given WIDTH×HEIGHT, with JBIG2's 1=black inverted to the
PDF convention (bit 1 = white).  Returns the byte vector, or NIL on failure."
  (declare (fixnum width height))
  (let ((viz (make-viz))
        (data (if (typep data '(simple-array (unsigned-byte 8) (*)))
                  data (coerce data '(simple-array (unsigned-byte 8) (*))))))
    (when globals
      (let ((g (if (typep globals '(simple-array (unsigned-byte 8) (*)))
                   globals (coerce globals '(simple-array (unsigned-byte 8) (*))))))
        (jbig2-process-stream viz g)))
    (jbig2-process-stream viz data)
    (let ((page (viz-page viz)))
      (unless page (return-from jbig2-decode nil))
      (let* ((pw (jb-w page)) (ph (jb-h page))
             (width (if (> width 0) width pw))
             (height (if (> height 0) height ph))
             (rb (ceiling width 8))
             (out (make-array (* rb height) :element-type '(unsigned-byte 8)
                                            :initial-element 0)))
        (declare (fixnum pw ph rb))
        ;; pack + invert: white pixels set bit 1, black clear.
        (dotimes (y height)
          (let ((base (* y rb)))
            (dotimes (x width)
              (let ((black (and (< x pw) (< y ph) (= 1 (jb-get page x y)))))
                (unless black
                  (setf (aref out (+ base (ash x -3)))
                        (logior (aref out (+ base (ash x -3)))
                                (ash #x80 (- (logand x 7))))))))))
        (values out width height)))))
