;;;; src/ccitt.lisp — CCITT Group 3 / Group 4 fax decoder (ITU-T T.4 / T.6).
;;;;
;;;; Decodes the bilevel run-length-coded data produced by the PDF CCITTFaxDecode
;;;; filter (§7.4.6) into a 1-bit-per-pixel sample buffer that the image pipeline
;;;; consumes exactly like a Flate-decoded 1-bit DeviceGray image.
;;;;
;;;;   /K  < 0  -> pure two-dimensional coding (Group 4, T.6) — the dominant PDF
;;;;               scan case, implemented in full.
;;;;   /K  = 0  -> one-dimensional coding (Group 3 1D, T.4) — implemented.
;;;;   /K  > 0  -> mixed 1D/2D (Group 3 2D) — decoded best-effort via the T.6 path.
;;;;
;;;; The run-length Huffman code tables (T.4 Tables 1–3: white/black terminating
;;;; and make-up codes, plus the shared extended make-up codes) are bundled below.
;;;;
;;;; Output polarity: the decoder knows intrinsically which pixels are white
;;;; (background) and which are black (ink).  It emits sample bits per /BlackIs1:
;;;; default (false) follows the normal PDF convention where a white pixel is a 1
;;;; bit and a black pixel is a 0 bit, so a DeviceGray/BPC-1 read renders black
;;;; marks on a white page; /BlackIs1 true flips the two.

(in-package #:folio)

;;; ---- MSB-first bit reader --------------------------------------------------
(defstruct (bitr (:constructor make-bitr (data)))
  (data (make-array 0 :element-type '(unsigned-byte 8))
        :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum)                   ; byte index
  (bit 0 :type fixnum))                  ; bit within byte, 0 = MSB

(declaim (inline bitr-eof-p))
(defun bitr-eof-p (br)
  (>= (bitr-pos br) (length (bitr-data br))))

(defun bitr-read-bit (br)
  "Next bit (0/1) MSB-first, or NIL at end of data."
  (declare (optimize (speed 3) (safety 1)))
  (let ((p (bitr-pos br)))
    (declare (fixnum p))
    (if (>= p (length (bitr-data br)))
        nil
        (let* ((byte (aref (bitr-data br) p))
               (b (logand (ash byte (- (- 7 (bitr-bit br)))) 1)))
          (declare (fixnum b))
          (if (>= (the fixnum (1+ (bitr-bit br))) 8)
              (progn (setf (bitr-bit br) 0) (setf (bitr-pos br) (the fixnum (1+ p))))
              (incf (bitr-bit br)))
          b))))

(defun bitr-align (br)
  "Advance to the next byte boundary (for /EncodedByteAlign)."
  (when (> (bitr-bit br) 0)
    (setf (bitr-bit br) 0)
    (incf (bitr-pos br))))

;;; ---- run-length code tables (T.4 Tables 1–3) -------------------------------
;;; Each entry is (run "bit-code").  Terminating codes carry runs 0–63; make-up
;;; codes carry runs >= 64.  A decoded run is the sum of make-up codes followed
;;; by exactly one terminating code.

(defparameter +white-run-codes+
  '((0 "00110101") (1 "000111") (2 "0111") (3 "1000") (4 "1011") (5 "1100")
    (6 "1110") (7 "1111") (8 "10011") (9 "10100") (10 "00111") (11 "01000")
    (12 "001000") (13 "000011") (14 "110100") (15 "110101") (16 "101010")
    (17 "101011") (18 "0100111") (19 "0001100") (20 "0001000") (21 "0010111")
    (22 "0000011") (23 "0000100") (24 "0101000") (25 "0101011") (26 "0010011")
    (27 "0100100") (28 "0011000") (29 "00000010") (30 "00000011") (31 "00011010")
    (32 "00011011") (33 "00010010") (34 "00010011") (35 "00010100") (36 "00010101")
    (37 "00010110") (38 "00010111") (39 "00101000") (40 "00101001") (41 "00101010")
    (42 "00101011") (43 "00101100") (44 "00101101") (45 "00000100") (46 "00000101")
    (47 "00001010") (48 "00001011") (49 "01010010") (50 "01010011") (51 "01010100")
    (52 "01010101") (53 "00100100") (54 "00100101") (55 "01011000") (56 "01011001")
    (57 "01011010") (58 "01011011") (59 "01001010") (60 "01001011") (61 "00110010")
    (62 "00110011") (63 "00110100")
    ;; white make-up codes
    (64 "11011") (128 "10010") (192 "010111") (256 "0110111") (320 "00110110")
    (384 "00110111") (448 "01100100") (512 "01100101") (576 "01101000")
    (640 "01100111") (704 "011001100") (768 "011001101") (832 "011010010")
    (896 "011010011") (960 "011010100") (1024 "011010101") (1088 "011010110")
    (1152 "011010111") (1216 "011011000") (1280 "011011001") (1344 "011011010")
    (1408 "011011011") (1472 "010011000") (1536 "010011001") (1600 "010011010")
    (1664 "011000") (1728 "010011011"))
  "T.4 white run-length terminating + make-up codes.")

(defparameter +black-run-codes+
  '((0 "0000110111") (1 "010") (2 "11") (3 "10") (4 "011") (5 "0011") (6 "0010")
    (7 "00011") (8 "000101") (9 "000100") (10 "0000100") (11 "0000101")
    (12 "0000111") (13 "00000100") (14 "00000111") (15 "000011000")
    (16 "0000010111") (17 "0000011000") (18 "0000001000") (19 "00001100111")
    (20 "00001101000") (21 "00001101100") (22 "00000110111") (23 "00000101000")
    (24 "00000010111") (25 "00000011000") (26 "000011001010") (27 "000011001011")
    (28 "000011001100") (29 "000011001101") (30 "000001101000") (31 "000001101001")
    (32 "000001101010") (33 "000001101011") (34 "000011010010") (35 "000011010011")
    (36 "000011010100") (37 "000011010101") (38 "000011010110") (39 "000011010111")
    (40 "000001101100") (41 "000001101101") (42 "000011011010") (43 "000011011011")
    (44 "000001010100") (45 "000001010101") (46 "000001010110") (47 "000001010111")
    (48 "000001100100") (49 "000001100101") (50 "000001010010") (51 "000001010011")
    (52 "000000100100") (53 "000000110111") (54 "000000111000") (55 "000000100111")
    (56 "000000101000") (57 "000001011000") (58 "000001011001") (59 "000000101011")
    (60 "000000101100") (61 "000001011010") (62 "000001100110") (63 "000001100111")
    ;; black make-up codes
    (64 "0000001111") (128 "000011001000") (192 "000011001001") (256 "000001011011")
    (320 "000000110011") (384 "000000110100") (448 "000000110101")
    (512 "0000001101100") (576 "0000001101101") (640 "0000001001010")
    (704 "0000001001011") (768 "0000001001100") (832 "0000001001101")
    (896 "0000001110010") (960 "0000001110011") (1024 "0000001110100")
    (1088 "0000001110101") (1152 "0000001110110") (1216 "0000001110111")
    (1280 "0000001010010") (1344 "0000001010011") (1408 "0000001010100")
    (1472 "0000001010101") (1536 "0000001011010") (1600 "0000001011011")
    (1664 "0000001100100") (1728 "0000001100101"))
  "T.4 black run-length terminating + make-up codes.")

(defparameter +ext-run-codes+
  '((1792 "00000001000") (1856 "00000001100") (1920 "00000001101")
    (1984 "000000010010") (2048 "000000010011") (2112 "000000010100")
    (2176 "000000010101") (2240 "000000010110") (2304 "000000010111")
    (2368 "000000011100") (2432 "000000011101") (2496 "000000011110")
    (2560 "000000011111"))
  "T.4 Table 3 extended make-up codes, shared by both colours.")

(defun %build-run-table (entries)
  "Build a canonical-trie hash: key = sentinel-prefixed integer of the bit code
(1 followed by the code bits) -> run length.  Prefix-free, so bit-by-bit lookup
yields the first (and only) match."
  (let ((h (make-hash-table)))
    (dolist (e (append entries +ext-run-codes+))
      (destructuring-bind (run bits) e
        (let ((key 1))
          (loop for ch across bits
                do (setf key (logior (ash key 1) (if (char= ch #\1) 1 0))))
          (setf (gethash key h) run))))
    h))

(defparameter +white-table+ (%build-run-table +white-run-codes+))
(defparameter +black-table+ (%build-run-table +black-run-codes+))

(defun %decode-run (br table)
  "Decode one full run (sum of make-up codes + a terminating code) from BR using
TABLE.  Returns the run length, or NIL on a malformed / truncated code."
  (let ((total 0))
    (declare (fixnum total))
    (loop
      (let ((key 1))
        (declare (fixnum key))
        (loop
          (let ((bit (bitr-read-bit br)))
            (when (null bit) (return-from %decode-run nil))
            (setf key (logior (ash key 1) bit))
            (let ((run (gethash key table)))
              (when run
                (incf total (the fixnum run))
                (when (< (the fixnum run) 64)
                  (return-from %decode-run total))
                (return)))          ; make-up: continue accumulating
            (when (> (integer-length key) 15)
              (return-from %decode-run nil))))))))

;;; ---- 2-D mode codes (T.6 §2) ----------------------------------------------
;;; Returns (values kind delta): :v (with delta -3..3), :h, :pass, :eol, :eof.
(defun %read-mode (br)
  (macrolet ((rb () '(bitr-read-bit br))
             (eof () '(return-from %read-mode (values :eof 0))))
    (let ((b (rb)))
      (cond
        ((null b) (values :eof 0))
        ((= b 1) (values :v 0))                          ; 1        = V0
        (t (let ((b (rb)))
             (cond
               ((null b) (values :eof 0))
               ((= b 1) (let ((c (rb)))                  ; 01x
                          (cond ((null c) (values :eof 0))
                                ((= c 1) (values :v 1))  ; 011      = VR1
                                (t (values :v -1)))))     ; 010      = VL1
               (t (let ((c (rb)))
                    (cond
                      ((null c) (values :eof 0))
                      ((= c 1) (values :h 0))            ; 001      = horizontal
                      (t (let ((d (rb)))
                           (cond
                             ((null d) (values :eof 0))
                             ((= d 1) (values :pass 0))  ; 0001     = pass
                             (t (let ((e (rb)))
                                  (cond
                                    ((null e) (values :eof 0))
                                    ((= e 1)             ; 00001x
                                     (let ((f (rb)))
                                       (cond ((null f) (values :eof 0))
                                             ((= f 1) (values :v 2))   ; 000011 VR2
                                             (t (values :v -2)))))      ; 000010 VL2
                                    (t (let ((f (rb)))   ; 00000x
                                         (cond
                                           ((null f) (values :eof 0))
                                           ((= f 1)      ; 000001x
                                            (let ((g (rb)))
                                              (cond ((null g) (values :eof 0))
                                                    ((= g 1) (values :v 3))  ; 0000011 VR3
                                                    (t (values :v -3)))))     ; 0000010 VL3
                                           (t (values :eol 0))))))))))))))))))))) ; 000000.. EOL/EOFB/ext

;;; ---- changing-element search (reference line) ------------------------------
(defun %find-b1b2 (ref reflen a0 color columns)
  "b1 = first changing element on the reference line to the right of A0 whose
colour is opposite to COLOR; b2 = the element following b1.  Reference changing
elements alternate colour starting black (index 0 = white->black)."
  (declare (type (simple-array fixnum (*)) ref) (fixnum reflen a0 color columns))
  (let ((i 0))
    (declare (fixnum i))
    (loop while (and (< i reflen) (<= (aref ref i) a0)) do (incf i))
    ;; element colour at index i: black(1) if even, white(0) if odd.
    (let ((want (- 1 color)))
      (when (and (< i reflen) (/= (if (evenp i) 1 0) want)) (incf i))
      (values (if (< i reflen) (aref ref i) columns)
              (if (< (1+ i) reflen) (aref ref (1+ i)) columns)))))

;;; ---- one coded line --------------------------------------------------------
(defun %decode-2d-line (br ref reflen cur columns)
  "Decode one T.6 line relative to REF (REFLEN transitions) into CUR (the coding
line's transition positions).  Returns (values curlen status), status one of
:ok :eol :eof."
  (declare (type (simple-array fixnum (*)) ref cur) (fixnum reflen columns))
  (let ((a0 -1) (color 0) (n 0) (guard 0)
        (cap (- (length cur) 2)) (maxit (+ (* 2 columns) 32)))
    (declare (fixnum a0 color n guard cap maxit))
    (loop
      (when (>= a0 columns) (return (values n :ok)))
      (when (or (>= n cap) (> (incf guard) maxit)) (return (values n :ok)))
      (multiple-value-bind (b1 b2) (%find-b1b2 ref reflen a0 color columns)
        (declare (fixnum b1 b2))
        (multiple-value-bind (kind delta) (%read-mode br)
          (case kind
            (:pass (setf a0 b2))                         ; colour unchanged
            (:h
             (let* ((start (if (< a0 0) 0 a0))
                    (r1 (%decode-run br (if (= color 0) +white-table+ +black-table+)))
                    (r2 (%decode-run br (if (= color 0) +black-table+ +white-table+))))
               (when (or (null r1) (null r2)) (return (values n :eof)))
               (let ((a1 (min columns (+ start (the fixnum r1))))
                     (a2 (min columns (+ start (the fixnum r1) (the fixnum r2)))))
                 (setf (aref cur n) a1) (incf n)
                 (setf (aref cur n) a2) (incf n)
                 (setf a0 a2))))                         ; colour unchanged (two flips)
            (:v
             (let ((a1 (max 0 (min columns (+ b1 (the fixnum delta))))))
               (setf (aref cur n) a1) (incf n)
               (setf a0 a1)
               (setf color (- 1 color))))
            ((:eol :eof) (return (values n kind)))))))))

;;; ---- one 1-D (Group 3 1D / T.4) line --------------------------------------
(defun %maybe-skip-eol (br)
  "Consume a single EOL code (000000000001, optionally preceded by fill bits) if
one begins here; otherwise leave BR untouched.  T.4 delimits lines with EOLs; a
run of >= 11 zero bits then a 1 is unambiguously an EOL (no run code is that
long)."
  (let ((sp (bitr-pos br)) (sb (bitr-bit br)) (zeros 0))
    (declare (fixnum sp sb zeros))
    (loop
      (let ((b (bitr-read-bit br)))
        (cond ((null b) (setf (bitr-pos br) sp (bitr-bit br) sb) (return))
              ((= b 0) (incf zeros))
              (t (unless (>= zeros 11)            ; not an EOL: rewind
                   (setf (bitr-pos br) sp (bitr-bit br) sb))
                 (return)))))))

(defun %decode-1d-line (br cur columns)
  "Decode one Modified-Huffman (1-D) line into CUR.  Returns (values curlen status)."
  (declare (type (simple-array fixnum (*)) cur) (fixnum columns))
  (%maybe-skip-eol br)
  (let ((color 0) (pos 0) (n 0) (cap (- (length cur) 1)))
    (declare (fixnum color pos n cap))
    (loop
      (when (>= pos columns) (return (values n :ok)))
      (when (>= n cap) (return (values n :ok)))
      (let ((run (%decode-run br (if (= color 0) +white-table+ +black-table+))))
        (when (null run) (return (values n (if (= n 0) :eof :ok))))
        (setf pos (min columns (+ pos (the fixnum run))))
        (setf (aref cur n) pos) (incf n)
        (setf color (- 1 color))))))

;;; ---- row emission ----------------------------------------------------------
(defun %set-bits (out base start end)
  "Set bits [START,END) to 1 in the row that begins at byte index BASE (MSB-first,
whole-byte fast path)."
  (declare (type (array (unsigned-byte 8) (*)) out) (fixnum base start end))
  (let ((i start))
    (declare (fixnum i))
    (loop while (< i end) do
      (let ((bit (logand i 7)))
        (declare (fixnum bit))
        (if (and (= bit 0) (<= (+ i 8) end))
            (progn (setf (aref out (+ base (ash i -3))) #xFF) (incf i 8))
            (progn (setf (aref out (+ base (ash i -3)))
                         (logior (aref out (+ base (ash i -3))) (ash #x80 (- bit))))
                   (incf i)))))))

(defun %emit-row (out row-bytes cur curlen columns white-bit black-bit)
  "Append one packed row (ROW-BYTES bytes) to OUT (a fill-pointer vector) from the
transition list CUR (CURLEN entries)."
  (declare (type (array (unsigned-byte 8) (*)) out)
           (type (simple-array fixnum (*)) cur)
           (fixnum row-bytes curlen columns))
  (let ((base (fill-pointer out)) (color 0) (pos 0))
    (declare (fixnum base color pos))
    (dotimes (k row-bytes) (vector-push-extend 0 out))
    (dotimes (i curlen)
      (let ((tt (max pos (min (aref cur i) columns))))
        (declare (fixnum tt))
        (when (> tt pos)
          (when (= (if (= color 0) white-bit black-bit) 1) (%set-bits out base pos tt)))
        (setf pos tt)
        (setf color (- 1 color))))
    (when (< pos columns)
      (when (= (if (= color 0) white-bit black-bit) 1) (%set-bits out base pos columns)))))

(defun %emit-white-row (out row-bytes columns white-bit)
  (declare (type (array (unsigned-byte 8) (*)) out) (fixnum row-bytes columns))
  (let ((base (fill-pointer out)))
    (dotimes (k row-bytes) (vector-push-extend 0 out))
    (when (= white-bit 1) (%set-bits out base 0 columns))))

;;; ---- top level -------------------------------------------------------------
(defparameter *ccitt-max-rows* 100000
  "Safety cap on decoded rows when /Rows is 0 (decode until EOFB/data end).")

(defun ccitt-decode (data &key (columns 1728) (rows 0) (k 0) black-is-1 byte-align)
  "Decode CCITTFaxDecode DATA into a packed 1-bit-per-pixel sample buffer
(row-major, MSB-first, each row padded to a byte boundary).  Returns
(values bytes n-rows)."
  (declare (fixnum columns rows k))
  (let* ((bytes (if (typep data '(simple-array (unsigned-byte 8) (*)))
                    data (coerce data '(simple-array (unsigned-byte 8) (*)))))
         (br (make-bitr bytes))
         (twod (not (= k 0)))            ; K<0 (G4) and K>0 (best-effort) go 2-D
         (row-bytes (ceiling columns 8))
         (white-bit (if black-is-1 0 1))
         (black-bit (if black-is-1 1 0))
         (max-rows (if (> rows 0) rows *ccitt-max-rows*))
         (out (make-array (* row-bytes (max 1 (min max-rows 4096)))
                          :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (ref (make-array (+ columns 4) :element-type 'fixnum :initial-element 0))
         (cur (make-array (+ columns 4) :element-type 'fixnum :initial-element 0))
         (reflen 0) (rowcount 0))
    (declare (fixnum row-bytes white-bit black-bit max-rows reflen rowcount))
    (block decode
      (loop
        (when (>= rowcount max-rows) (return))
        (when (and (> rows 0) (>= rowcount rows)) (return))
        (when (bitr-eof-p br) (return))
        (multiple-value-bind (curlen status)
            (if twod
                (%decode-2d-line br ref reflen cur columns)
                (%decode-1d-line br cur columns))
          (when (and (member status '(:eol :eof)) (= curlen 0))
            (return))                    ; EOFB / clean end
          (%emit-row out row-bytes cur curlen columns white-bit black-bit)
          (incf rowcount)
          (rotatef ref cur)
          (setf reflen curlen)
          (when (eq status :eof) (return))
          (when byte-align (bitr-align br)))))
    ;; pad to the requested row count with white rows if the data ran short
    (when (> rows 0)
      (loop while (< rowcount rows) do
        (%emit-white-row out row-bytes columns white-bit)
        (incf rowcount)))
    (values (coerce out '(simple-array (unsigned-byte 8) (*))) rowcount)))
