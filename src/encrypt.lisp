;;;; src/encrypt.lisp — the standard security handler (ISO 32000-1 §7.6.3,
;;;; ISO 32000-2 §7.6.4.3): empty-user-password decryption for RC4 (V1/V2, R2/R3),
;;;; AES-128 (V4/R4, AESV2) and AES-256 (V5/R6, AESV3).  Pure CL, no FFI: this
;;;; file carries its own MD5, RC4, AES (both directions, for the R6 hash) and the
;;;; SHA-2 family (256/384/512) so folio has no crypto dependency.

(in-package #:folio)

;;; ======================================================================
;;; MD5 (RFC 1321)
;;; ======================================================================

(declaim (inline u32 rol32))
(defun u32 (x) (logand x #xFFFFFFFF))
(defun rol32 (x n) (u32 (logior (ash x n) (ash x (- n 32)))))

(defparameter +md5-s+
  #(7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22
    5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20
    4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23
    6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21))

(defparameter +md5-k+
  (let ((a (make-array 64)))
    (dotimes (i 64 a)
      (setf (aref a i) (u32 (floor (* (abs (sin (float (1+ i) 1d0))) 4294967296d0)))))))

(defun md5 (bytes &rest more)
  "MD5 digest of the concatenation of BYTES and MORE octet vectors -> 16 octets."
  (let* ((parts (cons bytes more))
         (total (reduce #'+ parts :key #'length))
         (msg (make-array (* 64 (ceiling (+ total 9) 64))
                          :element-type '(unsigned-byte 8) :initial-element 0))
         (p 0))
    (dolist (v parts) (replace msg v :start1 p) (incf p (length v)))
    (setf (aref msg total) #x80)
    (let ((bits (* total 8)))
      (dotimes (i 8) (setf (aref msg (+ (- (length msg) 8) i)) (ldb (byte 8 (* 8 i)) bits))))
    (let ((a0 #x67452301) (b0 #xefcdab89) (c0 #x98badcfe) (d0 #x10325476))
      (loop for off from 0 below (length msg) by 64 do
        (let ((m (make-array 16)))
          (dotimes (i 16)
            (setf (aref m i)
                  (logior (aref msg (+ off (* i 4)))
                          (ash (aref msg (+ off (* i 4) 1)) 8)
                          (ash (aref msg (+ off (* i 4) 2)) 16)
                          (ash (aref msg (+ off (* i 4) 3)) 24))))
          (let ((a a0) (b b0) (c c0) (d d0))
            (dotimes (i 64)
              (let (f g)
                (cond ((< i 16) (setf f (logior (logand b c) (logand (lognot b) d)) g i))
                      ((< i 32) (setf f (logior (logand d b) (logand (lognot d) c)) g (mod (1+ (* 5 i)) 16)))
                      ((< i 48) (setf f (logxor b c d) g (mod (+ (* 3 i) 5) 16)))
                      (t        (setf f (logxor c (logior b (u32 (lognot d)))) g (mod (* 7 i) 16))))
                (setf f (u32 (+ f a (aref +md5-k+ i) (aref m g))))
                (setf a d d c c b
                      b (u32 (+ b (rol32 f (aref +md5-s+ i)))))))
            (setf a0 (u32 (+ a0 a)) b0 (u32 (+ b0 b))
                  c0 (u32 (+ c0 c)) d0 (u32 (+ d0 d))))))
      (let ((out (make-array 16 :element-type '(unsigned-byte 8))))
        (loop for word in (list a0 b0 c0 d0) for i from 0 do
          (dotimes (j 4) (setf (aref out (+ (* i 4) j)) (ldb (byte 8 (* 8 j)) word))))
        out))))

;;; ======================================================================
;;; RC4
;;; ======================================================================

(defun rc4 (key data)
  "RC4 keystream applied to DATA (encrypt == decrypt).  KEY, DATA octet vectors."
  (let ((s (make-array 256 :element-type '(unsigned-byte 16)))
        (klen (length key)))
    (dotimes (i 256) (setf (aref s i) i))
    (let ((j 0))
      (dotimes (i 256)
        (setf j (logand (+ j (aref s i) (aref key (mod i klen))) #xFF))
        (rotatef (aref s i) (aref s j))))
    (let ((out (make-array (length data) :element-type '(unsigned-byte 8)))
          (i 0) (j 0))
      (dotimes (n (length data) out)
        (setf i (logand (1+ i) #xFF))
        (setf j (logand (+ j (aref s i)) #xFF))
        (rotatef (aref s i) (aref s j))
        (setf (aref out n)
              (logxor (aref data n) (aref s (logand (+ (aref s i) (aref s j)) #xFF))))))))

;;; ======================================================================
;;; AES (FIPS-197) — encrypt + decrypt single block; CBC wrappers below.
;;; ======================================================================

(declaim (inline rol32-8))
(defun rol32-8 (x n) (logand (logior (ash x n) (ash x (- n 8))) #xFF))

(defparameter +aes-sbox+
  (let ((sb (make-array 256 :element-type '(unsigned-byte 8)))
        (p 1) (q 1))
    (setf (aref sb 0) #x63)
    (loop
      ;; p*3 in GF(2^8)
      (setf p (logxor p (logand (ash p 1) #xFF) (if (logbitp 7 p) #x1B 0)) p (logand p #xFF))
      (setf q (logxor q (ash q 1)) q (logand q #xFF)
            q (logxor q (ash q 2)) q (logand q #xFF)
            q (logxor q (ash q 4)) q (logand q #xFF)
            q (logxor q (if (logbitp 7 q) #x09 0)) q (logand q #xFF))
      (let ((x (logxor q (rol32-8 q 1) (rol32-8 q 2) (rol32-8 q 3) (rol32-8 q 4) #x63)))
        (setf (aref sb p) (logand x #xFF)))
      (when (= p 1) (return)))
    sb))

(defparameter +aes-inv-sbox+
  (let ((inv (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (i 256 inv) (setf (aref inv (aref +aes-sbox+ i)) i))))

(declaim (inline gmul))
(defun gmul (a b)
  "Multiply A*B in GF(2^8) with the AES polynomial."
  (let ((p 0))
    (dotimes (i 8 p)
      (when (logbitp 0 b) (setf p (logxor p a)))
      (let ((hi (logbitp 7 a)))
        (setf a (logand (ash a 1) #xFF))
        (when hi (setf a (logxor a #x1B))))
      (setf b (ash b -1)))))

(defun aes-key-schedule (key)
  "Expand a 16- or 32-byte KEY into round-key words (list of 4-byte words)."
  (let* ((nk (/ (length key) 4))
         (nr (+ nk 6))
         (words (make-array (* 4 (1+ nr))))
         (rcon 1))
    (dotimes (i nk)
      (setf (aref words i)
            (vector (aref key (* 4 i)) (aref key (+ (* 4 i) 1))
                    (aref key (+ (* 4 i) 2)) (aref key (+ (* 4 i) 3)))))
    (loop for i from nk below (* 4 (1+ nr)) do
      (let ((temp (copy-seq (aref words (1- i)))))
        (cond
          ((zerop (mod i nk))
           ;; rotword + subword + rcon
           (let ((t0 (aref temp 0)))
             (setf (aref temp 0) (aref +aes-sbox+ (aref temp 1))
                   (aref temp 1) (aref +aes-sbox+ (aref temp 2))
                   (aref temp 2) (aref +aes-sbox+ (aref temp 3))
                   (aref temp 3) (aref +aes-sbox+ t0)))
           (setf (aref temp 0) (logxor (aref temp 0) rcon))
           (setf rcon (let ((r (ash rcon 1))) (if (> r #xFF) (logxor r #x11B) r))))
          ((and (> nk 6) (= (mod i nk) 4))
           (dotimes (j 4) (setf (aref temp j) (aref +aes-sbox+ (aref temp j))))))
        (let ((prev (aref words (- i nk))) (w (make-array 4)))
          (dotimes (j 4) (setf (aref w j) (logxor (aref prev j) (aref temp j))))
          (setf (aref words i) w))))
    (values words nr)))

(defmacro with-state ((state block) &body body)
  `(let ((,state (make-array 16 :element-type '(unsigned-byte 8))))
     (replace ,state ,block)
     ,@body))

(defun aes-add-round-key (st words r)
  (dotimes (c 4)
    (let ((w (aref words (+ (* r 4) c))))
      (dotimes (row 4)
        (setf (aref st (+ (* c 4) row)) (logxor (aref st (+ (* c 4) row)) (aref w row)))))))

(defun aes-encrypt-block (block words nr)
  "Encrypt a 16-byte BLOCK (column-major state) with expanded WORDS -> 16 octets."
  (with-state (st block)
    (aes-add-round-key st words 0)
    (loop for round from 1 below nr do
      (dotimes (i 16) (setf (aref st i) (aref +aes-sbox+ (aref st i))))
      (aes-shift-rows st nil)
      (aes-mix-columns st nil)
      (aes-add-round-key st words round))
    (dotimes (i 16) (setf (aref st i) (aref +aes-sbox+ (aref st i))))
    (aes-shift-rows st nil)
    (aes-add-round-key st words nr)
    st))

(defun aes-decrypt-block (block words nr)
  "Decrypt a 16-byte BLOCK with expanded WORDS -> 16 octets."
  (with-state (st block)
    (aes-add-round-key st words nr)
    (loop for round from (1- nr) downto 1 do
      (aes-shift-rows st t)
      (dotimes (i 16) (setf (aref st i) (aref +aes-inv-sbox+ (aref st i))))
      (aes-add-round-key st words round)
      (aes-mix-columns st t))
    (aes-shift-rows st t)
    (dotimes (i 16) (setf (aref st i) (aref +aes-inv-sbox+ (aref st i))))
    (aes-add-round-key st words 0)
    st))

(defun aes-shift-rows (st inverse)
  ;; state is column-major: index = col*4 + row
  (dotimes (row 4)
    (unless (zerop row)
      (let ((tmp (make-array 4)))
        (dotimes (col 4)
          (let ((src (if inverse (mod (- col row) 4) (mod (+ col row) 4))))
            (setf (aref tmp col) (aref st (+ (* src 4) row)))))
        (dotimes (col 4) (setf (aref st (+ (* col 4) row)) (aref tmp col)))))))

(defun aes-mix-columns (st inverse)
  (dotimes (c 4)
    (let* ((b (* c 4))
           (a0 (aref st b)) (a1 (aref st (+ b 1)))
           (a2 (aref st (+ b 2))) (a3 (aref st (+ b 3))))
      (if inverse
          (setf (aref st b)       (logxor (gmul a0 14) (gmul a1 11) (gmul a2 13) (gmul a3 9))
                (aref st (+ b 1)) (logxor (gmul a0 9) (gmul a1 14) (gmul a2 11) (gmul a3 13))
                (aref st (+ b 2)) (logxor (gmul a0 13) (gmul a1 9) (gmul a2 14) (gmul a3 11))
                (aref st (+ b 3)) (logxor (gmul a0 11) (gmul a1 13) (gmul a2 9) (gmul a3 14)))
          (setf (aref st b)       (logxor (gmul a0 2) (gmul a1 3) a2 a3)
                (aref st (+ b 1)) (logxor a0 (gmul a1 2) (gmul a2 3) a3)
                (aref st (+ b 2)) (logxor a0 a1 (gmul a2 2) (gmul a3 3))
                (aref st (+ b 3)) (logxor (gmul a0 3) a1 a2 (gmul a3 2)))))))

(defun aes-cbc-decrypt (key data &key (strip-padding t))
  "AES-CBC decrypt DATA whose FIRST 16 bytes are the IV.  KEY = 16 or 32 octets.
When STRIP-PADDING, remove PKCS#7 padding.  Returns the plaintext octet vector."
  (when (< (length data) 32) (return-from aes-cbc-decrypt (make-array 0 :element-type '(unsigned-byte 8))))
  (multiple-value-bind (words nr) (aes-key-schedule key)
    (let* ((iv (subseq data 0 16))
           (n (- (length data) 16))
           (nblocks (floor n 16))
           (out (make-array (* nblocks 16) :element-type '(unsigned-byte 8)))
           (prev iv))
      (dotimes (i nblocks)
        (let* ((ct (subseq data (+ 16 (* i 16)) (+ 32 (* i 16))))
               (pt (aes-decrypt-block ct words nr)))
          (dotimes (j 16) (setf (aref out (+ (* i 16) j)) (logxor (aref pt j) (aref prev j))))
          (setf prev ct)))
      (if (and strip-padding (> (length out) 0))
          (let ((pad (aref out (1- (length out)))))
            (if (<= 1 pad 16) (subseq out 0 (- (length out) pad)) out))
          out))))

(defun aes-cbc-encrypt-nopad (key iv data)
  "AES-CBC encrypt DATA (a multiple of 16 bytes) with KEY and IV, no padding.
Used only by the R6 hash (Algorithm 2.B)."
  (multiple-value-bind (words nr) (aes-key-schedule key)
    (let* ((n (length data))
           (out (make-array n :element-type '(unsigned-byte 8)))
           (prev iv))
      (loop for i from 0 below n by 16 do
        (let ((blk (make-array 16 :element-type '(unsigned-byte 8))))
          (dotimes (j 16) (setf (aref blk j) (logxor (aref data (+ i j)) (aref prev j))))
          (let ((ct (aes-encrypt-block blk words nr)))
            (replace out ct :start1 i)
            (setf prev ct))))
      out)))

;;; ======================================================================
;;; SHA-2 (256 / 384 / 512) — needed only by the R6 (AES-256) key hash.
;;; ======================================================================

(defparameter +sha256-k+
  #(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
    #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
    #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
    #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
    #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
    #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
    #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
    #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(declaim (inline ror32))
(defun ror32 (x n) (u32 (logior (ash x (- n)) (ash x (- 32 n)))))

(defun sha256 (bytes)
  "SHA-256 of octet vector BYTES -> 32 octets."
  (let* ((ml (length bytes))
         (padded (make-array (* 64 (ceiling (+ ml 9) 64))
                             :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace padded bytes)
    (setf (aref padded ml) #x80)
    (let ((bits (* ml 8)))
      (dotimes (i 8) (setf (aref padded (- (length padded) 1 i)) (ldb (byte 8 (* 8 i)) bits))))
    (let ((h (make-array 8 :initial-contents
                         '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                           #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
          (w (make-array 64)))
      (loop for off from 0 below (length padded) by 64 do
        (dotimes (i 16)
          (setf (aref w i) (logior (ash (aref padded (+ off (* i 4))) 24)
                                   (ash (aref padded (+ off (* i 4) 1)) 16)
                                   (ash (aref padded (+ off (* i 4) 2)) 8)
                                   (aref padded (+ off (* i 4) 3)))))
        (loop for i from 16 below 64 do
          (let ((s0 (logxor (ror32 (aref w (- i 15)) 7) (ror32 (aref w (- i 15)) 18) (ash (aref w (- i 15)) -3)))
                (s1 (logxor (ror32 (aref w (- i 2)) 17) (ror32 (aref w (- i 2)) 19) (ash (aref w (- i 2)) -10))))
            (setf (aref w i) (u32 (+ (aref w (- i 16)) s0 (aref w (- i 7)) s1)))))
        (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
              (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
          (dotimes (i 64)
            (let* ((s1 (logxor (ror32 e 6) (ror32 e 11) (ror32 e 25)))
                   (ch (logxor (logand e f) (logand (u32 (lognot e)) g)))
                   (t1 (u32 (+ hh s1 ch (aref +sha256-k+ i) (aref w i))))
                   (s0 (logxor (ror32 a 2) (ror32 a 13) (ror32 a 22)))
                   (maj (logxor (logand a b) (logand a c) (logand b c)))
                   (t2 (u32 (+ s0 maj))))
              (setf hh g g f f e e (u32 (+ d t1)) d c c b b a a (u32 (+ t1 t2)))))
          (setf (aref h 0) (u32 (+ (aref h 0) a)) (aref h 1) (u32 (+ (aref h 1) b))
                (aref h 2) (u32 (+ (aref h 2) c)) (aref h 3) (u32 (+ (aref h 3) d))
                (aref h 4) (u32 (+ (aref h 4) e)) (aref h 5) (u32 (+ (aref h 5) f))
                (aref h 6) (u32 (+ (aref h 6) g)) (aref h 7) (u32 (+ (aref h 7) hh)))))
      (let ((out (make-array 32 :element-type '(unsigned-byte 8))))
        (dotimes (i 8 out)
          (dotimes (j 4) (setf (aref out (+ (* i 4) j)) (ldb (byte 8 (* 8 (- 3 j))) (aref h i)))))))))

(defparameter +sha512-k+
  #(#x428a2f98d728ae22 #x7137449123ef65cd #xb5c0fbcfec4d3b2f #xe9b5dba58189dbbc
    #x3956c25bf348b538 #x59f111f1b605d019 #x923f82a4af194f9b #xab1c5ed5da6d8118
    #xd807aa98a3030242 #x12835b0145706fbe #x243185be4ee4b28c #x550c7dc3d5ffb4e2
    #x72be5d74f27b896f #x80deb1fe3b1696b1 #x9bdc06a725c71235 #xc19bf174cf692694
    #xe49b69c19ef14ad2 #xefbe4786384f25e3 #x0fc19dc68b8cd5b5 #x240ca1cc77ac9c65
    #x2de92c6f592b0275 #x4a7484aa6ea6e483 #x5cb0a9dcbd41fbd4 #x76f988da831153b5
    #x983e5152ee66dfab #xa831c66d2db43210 #xb00327c898fb213f #xbf597fc7beef0ee4
    #xc6e00bf33da88fc2 #xd5a79147930aa725 #x06ca6351e003826f #x142929670a0e6e70
    #x27b70a8546d22ffc #x2e1b21385c26c926 #x4d2c6dfc5ac42aed #x53380d139d95b3df
    #x650a73548baf63de #x766a0abb3c77b2a8 #x81c2c92e47edaee6 #x92722c851482353b
    #xa2bfe8a14cf10364 #xa81a664bbc423001 #xc24b8b70d0f89791 #xc76c51a30654be30
    #xd192e819d6ef5218 #xd69906245565a910 #xf40e35855771202a #x106aa07032bbd1b8
    #x19a4c116b8d2d0c8 #x1e376c085141ab53 #x2748774cdf8eeb99 #x34b0bcb5e19b48a8
    #x391c0cb3c5c95a63 #x4ed8aa4ae3418acb #x5b9cca4f7763e373 #x682e6ff3d6b2b8a3
    #x748f82ee5defb2fc #x78a5636f43172f60 #x84c87814a1f0ab72 #x8cc702081a6439ec
    #x90befffa23631e28 #xa4506cebde82bde9 #xbef9a3f7b2c67915 #xc67178f2e372532b
    #xca273eceea26619c #xd186b8c721c0c207 #xeada7dd6cde0eb1e #xf57d4f7fee6ed178
    #x06f067aa72176fba #x0a637dc5a2c898a6 #x113f9804bef90dae #x1b710b35131c471b
    #x28db77f523047d84 #x32caab7b40c72493 #x3c9ebe0a15c9bebc #x431d67c49c100d4c
    #x4cc5d4becb3e42b6 #x597f299cfc657e2a #x5fcb6fab3ad6faec #x6c44198c4a475817))

(declaim (inline u64 ror64))
(defun u64 (x) (logand x #xFFFFFFFFFFFFFFFF))
(defun ror64 (x n) (u64 (logior (ash x (- n)) (ash x (- 64 n)))))

(defun %sha512-core (bytes h0)
  "SHA-512/384 compression over BYTES with initial hash vector H0 (8 x 64-bit)."
  (let* ((ml (length bytes))
         (padded (make-array (* 128 (ceiling (+ ml 17) 128))
                             :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace padded bytes)
    (setf (aref padded ml) #x80)
    (let ((bits (* ml 8)))
      (dotimes (i 8) (setf (aref padded (- (length padded) 1 i)) (ldb (byte 8 (* 8 i)) bits))))
    (let ((h (copy-seq h0)) (w (make-array 80)))
      (loop for off from 0 below (length padded) by 128 do
        (dotimes (i 16)
          (let ((v 0)) (dotimes (b 8) (setf v (logior (ash v 8) (aref padded (+ off (* i 8) b)))))
            (setf (aref w i) v)))
        (loop for i from 16 below 80 do
          (let ((s0 (logxor (ror64 (aref w (- i 15)) 1) (ror64 (aref w (- i 15)) 8) (ash (aref w (- i 15)) -7)))
                (s1 (logxor (ror64 (aref w (- i 2)) 19) (ror64 (aref w (- i 2)) 61) (ash (aref w (- i 2)) -6))))
            (setf (aref w i) (u64 (+ (aref w (- i 16)) s0 (aref w (- i 7)) s1)))))
        (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
              (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
          (dotimes (i 80)
            (let* ((s1 (logxor (ror64 e 14) (ror64 e 18) (ror64 e 41)))
                   (ch (logxor (logand e f) (logand (u64 (lognot e)) g)))
                   (t1 (u64 (+ hh s1 ch (aref +sha512-k+ i) (aref w i))))
                   (s0 (logxor (ror64 a 28) (ror64 a 34) (ror64 a 39)))
                   (maj (logxor (logand a b) (logand a c) (logand b c)))
                   (t2 (u64 (+ s0 maj))))
              (setf hh g g f f e e (u64 (+ d t1)) d c c b b a a (u64 (+ t1 t2)))))
          (setf (aref h 0) (u64 (+ (aref h 0) a)) (aref h 1) (u64 (+ (aref h 1) b))
                (aref h 2) (u64 (+ (aref h 2) c)) (aref h 3) (u64 (+ (aref h 3) d))
                (aref h 4) (u64 (+ (aref h 4) e)) (aref h 5) (u64 (+ (aref h 5) f))
                (aref h 6) (u64 (+ (aref h 6) g)) (aref h 7) (u64 (+ (aref h 7) hh)))))
      h)))

(defun sha512 (bytes)
  (let ((h (%sha512-core bytes #(#x6a09e667f3bcc908 #xbb67ae8584caa73b #x3c6ef372fe94f82b #xa54ff53a5f1d36f1
                                 #x510e527fade682d1 #x9b05688c2b3e6c1f #x1f83d9abfb41bd6b #x5be0cd19137e2179)))
        (out (make-array 64 :element-type '(unsigned-byte 8))))
    (dotimes (i 8 out) (dotimes (j 8) (setf (aref out (+ (* i 8) j)) (ldb (byte 8 (* 8 (- 7 j))) (aref h i)))))))

(defun sha384 (bytes)
  (let ((h (%sha512-core bytes #(#xcbbb9d5dc1059ed8 #x629a292a367cd507 #x9159015a3070dd17 #x152fecd8f70e5939
                                 #x67332667ffc00b31 #x8eb44a8768581511 #xdb0c2e0d64f98fa7 #x47b5481dbefa4fa4)))
        (out (make-array 48 :element-type '(unsigned-byte 8))))
    (dotimes (i 6 out) (dotimes (j 8) (setf (aref out (+ (* i 8) j)) (ldb (byte 8 (* 8 (- 7 j))) (aref h i)))))))

;;; ======================================================================
;;; The standard security handler
;;; ======================================================================

(defparameter +pad+
  (coerce #(#x28 #xBF #x4E #x5E #x4E #x75 #x8A #x41 #x64 #x00 #x4E #x56 #xFF #xFA #x01 #x08
            #x2E #x2E #x00 #xB6 #xD0 #x68 #x3E #x80 #x2F #x0C #xA9 #xFE #x64 #x53 #x69 #x7A)
          '(simple-array (unsigned-byte 8) (*)))
  "The 32-byte password padding string (Algorithm 2).")

(defstruct crypt
  method            ; :rc4 | :aesv2 | :aesv3
  key               ; the file encryption key (octet vector)
  (v 0) (r 0)
  encrypt-num       ; object number of the /Encrypt dict (never decrypted)
  (supported nil)   ; T when we can decrypt
  reason)           ; a human string when unsupported

(defun %pdf-bytes (x)
  (cond ((pdf-string-p x) (pdf-string-bytes x))
        ((and (vectorp x) (every #'integerp x))
         (coerce x '(simple-array (unsigned-byte 8) (*))))
        (t (make-array 0 :element-type '(unsigned-byte 8)))))

(defun setup-decryption (doc)
  "Detect /Encrypt in the trailer and, for the standard handler with an empty user
password, derive the file key.  Records a CRYPT on DOC (SUPPORTED = T when we can
decrypt).  Clears the object cache so every object re-loads decrypted."
  (let* ((tr (document-trailer doc))
         (enc-ref (and (hash-table-p tr) (dict-get tr "Encrypt"))))
    (unless enc-ref (return-from setup-decryption nil))
    (let* ((enc-num (and (pdf-ref-p enc-ref) (pdf-ref-num enc-ref)))
           (enc (resolve doc enc-ref)))
      (unless (hash-table-p enc)
        (setf (document-crypt doc) (make-crypt :supported nil :reason "malformed /Encrypt"))
        (return-from setup-decryption nil))
      (let ((c (handler-case (build-crypt doc enc enc-num)
                 (error (e) (make-crypt :supported nil
                                        :reason (format nil "~A" e))))))
        (setf (document-crypt doc) c)
        ;; wipe any objects cached before the key existed
        (clrhash (document-cache doc))
        (clrhash (document-objstm-cache doc))
        c))))

(defun build-crypt (doc enc enc-num)
  (let* ((filter (dict-get enc "Filter"))
         (v (let ((x (resolve doc (dict-get enc "V")))) (if (integerp x) x 0)))
         (r (let ((x (resolve doc (dict-get enc "R")))) (if (integerp x) x 0))))
    (unless (name= filter "Standard")
      (return-from build-crypt
        (make-crypt :supported nil :v v :r r :encrypt-num enc-num
                    :reason (format nil "non-standard security handler /~A"
                                    (and (pdf-name-p filter) (pdf-name-string filter))))))
    (let ((method (crypt-method-for enc v)))
      (case method
        ((:rc4 :aesv2)
         (let ((key (compute-rc4-key doc enc v r)))
           (make-crypt :method method :key key :v v :r r :encrypt-num enc-num
                       :supported (and key t)
                       :reason (unless key "could not derive key"))))
        (:aesv3
         (let ((key (compute-aes256-key doc enc)))
           (make-crypt :method :aesv3 :key key :v v :r r :encrypt-num enc-num
                       :supported (and key t)
                       :reason (if key nil "empty user password rejected (AES-256)"))))
        (t (make-crypt :supported nil :v v :r r :encrypt-num enc-num
                       :reason "unsupported crypt filter method"))))))

(defun crypt-method-for (enc v)
  "Determine the string/stream method from /V and the /CF crypt-filter (§7.6.5)."
  (if (< v 4)
      :rc4
      (let* ((cf (dict-get enc "CF"))
             (stmf (dict-get enc "StmF"))
             (name (and (pdf-name-p stmf) (pdf-name-string stmf))))
        (cond
          ((or (null name) (string= name "Identity")) :identity)
          ((hash-table-p cf)
           (let* ((filt (dict-get cf name))
                  (cfm (and (hash-table-p filt) (dict-get filt "CFM"))))
             (cond ((name= cfm "V2") :rc4)
                   ((name= cfm "AESV2") :aesv2)
                   ((name= cfm "AESV3") :aesv3)
                   (t :identity))))
          (t :identity)))))

(defun compute-rc4-key (doc enc v r)
  "Algorithm 2: derive the file key from the EMPTY user password (RC4 / AESV2)."
  (declare (ignore v))
  (let* ((o (%pdf-bytes (resolve doc (dict-get enc "O"))))
         (p (let ((x (resolve doc (dict-get enc "P")))) (if (integerp x) x 0)))
         (len (let ((x (resolve doc (dict-get enc "Length")))) (if (integerp x) x 40)))
         (nbytes (if (= r 2) 5 (floor len 8)))
         (id (first-id doc))
         (pbytes (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4) (setf (aref pbytes i) (ldb (byte 8 (* 8 i)) (logand p #xFFFFFFFF))))
    (let* ((metadata (resolve doc (dict-get enc "EncryptMetadata")))
           (parts (list +pad+ o pbytes id)))
      (when (and (>= r 4) (eq metadata :false))
        (setf parts (append parts (list (coerce #(#xFF #xFF #xFF #xFF) '(simple-array (unsigned-byte 8) (*)))))))
      (let ((hash (apply #'md5 parts)))
        (when (>= r 3)
          (dotimes (i 50) (setf hash (md5 (subseq hash 0 nbytes)))))
        (subseq hash 0 nbytes)))))

(defun first-id (doc)
  "First element of the trailer /ID (the file identifier), as octets."
  (let ((idarr (resolve doc (dict-get (document-trailer doc) "ID"))))
    (if (and (pdf-array-p idarr) (plusp (length idarr)))
        (%pdf-bytes (resolve doc (aref idarr 0)))
        (make-array 0 :element-type '(unsigned-byte 8)))))

;;; ---- AES-256 (R6) -----------------------------------------------------

(defun hash-r6 (password salt udata)
  "ISO 32000-2 Algorithm 2.B: the iterated SHA-2/AES-128 hash for R6."
  (let ((k (sha256 (concat-bytes password salt udata))))
    (loop for round from 0 do
      (let* ((k1-unit (concat-bytes password k udata))
             (k1 (repeat-bytes k1-unit 64))
             (e (aes-cbc-encrypt-nopad (subseq k 0 16) (subseq k 16 32) k1))
             (mod3 (mod (reduce #'+ e :start 0 :end 16) 3)))
        (setf k (case mod3 (0 (sha256 e)) (1 (sha384 e)) (2 (sha512 e))))
        (when (and (>= round 63) (<= (aref e (1- (length e))) (- round 32)))
          (return))))
    (subseq k 0 32)))

(defun compute-aes256-key (doc enc)
  "Algorithm 2.A for the EMPTY user password: validate /U, then unwrap /UE to the
file key.  Returns the 32-byte key, or NIL if the empty password is not the user
password."
  (let* ((u (%pdf-bytes (resolve doc (dict-get enc "U"))))
         (ue (%pdf-bytes (resolve doc (dict-get enc "UE"))))
         (pw (make-array 0 :element-type '(unsigned-byte 8))))
    (when (< (length u) 48) (return-from compute-aes256-key nil))
    (let* ((val-salt (subseq u 32 40))
           (key-salt (subseq u 40 48))
           (empty (make-array 0 :element-type '(unsigned-byte 8)))
           (check (hash-r6 pw val-salt empty)))
      (unless (equalp check (subseq u 0 32))
        (return-from compute-aes256-key nil))     ; empty pw is not the user pw
      (let* ((ik (hash-r6 pw key-salt empty))
             ;; AES-256-CBC, zero IV, no padding: prepend IV of zeros to reuse decrypt
             (iv (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
             (buf (concat-bytes iv ue)))
        (aes-cbc-decrypt ik buf :strip-padding nil)))))

;;; ---- byte helpers -----------------------------------------------------

(defun concat-bytes (&rest vs)
  (let* ((total (reduce #'+ vs :key #'length))
         (out (make-array total :element-type '(unsigned-byte 8)))
         (p 0))
    (dolist (v vs out) (replace out v :start1 p) (incf p (length v)))))

(defun repeat-bytes (v n)
  (let* ((l (length v)) (out (make-array (* l n) :element-type '(unsigned-byte 8))))
    (dotimes (i n out) (replace out v :start1 (* i l)))))

;;; ---- object-level decryption ------------------------------------------

(defun object-key (crypt num gen)
  "Algorithm 1: the per-object key for RC4 / AESV2 (AESV3 uses the file key)."
  (let* ((fk (crypt-key crypt))
         (extra (make-array (if (eq (crypt-method crypt) :aesv2) 9 5)
                            :element-type '(unsigned-byte 8))))
    (setf (aref extra 0) (ldb (byte 8 0) num)
          (aref extra 1) (ldb (byte 8 8) num)
          (aref extra 2) (ldb (byte 8 16) num)
          (aref extra 3) (ldb (byte 8 0) gen)
          (aref extra 4) (ldb (byte 8 8) gen))
    (when (eq (crypt-method crypt) :aesv2)
      (setf (aref extra 5) #x73 (aref extra 6) #x41 (aref extra 7) #x6C (aref extra 8) #x54))
    (let ((h (md5 fk extra)))
      (subseq h 0 (min (+ (length fk) 5) 16)))))

(defun decrypt-bytes (crypt bytes num gen)
  "Decrypt a single string/stream octet vector with the object's key."
  (case (crypt-method crypt)
    (:rc4   (rc4 (object-key crypt num gen) bytes))
    (:aesv2 (if (< (length bytes) 16) bytes
                (aes-cbc-decrypt (object-key crypt num gen) bytes)))
    (:aesv3 (if (< (length bytes) 16) bytes
                (aes-cbc-decrypt (crypt-key crypt) bytes)))
    (t bytes)))

(defun decrypt-object! (crypt obj num gen)
  "Recursively decrypt every string and stream body inside OBJ (in place),
using the indirect object's NUM/GEN.  Returns OBJ."
  (cond
    ((pdf-string-p obj)
     (setf (pdf-string-bytes obj)
           (coerce (decrypt-bytes crypt (pdf-string-bytes obj) num gen)
                   '(simple-array (unsigned-byte 8) (*)))))
    ((pdf-stream-p obj)
     (decrypt-object! crypt (pdf-stream-dict obj) num gen)
     (setf (pdf-stream-raw obj)
           (coerce (decrypt-bytes crypt (pdf-stream-raw obj) num gen)
                   '(simple-array (unsigned-byte 8) (*)))))
    ((hash-table-p obj)
     (maphash (lambda (k v)
                (declare (ignore k))
                (when (decryptable-p v) (decrypt-object! crypt v num gen)))
              obj))
    ((pdf-array-p obj)
     (loop for x across obj when (decryptable-p x) do (decrypt-object! crypt x num gen))))
  obj)

(defun decryptable-p (x)
  (or (pdf-string-p x) (pdf-stream-p x) (hash-table-p x) (pdf-array-p x)))

(defun maybe-decrypt-object (doc obj num gen)
  "Decrypt OBJ if the document is encrypted, this object is not the /Encrypt dict,
and it is not an /XRef stream (cross-reference streams are never encrypted)."
  (let ((c (document-crypt doc)))
    (if (and c (crypt-supported c)
             (not (eql num (crypt-encrypt-num c)))
             (not (and (pdf-stream-p obj)
                       (name= (dict-get (pdf-stream-dict obj) "Type") "XRef"))))
        (decrypt-object! c obj num gen)
        obj)))

(defun encryption-status (doc)
  "One of :NONE (not encrypted), :DECRYPTED (encrypted, key derived), or
:UNSUPPORTED (encrypted but folio cannot decrypt — e.g. a real password)."
  (let ((c (document-crypt doc)))
    (cond ((null c) :none)
          ((crypt-supported c) :decrypted)
          (t :unsupported))))

(defun encryption-reason (doc)
  (let ((c (document-crypt doc))) (and c (crypt-reason c))))
