;;;; src/shading.lisp — shadings / gradients (ISO 32000-1 §8.7.4.5) and shading
;;;; patterns (§8.7.3.3), plus the colour functions they share (§7.10).
;;;;
;;;; A shading paints a smooth colour gradation across a region.  The `sh` operator
;;;; paints one across the current clip; a shading PATTERN (/PatternType 2) named as
;;;; a fill colour paints one clipped to a filled path.  For each device pixel we map
;;;; back through the CTM to shading space, derive the shading parameter s (axial:
;;;; projection onto the axis; radial: the blends-of-circles solution), evaluate the
;;;; colour /Function at t = Domain(s), convert through the shading /ColorSpace to
;;;; RGB, and composite through gesso:fill-callback (which honours the clip, global
;;;; alpha, blend mode and soft mask — the same chokepoint fills use).
;;;;
;;;; Colour functions (§7.10): Type 2 exponential, Type 3 stitching, Type 0 sampled,
;;;; Type 4 PostScript calculator.  A shading /Function is evaluated once per LUT
;;;; entry (256 samples of s), so per-pixel cost is a table lookup + colour convert.

(in-package #:folio)

(defparameter *deferred-shadings* nil
  "Bound per render to collect shading types we cannot paint (mesh types 4–7).")

;;; ---- small numeric helpers ------------------------------------------------
(declaim (inline clampn))
(defun clampn (x lo hi)
  (let ((lo (min lo hi)) (hi (max lo hi)))
    (max lo (min hi (float x 1d0)))))

(defun interp-lin (x x0 x1 y0 y1)
  "Map X from [X0,X1] linearly onto [Y0,Y1]; if the domain is degenerate, Y0."
  (if (= x0 x1) (float y0 1d0)
      (+ y0 (* (- (float x 1d0) x0) (/ (- y1 y0) (- x1 x0))))))

(defun dict-num-array (doc dict key)
  "A dict entry that is an array of numbers -> a Lisp list of doubles, else NIL."
  (let ((a (resolve doc (dict-get dict key))))
    (when (pdf-array-p a)
      (map 'list (lambda (x) (num (resolve doc x))) a))))

;;; =====================================================================
;;;  Colour functions (§7.10)
;;; =====================================================================
;;; Every function is compiled to a closure (LAMBDA (inputs-list) -> outputs-list).
;;; A /Function that is an ARRAY of 1-output functions is concatenated.

(defun load-function (doc obj)
  "Parse a PDF function OBJ (a dict, a stream, or an array of functions) into a
closure mapping an input list to an output list."
  (setf obj (resolve doc obj))
  (cond
    ((pdf-array-p obj)
     (let ((fns (map 'list (lambda (f) (load-function doc f)) obj)))
       (lambda (in) (mapcan (lambda (f) (copy-list (funcall f in))) fns))))
    ((or (pdf-stream-p obj) (hash-table-p obj))
     (let* ((dict (if (pdf-stream-p obj) (pdf-stream-dict obj) obj))
            (ft (let ((x (resolve doc (dict-get dict "FunctionType"))))
                  (if (numberp x) (round x) -1))))
       (case ft
         (0 (load-sampled-function doc obj dict))
         (2 (load-exponential-function doc dict))
         (3 (load-stitching-function doc dict))
         (4 (load-postscript-function doc obj dict))
         (t (lambda (in) (declare (ignore in)) '(0d0))))))
    (t (lambda (in) (declare (ignore in)) '(0d0)))))

;;; ---- Type 2: exponential interpolation ------------------------------------
(defun load-exponential-function (doc dict)
  (let* ((domain (or (dict-num-array doc dict "Domain") '(0d0 1d0)))
         (c0 (or (dict-num-array doc dict "C0") '(0d0)))
         (c1 (or (dict-num-array doc dict "C1") '(1d0)))
         (n (num (resolve doc (dict-get dict "N"))))
         (d0 (first domain)) (d1 (second domain)))
    (lambda (in)
      (let* ((x (clampn (num (first in)) d0 d1))
             (xn (cond ((= n 1d0) x)
                       ((and (< x 0d0) (/= (fround n) n)) 0d0)
                       (t (expt x n)))))
        (mapcar (lambda (a b) (+ a (* xn (- b a)))) c0 c1)))))

;;; ---- Type 3: stitching ----------------------------------------------------
(defun load-stitching-function (doc dict)
  (let* ((domain (or (dict-num-array doc dict "Domain") '(0d0 1d0)))
         (d0 (first domain)) (d1 (second domain))
         (fns (let ((a (resolve doc (dict-get dict "Functions"))))
                (if (pdf-array-p a)
                    (map 'vector (lambda (f) (load-function doc f)) a)
                    (vector (lambda (in) (declare (ignore in)) '(0d0))))))
         (bounds (coerce (or (dict-num-array doc dict "Bounds") '()) 'vector))
         (encode (coerce (or (dict-num-array doc dict "Encode") '()) 'vector))
         (k (length fns)))
    (lambda (in)
      (let* ((x (clampn (num (first in)) d0 d1))
             (i (or (loop for j below (1- k)
                          when (< x (aref bounds j)) return j)
                    (1- k)))
             (lo (if (= i 0) d0 (aref bounds (1- i))))
             (hi (if (= i (1- k)) d1 (aref bounds i)))
             (e0 (if (< (* 2 i) (length encode)) (aref encode (* 2 i)) 0d0))
             (e1 (if (< (1+ (* 2 i)) (length encode)) (aref encode (1+ (* 2 i))) 1d0))
             (xe (interp-lin x lo hi e0 e1)))
        (funcall (aref fns i) (list xe))))))

;;; ---- Type 0: sampled ------------------------------------------------------
(defun read-bits (data bitpos nbits)
  "Read NBITS bits (MSB-first) starting at bit offset BITPOS in octet vector DATA."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (fixnum bitpos nbits))
  (let ((v 0) (len (length data)))
    (dotimes (i nbits v)
      (let* ((bp (+ bitpos i))
             (byte (ash bp -3))
             (bit (- 7 (logand bp 7)))
             (bv (if (< byte len) (logand (ash (aref data byte) (- bit)) 1) 0)))
        (setf v (logior (ash v 1) bv))))))

(defun load-sampled-function (doc strm dict)
  (let* ((domain (or (dict-num-array doc dict "Domain") '(0d0 1d0)))
         (range (or (dict-num-array doc dict "Range") '(0d0 1d0)))
         (size (mapcar #'round (or (dict-num-array doc dict "Size") '(2))))
         (bps (let ((x (resolve doc (dict-get dict "BitsPerSample")))) (if x (round (num x)) 8)))
         (m (length size))
         (n (floor (length range) 2))
         (encode (or (dict-num-array doc dict "Encode")
                     (loop for sz in size append (list 0d0 (float (1- sz) 1d0)))))
         (decode (or (dict-num-array doc dict "Decode") range))
         (data (ignore-errors (decode-stream strm :resolver (lambda (v) (resolve doc v)))))
         (maxval (float (1- (ash 1 bps)) 1d0)))
    (when (and data (not (typep data '(simple-array (unsigned-byte 8) (*)))))
      (setf data (coerce data '(simple-array (unsigned-byte 8) (*)))))
    (unless (and data (plusp m) (plusp n))
      (return-from load-sampled-function
        (lambda (in) (declare (ignore in)) (make-list n :initial-element 0d0))))
    (labels ((sample-at (flat comp) (read-bits data (* (+ (* flat n) comp) bps) bps))
             (out-decode (comp sv)
               (+ (nth (* 2 comp) decode)
                  (* sv (- (nth (1+ (* 2 comp)) decode) (nth (* 2 comp) decode))))))
      (if (= m 1)
          ;; 1-D linear interpolation (the shading / tint-transform case)
          (let ((sz0 (first size)) (d0 (first domain)) (d1 (second domain))
                (en0 (first encode)) (en1 (second encode)))
            (lambda (in)
              (let* ((x (clampn (num (first in)) d0 d1))
                     (e (clampn (interp-lin x d0 d1 en0 en1) 0d0 (float (1- sz0) 1d0)))
                     (i0 (floor e)) (i1 (min (1- sz0) (1+ i0)))
                     (fr (- e i0)))
                (loop for comp below n collect
                  (let ((s0 (/ (sample-at i0 comp) maxval))
                        (s1 (/ (sample-at i1 comp) maxval)))
                    (out-decode comp (+ s0 (* fr (- s1 s0)))))))))
          ;; m>1: nearest-neighbour on each input dimension
          (lambda (in)
            (let ((flat 0) (stride 1))
              (loop for k below m
                    for xv = (clampn (num (nth k in)) (nth (* 2 k) domain) (nth (1+ (* 2 k)) domain))
                    for e = (interp-lin xv (nth (* 2 k) domain) (nth (1+ (* 2 k)) domain)
                                        (nth (* 2 k) encode) (nth (1+ (* 2 k)) encode))
                    for ei = (max 0 (min (1- (nth k size)) (round e)))
                    do (incf flat (* ei stride)) (setf stride (* stride (nth k size))))
              (loop for comp below n collect
                (out-decode comp (/ (sample-at flat comp) maxval)))))))))

;;; ---- Type 4: PostScript calculator ----------------------------------------
(defun parse-ps-calculator (bytes)
  "Parse a Type-4 calculator program (the outer { ... }) into a nested list of
numbers, operator strings, and (:proc . sub-list) procedures."
  (let ((lx (make-lexer bytes)))
    (loop for tok = (lex-next lx)
          do (cond ((eq (token-type tok) :eof) (return-from parse-ps-calculator nil))
                   ((and (eq (token-type tok) :keyword)
                         (string= (token-value tok) "{"))
                    (return))))
    (parse-ps-block lx)))

(defun parse-ps-block (lx)
  "Read tokens until the matching `}` (the opening `{` already consumed)."
  (let ((out '()))
    (loop for tok = (lex-next lx) do
      (case (token-type tok)
        (:eof (return))
        ((:int :real) (push (token-value tok) out))
        (:keyword
         (let ((s (token-value tok)))
           (cond ((string= s "{") (push (cons :proc (parse-ps-block lx)) out))
                 ((string= s "}") (return))
                 (t (push s out)))))
        (t nil)))
    (nreverse out)))

(declaim (inline ps-num ps-true ps-boolp))
(defun ps-num (x) (cond ((numberp x) (float x 1d0)) ((eq x t) 1d0) (t 0d0)))
(defun ps-boolp (x) (or (eq x t) (null x)))
(defun ps-true (x) (cond ((eq x t) t) ((null x) nil) ((numberp x) (not (zerop x))) (t t)))
(defun ps-eq (a b) (if (and (numberp a) (numberp b)) (= (ps-num a) (ps-num b)) (eql a b)))

(defun eval-ps (prog stack)
  "Evaluate calculator program PROG against STACK (a list, car = top); return the
resulting stack."
  (let ((st stack))
    (dolist (tok prog st)
      (setf st (ps-step tok st)))))

(defun ps-step (tok stack)
  (cond
    ((numberp tok) (cons (float tok 1d0) stack))
    ((and (consp tok) (eq (car tok) :proc)) (cons tok stack)) ; push a procedure
    ((stringp tok) (ps-op tok stack))
    (t stack)))

(defun ps-op (op stack)
  "Apply the calculator operator OP to STACK (car = top); return the new stack."
  (labels ((p () (if stack (prog1 (car stack) (setf stack (cdr stack))) 0d0))
           (u (f) (push1 (funcall f (ps-num (p)))))
           (bin (f) (let ((b (p)) (a (p))) (push1 (funcall f (ps-num a) (ps-num b)))))
           (cmp (f) (let ((b (p)) (a (p))) (push1 (if (funcall f (ps-num a) (ps-num b)) t nil))))
           (push1 (v) (setf stack (cons v stack))))
    (cond
      ((string= op "add") (bin #'+))
      ((string= op "sub") (bin #'-))
      ((string= op "mul") (bin #'*))
      ((string= op "div") (bin (lambda (a b) (if (zerop b) 0d0 (/ a b)))))
      ((string= op "idiv") (bin (lambda (a b) (if (zerop b) 0d0 (float (truncate a b) 1d0)))))
      ((string= op "mod") (bin (lambda (a b) (if (zerop b) 0d0 (float (nth-value 1 (truncate a b)) 1d0)))))
      ((string= op "neg") (u #'-))
      ((string= op "abs") (u #'abs))
      ((string= op "sqrt") (u (lambda (x) (sqrt (max 0d0 x)))))
      ((string= op "sin") (u (lambda (x) (sin (* x (/ pi 180d0))))))
      ((string= op "cos") (u (lambda (x) (cos (* x (/ pi 180d0))))))
      ((string= op "atan") (let ((den (ps-num (p))) (nm (ps-num (p))))
                             (let ((d (* (atan nm den) (/ 180d0 pi))))
                               (push1 (if (< d 0d0) (+ d 360d0) d)))))
      ((string= op "exp") (bin (lambda (a b) (if (and (< a 0d0) (/= (fround b) b)) 0d0 (expt a b)))))
      ((string= op "ln") (u (lambda (x) (if (> x 0d0) (log x) 0d0))))
      ((string= op "log") (u (lambda (x) (if (> x 0d0) (log x 10d0) 0d0))))
      ((string= op "cvi") (u (lambda (x) (float (truncate x) 1d0))))
      ((string= op "cvr") (u #'identity))
      ((string= op "round") (u #'fround))
      ((string= op "truncate") (u #'ftruncate))
      ((string= op "floor") (u #'ffloor))
      ((string= op "ceiling") (u #'fceiling))
      ((string= op "eq") (let ((b (p)) (a (p))) (push1 (if (ps-eq a b) t nil))))
      ((string= op "ne") (let ((b (p)) (a (p))) (push1 (if (ps-eq a b) nil t))))
      ((string= op "gt") (cmp #'>))
      ((string= op "ge") (cmp #'>=))
      ((string= op "lt") (cmp #'<))
      ((string= op "le") (cmp #'<=))
      ((string= op "and") (let ((b (p)) (a (p)))
                            (push1 (if (and (ps-boolp a) (ps-boolp b))
                                       (and (ps-true a) (ps-true b))
                                       (float (logand (round (ps-num a)) (round (ps-num b))) 1d0)))))
      ((string= op "or") (let ((b (p)) (a (p)))
                           (push1 (if (and (ps-boolp a) (ps-boolp b))
                                      (or (ps-true a) (ps-true b))
                                      (float (logior (round (ps-num a)) (round (ps-num b))) 1d0)))))
      ((string= op "xor") (let ((b (p)) (a (p)))
                            (push1 (if (and (ps-boolp a) (ps-boolp b))
                                       (if (eq (ps-true a) (ps-true b)) nil t)
                                       (float (logxor (round (ps-num a)) (round (ps-num b))) 1d0)))))
      ((string= op "not") (let ((a (p))) (push1 (if (ps-boolp a) (not (ps-true a))
                                                    (float (lognot (round (ps-num a))) 1d0)))))
      ((string= op "bitshift") (let ((sh (round (ps-num (p)))) (a (round (ps-num (p)))))
                                 (push1 (float (ash a sh) 1d0))))
      ((string= op "true") (push1 t))
      ((string= op "false") (push1 nil))
      ((string= op "pop") (p))
      ((string= op "exch") (let ((b (p)) (a (p))) (push1 b) (push1 a)))
      ((string= op "dup") (let ((a (p))) (push1 a) (push1 a)))
      ((string= op "copy") (let ((n (round (ps-num (p)))))
                             (when (and (> n 0) (<= n (length stack)))
                               (setf stack (append (subseq stack 0 n) stack)))))
      ((string= op "index") (let ((n (round (ps-num (p)))))
                              (push1 (if (and (>= n 0) (< n (length stack))) (nth n stack) 0d0))))
      ((string= op "roll") (let ((j (round (ps-num (p)))) (n (round (ps-num (p)))))
                             (when (and (> n 0) (<= n (length stack)))
                               (let* ((seg (subseq stack 0 n))
                                      (rest (subseq stack n))
                                      (v (reverse seg))           ; bottom->top
                                      (newv (make-list n)))
                                 (dotimes (i n) (setf (nth (mod (+ i j) n) newv) (nth i v)))
                                 (setf stack (append (reverse newv) rest))))))
      ((string= op "if") (let ((proc (p)) (c (p)))
                           (when (and (consp proc) (eq (car proc) :proc) (ps-true c))
                             (setf stack (eval-ps (cdr proc) stack)))))
      ((string= op "ifelse") (let ((p2 (p)) (p1 (p)) (c (p)))
                               (let ((chosen (if (ps-true c) p1 p2)))
                                 (when (and (consp chosen) (eq (car chosen) :proc))
                                   (setf stack (eval-ps (cdr chosen) stack))))))
      (t nil))
    stack))

(defun load-postscript-function (doc strm dict)
  (let* ((domain (or (dict-num-array doc dict "Domain") '(0d0 1d0)))
         (range (or (dict-num-array doc dict "Range") '(0d0 1d0)))
         (n (floor (length range) 2))
         (data (ignore-errors (decode-stream strm :resolver (lambda (v) (resolve doc v)))))
         (prog (and data (ignore-errors (parse-ps-calculator
                                         (coerce data '(simple-array (unsigned-byte 8) (*))))))))
    (unless prog
      (return-from load-postscript-function
        (lambda (in) (declare (ignore in)) (make-list n :initial-element 0d0))))
    (lambda (in)
      (let ((stack '()))
        (loop for k below (floor (length domain) 2)
              for x = (clampn (num (nth k in)) (nth (* 2 k) domain) (nth (1+ (* 2 k)) domain))
              do (setf stack (cons x stack)))
        (setf stack (ignore-errors (eval-ps prog (reverse stack))))
        (let ((outs (reverse (subseq stack 0 (min n (length stack))))))
          (loop for comp below n
                for v = (if (< comp (length outs)) (ps-num (nth comp outs)) 0d0)
                collect (clampn v (nth (* 2 comp) range) (nth (1+ (* 2 comp)) range))))))))

;;; =====================================================================
;;;  Colour-space conversion (shading / pattern colour spaces -> RGB)
;;; =====================================================================
(defun lab-inv (f)
  (let ((f3 (* f f f))) (if (> f3 0.008856d0) f3 (/ (- f (/ 16d0 116d0)) 7.787d0))))

(defun lab->rgb (l a b)
  "CIELAB (D50) -> 8-bit sRGB (r g b)."
  (let* ((fy (/ (+ (float l 1d0) 16d0) 116d0))
         (fx (+ fy (/ (float a 1d0) 500d0)))
         (fz (- fy (/ (float b 1d0) 200d0)))
         (xx (* (lab-inv fx) 0.9642d0)) (yy (lab-inv fy)) (zz (* (lab-inv fz) 0.8249d0))
         (r (+ (* 3.1338561d0 xx) (* -1.6168667d0 yy) (* -0.4906146d0 zz)))
         (g (+ (* -0.9787684d0 xx) (* 1.9161415d0 yy) (* 0.0334540d0 zz)))
         (bl (+ (* 0.0719453d0 xx) (* -0.2289914d0 yy) (* 1.4052427d0 zz))))
    (flet ((gm (u) (let ((u (max 0d0 (min 1d0 u))))
                     (if (<= u 0.0031308d0) (* 12.92d0 u)
                         (- (* 1.055d0 (expt u (/ 1d0 2.4d0))) 0.055d0))))
           (ch (u) (max 0 (min 255 (round (* 255d0 u))))))
      (list (ch (gm r)) (ch (gm g)) (ch (gm bl))))))

(defun make-color-converter (ip csobj)
  "Return (values FN NCOMP): FN maps a list of NCOMP colour components (floats) to an
8-bit (r g b) list under colour space CSOBJ.  Handles Device*/Cal*/ICCBased/Lab/
Indexed/Separation/DeviceN (the last two evaluating their tint-transform function)."
  (let ((doc (interp-doc ip)))
    (setf csobj (resolve doc csobj))
    (cond
      ((null csobj) (values (lambda (c) (gray->rgb (num (first c)))) 1))
      ((pdf-name-p csobj)
       (let ((s (pdf-name-string csobj)))
         (cond
           ((member s '("DeviceGray" "G" "CalGray") :test #'string=)
            (values (lambda (c) (gray->rgb (num (first c)))) 1))
           ((member s '("DeviceRGB" "RGB" "CalRGB") :test #'string=)
            (values (lambda (c) (rgb255 (num (first c)) (num (second c)) (num (third c)))) 3))
           ((member s '("DeviceCMYK" "CMYK") :test #'string=)
            (values (lambda (c) (cmyk->rgb (num (first c)) (num (second c))
                                           (num (third c)) (num (fourth c)))) 4))
           (t (let* ((csd (and (interp-resources ip)
                               (resolve doc (dict-get (interp-resources ip) "ColorSpace"))))
                     (spec (and (hash-table-p csd) (dict-get csd s))))
                (if spec (make-color-converter ip spec)
                    (values (lambda (c) (gray->rgb (num (first c)))) 1)))))))
      ((pdf-array-p csobj)
       (let* ((head (and (plusp (length csobj)) (resolve doc (aref csobj 0))))
              (s (and (pdf-name-p head) (pdf-name-string head))))
         (cond
           ((equal s "ICCBased")
            (let* ((strm (resolve doc (aref csobj 1)))
                   (sd (and (pdf-stream-p strm) (pdf-stream-dict strm)))
                   (n (and sd (resolve doc (dict-get sd "N"))))
                   (alt (and sd (dict-get sd "Alternate"))))
              (cond (alt (make-color-converter ip alt))
                    (t (case (and n (round (num n)))
                         (1 (values (lambda (c) (gray->rgb (num (first c)))) 1))
                         (4 (values (lambda (c) (cmyk->rgb (num (first c)) (num (second c))
                                                           (num (third c)) (num (fourth c)))) 4))
                         (t (values (lambda (c) (rgb255 (num (first c)) (num (second c))
                                                        (num (third c)))) 3)))))))
           ((equal s "CalRGB")
            (values (lambda (c) (rgb255 (num (first c)) (num (second c)) (num (third c)))) 3))
           ((equal s "CalGray")
            (values (lambda (c) (gray->rgb (num (first c)))) 1))
           ((equal s "Lab")
            (values (lambda (c) (lab->rgb (num (first c)) (num (second c)) (num (third c)))) 3))
           ((or (equal s "Indexed") (equal s "I"))
            (multiple-value-bind (bfn bn) (make-color-converter ip (aref csobj 1))
              (let* ((lut (resolve doc (aref csobj 3)))
                     (bytes (cond ((pdf-string-p lut) (pdf-string-bytes lut))
                                  ((pdf-stream-p lut)
                                   (ignore-errors (decode-stream lut :resolver (lambda (v) (resolve doc v)))))
                                  (t nil))))
                (values
                 (lambda (c)
                   (let ((idx (max 0 (round (num (first c))))))
                     (if (and bytes (<= (* (1+ idx) bn) (length bytes)))
                         (funcall bfn (loop for i below bn
                                            collect (/ (aref bytes (+ (* idx bn) i)) 255d0)))
                         '(0 0 0))))
                 1))))
           ((equal s "Separation")
            (let ((tint (load-function doc (aref csobj 3))))
              (multiple-value-bind (afn) (make-color-converter ip (aref csobj 2))
                (values (lambda (c) (funcall afn (funcall tint (list (num (first c)))))) 1))))
           ((equal s "DeviceN")
            (let* ((names (resolve doc (aref csobj 1)))
                   (nc (if (pdf-array-p names) (length names) 1))
                   (tint (load-function doc (aref csobj 3))))
              (multiple-value-bind (afn) (make-color-converter ip (aref csobj 2))
                (values (lambda (c) (funcall afn (funcall tint (mapcar #'num c)))) nc))))
           (t (values (lambda (c) (rgb255 (num (first c)) (num (second c)) (num (third c)))) 3)))))
      (t (values (lambda (c) (gray->rgb (num (first c)))) 1)))))

;;; =====================================================================
;;;  Shadings (§8.7.4.5): axial (type 2), radial (type 3), function (type 1)
;;; =====================================================================
(defun shading-extend (doc dict)
  (let ((e (resolve doc (dict-get dict "Extend"))))
    (if (and (pdf-array-p e) (>= (length e) 2))
        (list (let ((x (resolve doc (aref e 0)))) (or (eq x :true) (eq x t)))
              (let ((x (resolve doc (aref e 1)))) (or (eq x :true) (eq x t))))
        '(nil nil))))

(defun shading-background (ip dict cvt)
  (let ((bg (dict-num-array (interp-doc ip) dict "Background")))
    (when bg (funcall cvt bg))))

(defun make-shading-lut (doc dict cvt &optional (nlut 256))
  "Precompute an (NLUT+1)-entry LUT mapping s in [0,1] to an RGB list, via the
shading /Function over /Domain and the colour converter CVT.  Returns a closure
(s -> rgb-list)."
  (let* ((domain (or (dict-num-array doc dict "Domain") '(0d0 1d0)))
         (t0 (first domain)) (t1 (second domain))
         (func (let ((f (dict-get dict "Function"))) (and f (load-function doc f))))
         (lut (make-array (1+ nlut))))
    (dotimes (i (1+ nlut))
      (let* ((s (/ i (float nlut 1d0)))
             (tt (+ t0 (* s (- t1 t0))))
             (comps (if func (funcall func (list tt)) (list s))))
        (setf (aref lut i) (funcall cvt comps))))
    (lambda (s)
      (aref lut (max 0 (min nlut (round (* (max 0d0 (min 1d0 s)) nlut))))))))

(defun axial-shading-fn (ip shading dict)
  "Type 2 axial shading -> a closure (ux uy) -> rgb-list or NIL (unpainted)."
  (declare (ignore shading))
  (let* ((doc (interp-doc ip))
         (coords (dict-num-array doc dict "Coords")))
    (unless (and coords (>= (length coords) 4)) (return-from axial-shading-fn nil))
    (multiple-value-bind (cvt) (make-color-converter ip (dict-get dict "ColorSpace"))
      (destructuring-bind (x0 y0 x1 y1 &rest ignore) coords
        (declare (ignore ignore))
        (let* ((dx (- x1 x0)) (dy (- y1 y0))
               (dd (+ (* dx dx) (* dy dy)))
               (extend (shading-extend doc dict))
               (e0 (first extend)) (e1 (second extend))
               (lut (make-shading-lut doc dict cvt))
               (bg (shading-background ip dict cvt)))
          (lambda (ux uy)
            (let ((s (if (<= dd 1d-12) 0d0
                         (/ (+ (* (- ux x0) dx) (* (- uy y0) dy)) dd))))
              (cond ((< s 0d0) (if e0 (funcall lut 0d0) bg))
                    ((> s 1d0) (if e1 (funcall lut 1d0) bg))
                    (t (funcall lut s))))))))))

(defun radial-shading-fn (ip shading dict)
  "Type 3 radial shading (blends of circles) -> a closure (ux uy) -> rgb or NIL.
Solves a s^2 - 2 b s + c = 0 for the interpolation parameter s, choosing the
largest s whose circle radius is non-negative and lies in the (extended) domain."
  (declare (ignore shading))
  (let* ((doc (interp-doc ip))
         (coords (dict-num-array doc dict "Coords")))
    (unless (and coords (>= (length coords) 6)) (return-from radial-shading-fn nil))
    (multiple-value-bind (cvt) (make-color-converter ip (dict-get dict "ColorSpace"))
      (destructuring-bind (x0 y0 r0 x1 y1 r1) coords
        (let* ((dcx (- x1 x0)) (dcy (- y1 y0)) (dr (- r1 r0))
               (a (- (+ (* dcx dcx) (* dcy dcy)) (* dr dr)))
               (extend (shading-extend doc dict))
               (e0 (first extend)) (e1 (second extend))
               (lut (make-shading-lut doc dict cvt))
               (bg (shading-background ip dict cvt)))
          (flet ((rad (s) (+ r0 (* s dr)))
                 (okrange (s) (cond ((<= 0d0 s 1d0) t) ((< s 0d0) e0) (t e1))))
            (lambda (ux uy)
              (let* ((pdx (- ux x0)) (pdy (- uy y0))
                     (b (+ (* pdx dcx) (* pdy dcy) (* r0 dr)))
                     (c (- (+ (* pdx pdx) (* pdy pdy)) (* r0 r0)))
                     (best nil))
                (if (< (abs a) 1d-9)
                    (unless (zerop b)
                      (let ((s (/ c (* 2d0 b))))
                        (when (and (>= (rad s) 0d0) (okrange s)) (setf best s))))
                    (let ((disc (- (* b b) (* a c))))
                      (when (>= disc 0d0)
                        (let* ((sq (sqrt disc))
                               (s1 (/ (+ b sq) a)) (s2 (/ (- b sq) a))
                               (hi (max s1 s2)) (lo (min s1 s2)))
                          (cond ((and (>= (rad hi) 0d0) (okrange hi)) (setf best hi))
                                ((and (>= (rad lo) 0d0) (okrange lo)) (setf best lo)))))))
                (if best (funcall lut (max 0d0 (min 1d0 best))) bg)))))))))

(defun function-shading-fn (ip shading dict)
  "Type 1 function-based shading -> a closure (ux uy) -> rgb or NIL.  A 2-input
function over /Domain, positioned by the shading /Matrix."
  (declare (ignore shading))
  (let* ((doc (interp-doc ip))
         (domain (or (dict-num-array doc dict "Domain") '(0d0 1d0 0d0 1d0)))
         (matrix (or (mat-from-array doc (dict-get dict "Matrix")) (gesso:mat-identity)))
         (minv (gesso:mat-invert matrix))
         (func (let ((f (dict-get dict "Function"))) (and f (load-function doc f)))))
    (unless func (return-from function-shading-fn nil))
    (multiple-value-bind (cvt) (make-color-converter ip (dict-get dict "ColorSpace"))
      (lambda (ux uy)
        (multiple-value-bind (fx fy) (if minv (gesso:mat-apply minv ux uy) (values ux uy))
          (if (and (<= (first domain) fx (second domain))
                   (<= (third domain) fy (fourth domain)))
              (funcall cvt (funcall func (list fx fy)))
              nil))))))

(defun shading-color-fn (ip shading dict stype)
  "Build the device-independent colour closure for SHADING (type STYPE)."
  (case stype
    (1 (ignore-errors (function-shading-fn ip shading dict)))
    (2 (ignore-errors (axial-shading-fn ip shading dict)))
    (3 (ignore-errors (radial-shading-fn ip shading dict)))
    (t (pushnew stype *deferred-shadings*) nil)))

(defun paint-shading (ip shading ctm)
  "Paint SHADING (dict or stream) across the current clip.  CTM maps shading space
-> device; its inverse maps each device pixel back to shading space."
  (let* ((doc (interp-doc ip))
         (dict (if (pdf-stream-p shading) (pdf-stream-dict shading) shading))
         (stype (let ((x (resolve doc (dict-get dict "ShadingType"))))
                  (if (numberp x) (round x) -1)))
         (inv (gesso:mat-invert ctm)))
    (when (and (hash-table-p dict) inv)
      (let ((evalfn (shading-color-fn ip shading dict stype)))
        (when evalfn
          (gesso:fill-callback
           (interp-ctx ip)
           (lambda (px py)
             (multiple-value-bind (ux uy)
                 (gesso:mat-apply inv (+ px 0.5d0) (+ py 0.5d0))
               (let ((rgb (funcall evalfn ux uy)))
                 (if rgb
                     (values (first rgb) (second rgb) (third rgb) 1d0)
                     (values nil nil nil 0d0)))))))))))

;;; ---- the `sh` operator (§8.7.4.5) -----------------------------------------
(defun op-sh (ip name-obj)
  "Paint the shading named NAME-OBJ (from /Resources /Shading) across the clip."
  (let* ((doc (interp-doc ip))
         (name (and (pdf-name-p name-obj) (pdf-name-string name-obj)))
         (shd (and name (interp-resources ip)
                   (resolve doc (dict-get (interp-resources ip) "Shading"))))
         (sh (and (hash-table-p shd) (resolve doc (dict-get shd name)))))
    (when (or (hash-table-p sh) (pdf-stream-p sh))
      (let ((ctx (interp-ctx ip)) (s (interp-state ip)))
        (gesso:save ctx)
        (gesso:set-global-alpha ctx (gstate2-fill-alpha s))
        (ignore-errors (paint-shading ip sh (gstate2-ctm s)))
        (gesso:restore ctx)))))

;;; ---- shading patterns (§8.7.3.3, /PatternType 2) --------------------------
(defun shading-pattern-obj (doc pat)
  "PAT if it is a shading pattern (a dict/stream with /PatternType 2), else NIL."
  (let ((d (cond ((pdf-stream-p pat) (pdf-stream-dict pat))
                 ((hash-table-p pat) pat) (t nil))))
    (when d
      (let ((pt (resolve doc (dict-get d "PatternType"))))
        (when (and (numberp pt) (= (round pt) 2)) pat)))))

(defun resolve-pattern (ip name)
  "Look up a pattern NAME in /Resources /Pattern."
  (let* ((doc (interp-doc ip))
         (pats (and (interp-resources ip)
                    (resolve doc (dict-get (interp-resources ip) "Pattern")))))
    (and (hash-table-p pats) (resolve doc (dict-get pats name)))))

(defun shading-pattern-average (ip pat)
  "A representative solid colour for shading pattern PAT (its colour at the domain
midpoint), used as a fallback when the pattern cannot be painted as a gradient
(e.g. a stroke).  NIL on failure."
  (let* ((doc (interp-doc ip))
         (pd (shading-pattern-obj doc pat)))
    (when pd
      (let* ((pdict (if (pdf-stream-p pd) (pdf-stream-dict pd) pd))
             (shading (resolve doc (dict-get pdict "Shading")))
             (sdict (and shading (if (pdf-stream-p shading) (pdf-stream-dict shading) shading))))
        (when (hash-table-p sdict)
          (let ((cvt (make-color-converter ip (dict-get sdict "ColorSpace")))
                (func (let ((f (dict-get sdict "Function"))) (and f (load-function doc f))))
                (dom (or (dict-num-array doc sdict "Domain") '(0d0 1d0))))
            (when func
              (funcall cvt (funcall func (list (/ (+ (first dom) (second dom)) 2d0)))))))))))

(defun shading-pattern-stencil-fn (ip pat img-ctm)
  "Return a closure (u v) -> (r g b) 0..255 that maps an ImageMask stencil's
unit-square coordinates to shading pattern PAT's per-pixel gradient colour, or NIL
if PAT is not a paintable shading pattern.  IMG-CTM maps the image unit square to
device; the pattern maps pattern space to device via base-CTM · /Matrix, so a
stencil painted under a shading-pattern fill (§8.7.3.3) shows the full gradient
through its coverage rather than a flat representative colour."
  (let* ((doc (interp-doc ip))
         (pd (shading-pattern-obj doc pat)))
    (when pd
      (let* ((pdict (if (pdf-stream-p pd) (pdf-stream-dict pd) pd))
             (pmatrix (or (mat-from-array doc (dict-get pdict "Matrix")) (gesso:mat-identity)))
             (shading (resolve doc (dict-get pdict "Shading")))
             (sdict (and shading (if (pdf-stream-p shading) (pdf-stream-dict shading) shading))))
        (when (hash-table-p sdict)
          (let* ((stype (let ((x (resolve doc (dict-get sdict "ShadingType"))))
                          (if (numberp x) (round x) -1)))
                 (evalfn (ignore-errors (shading-color-fn ip shading sdict stype)))
                 (base (or (interp-base-ctm ip) img-ctm))
                 (sctm (gesso:mat-mul base pmatrix))
                 (sinv (gesso:mat-invert sctm)))
            (when (and evalfn sinv)
              ;; M maps image unit-square coords -> shading space:
              ;;   device = img-ctm · (u,v) ;  shading = sinv · device
              (let ((m (gesso:mat-mul sinv img-ctm)))
                (lambda (u v)
                  (multiple-value-bind (sx sy) (gesso:mat-apply m u v)
                    (funcall evalfn sx sy)))))))))))

(defun fill-with-shading-pattern (ip pat fill-rule)
  "Fill the current path with shading pattern PAT: clip to the path (FILL-RULE) and
paint the pattern's shading.  The pattern /Matrix maps pattern space to the page's
default coordinate system, so the shading CTM is base-CTM · Matrix."
  (let* ((doc (interp-doc ip))
         (ctx (interp-ctx ip)) (s (interp-state ip))
         (pdict (if (pdf-stream-p pat) (pdf-stream-dict pat) pat))
         (pmatrix (or (mat-from-array doc (dict-get pdict "Matrix")) (gesso:mat-identity)))
         (shading (resolve doc (dict-get pdict "Shading")))
         (base (or (interp-base-ctm ip) (gstate2-ctm s)))
         (ctm (gesso:mat-mul base pmatrix)))
    (when (or (hash-table-p shading) (pdf-stream-p shading))
      (gesso:save ctx)
      (gesso:set-global-alpha ctx (gstate2-fill-alpha s))
      (gesso:clip ctx fill-rule)                 ; confine to the path
      (ignore-errors (paint-shading ip shading ctm))
      (gesso:restore ctx))))
