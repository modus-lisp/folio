;;;; src/image.lisp — XObjects (§8.10) and images (§8.9): the `Do` operator, form
;;;; XObject recursion, image XObjects, and inline images (BI ... ID <bytes> EI).
;;;;
;;;; A form XObject is a self-contained content stream: `Do` saves the graphics
;;;; state, concatenates the form's /Matrix onto the CTM, intersects the clip with
;;;; the form's /BBox, switches the resource scope to the form's /Resources, and
;;;; recurses the interpreter on the form's decoded bytes (with a depth limit and a
;;;; visited-set so a self-referential form cannot loop).  An image XObject (and an
;;;; inline image) is decoded to straight-alpha RGBA — filter (Flate already done
;;;; by the parser, DCT via pigment's JPEG decoder; JPX/CCITT/JBIG2 deferred),
;;;; colour space (Gray/RGB/CMYK/Indexed/ICCBased/Separation), bit depth, /Decode,
;;;; /ImageMask stencil, /SMask + /Mask alpha — then painted by mapping the unit
;;;; square through the CTM (gesso:draw-image-rgba, so rotation/skew are handled).

(in-package #:folio)

(defparameter *form-depth-limit* 15
  "Maximum nesting of form XObjects (a guard against pathological documents; the
visited-set already breaks true reference cycles).")

(defparameter *deferred-image-filters* nil
  "Bound per render to collect the names of image filters we could not decode
(JPXDecode / CCITTFaxDecode / JBIG2Decode) so the caller can report them.")

;;; ---- dict access with inline-image key abbreviations ----------------------
(defun img-dget (doc dict full abbr)
  "Fetch DICT[FULL] or its inline-image abbreviation DICT[ABBR], resolved."
  (let ((v (or (dict-get dict full) (and abbr (dict-get dict abbr)))))
    (if v (resolve doc v) nil)))

;;; ---- Do dispatch ----------------------------------------------------------
(defun op-do (ip name-obj)
  "Execute `Do /Name`: look /Name up in /Resources /XObject and dispatch on its
/Subtype (Form -> recurse, Image -> paint)."
  (let* ((doc (interp-doc ip))
         (name (and (pdf-name-p name-obj) (pdf-name-string name-obj))))
    (when name
      (let* ((xobjs (and (interp-resources ip)
                         (resolve doc (dict-get (interp-resources ip) "XObject"))))
             (xref (and (hash-table-p xobjs) (dict-get xobjs name)))
             (xobj (and xref (resolve doc xref))))
        (when (pdf-stream-p xobj)
          (let* ((d (pdf-stream-dict xobj))
                 (st (resolve doc (dict-get d "Subtype")))
                 (sub (and (pdf-name-p st) (pdf-name-string st))))
            (cond
              ((equal sub "Form")  (do-form-xobject ip xobj))
              ((equal sub "Image") (draw-image-xobject ip xobj))
              ;; some producers omit /Subtype: guess from the keys present
              ((dict-get d "BBox")  (do-form-xobject ip xobj))
              ((dict-get d "Width") (draw-image-xobject ip xobj))
              (t nil))))))))

;;; ---- form XObjects (§8.10.1) ----------------------------------------------
(defun mat-from-array (doc v)
  "A /Matrix array -> a gesso 6-tuple, or NIL if absent/malformed."
  (let ((a (resolve doc v)))
    (when (and (pdf-array-p a) (>= (length a) 6))
      (list (num (resolve doc (aref a 0))) (num (resolve doc (aref a 1)))
            (num (resolve doc (aref a 2))) (num (resolve doc (aref a 3)))
            (num (resolve doc (aref a 4))) (num (resolve doc (aref a 5)))))))

(defun clip-to-rect (ctx x0 y0 x1 y1)
  "Intersect the clip with the user-space rectangle (X0 Y0 X1 Y1)."
  (gesso:begin-path ctx)
  (gesso:move-to ctx x0 y0) (gesso:line-to ctx x1 y0)
  (gesso:line-to ctx x1 y1) (gesso:line-to ctx x0 y1) (gesso:close-path ctx)
  (gesso:clip ctx)
  (gesso:begin-path ctx))

(defun do-form-xobject (ip xobj)
  "Render a /Subtype /Form XObject: q, concat /Matrix, clip to /BBox, switch to
the form's /Resources, recurse on its content, Q.  Guarded against cycles."
  (when (or (>= (interp-form-depth ip) *form-depth-limit*)
            (member xobj (interp-form-stack ip) :test #'eq))
    (return-from do-form-xobject))
  (let* ((doc (interp-doc ip))
         (ctx (interp-ctx ip))
         (d (pdf-stream-dict xobj))
         (matrix (mat-from-array doc (dict-get d "Matrix")))
         (bbox (let ((b (resolve doc (dict-get d "BBox"))))
                 (when (and (pdf-array-p b) (= (length b) 4))
                   (mapcar (lambda (x) (num (resolve doc x))) (coerce b 'list)))))
         (fres (let ((r (resolve doc (dict-get d "Resources"))))
                 (if (hash-table-p r) r (interp-resources ip))))
         (saved-res (interp-resources ip)))
    (op-save ip)                         ; q
    (when matrix (apply #'op-cm ip matrix))
    (when bbox
      (destructuring-bind (x0 y0 x1 y1) bbox
        (clip-to-rect ctx (min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1))))
    (setf (interp-resources ip) fres)
    (push xobj (interp-form-stack ip))
    (incf (interp-form-depth ip))
    (let ((bytes (ignore-errors (decode-stream xobj :resolver (lambda (v) (resolve doc v))))))
      (when bytes (ignore-errors (run-content ip bytes))))
    (decf (interp-form-depth ip))
    (pop (interp-form-stack ip))
    (setf (interp-resources ip) saved-res)
    (op-restore ip)))                    ; Q

;;; ---- ExtGState soft masks (§11.6.5.2) -------------------------------------
;;; A soft mask is a transparency-group form XObject (/G) whose LUMINOSITY (/S
;;; /Luminosity) or ALPHA (/S /Alpha) becomes a per-pixel alpha applied to every
;;; mark painted while the mask is active.  We render /G to an offscreen canvas at
;;; the SAME device resolution and under the SAME CTM in force when `gs` set the
;;; mask (so it lands registered with the page), then reduce that offscreen to a
;;; device-space alpha array gesso multiplies into subsequent coverage.

(defun smask-backdrop-rgb (ip mdict)
  "The luminosity-group backdrop colour /BC (in the group's colour space) as an
8-bit (r g b); default black (fully masked outside the group)."
  (let* ((doc (interp-doc ip))
         (bc (resolve doc (dict-get mdict "BC"))))
    (if (pdf-array-p bc)
        (let ((vals (map 'list (lambda (x) (num (resolve doc x))) bc)))
          (case (length vals)
            (1 (gray->rgb (first vals)))
            (3 (rgb255 (first vals) (second vals) (third vals)))
            (4 (cmyk->rgb (first vals) (second vals) (third vals) (fourth vals)))
            (t '(0 0 0))))
        '(0 0 0))))

(defun extract-mask (ctx cw ch alpha-mask)
  "Reduce the offscreen group render in CTX to a CW*CH device alpha array (0..1):
the group's accumulated alpha for an /Alpha mask, else per-pixel luminosity
(0.3R+0.59G+0.11B) for a /Luminosity mask."
  (let* ((cv (gesso:context-canvas ctx))
         (n (* cw ch))
         (arr (make-array n :element-type 'double-float :initial-element 0d0)))
    (if (and alpha-mask (scribe:canvas-alpha cv))
        (let ((ap (scribe:canvas-alpha cv)))
          (dotimes (i n) (setf (aref arr i) (/ (aref ap i) 255d0))))
        (let ((px (scribe:canvas-pixels cv)))
          (dotimes (i n)
            (let ((j (* 3 i)))
              (setf (aref arr i)
                    (/ (+ (* 0.30d0 (aref px j))
                          (* 0.59d0 (aref px (+ j 1)))
                          (* 0.11d0 (aref px (+ j 2))))
                       255d0))))))
    arr))

(defun build-soft-mask (ip mask-dict)
  "Render the ExtGState /SMask MASK-DICT's transparency group /G to a device-space
alpha array (0..1) for gesso to multiply into subsequent paint.  Uses the current
canvas size and CTM.  Returns the array, or NIL on failure."
  (let* ((doc (interp-doc ip))
         (ctx (interp-ctx ip))
         (cv (gesso:context-canvas ctx))
         (cw (scribe:canvas-width cv)) (ch (scribe:canvas-height cv))
         (g (resolve doc (dict-get mask-dict "G")))
         (styp (resolve doc (dict-get mask-dict "S")))
         (subtype (and (pdf-name-p styp) (pdf-name-string styp)))
         (alpha-mask (equal subtype "Alpha"))
         (ctm (copy-list (gstate2-ctm (interp-state ip)))))
    (unless (pdf-stream-p g) (return-from build-soft-mask nil))
    (let* ((off (if alpha-mask
                    (gesso:make-context cw ch :alpha t)
                    (gesso:make-context cw ch
                                        :background (smask-backdrop-rgb ip mask-dict))))
           (oip (make-interp :ctx off
                             :state (%make-gstate2 :ctm ctm)
                             :resources (interp-resources ip)
                             :base-ctm (interp-base-ctm ip)
                             :doc doc
                             :font-cache (interp-font-cache ip)
                             :form-depth (interp-form-depth ip)
                             :form-stack (interp-form-stack ip))))
      (apply #'gesso:set-transform off ctm)
      (do-form-xobject oip g)
      (extract-mask off cw ch alpha-mask))))

;;; ---- annotation appearance streams (§12.5.5) ------------------------------
(defun annot-appearance-stream (doc annot)
  "The normal appearance (/AP /N) form XObject for ANNOT, honouring the /AS
appearance-state selector when /N is a sub-dictionary of states.  NIL if none."
  (let* ((ap (resolve doc (dict-get annot "AP")))
         (n (and (hash-table-p ap) (resolve doc (dict-get ap "N")))))
    (cond
      ((pdf-stream-p n) n)
      ((hash-table-p n)                  ; keyed by appearance state /AS
       (let* ((as (resolve doc (dict-get annot "AS")))
              (key (and (pdf-name-p as) (pdf-name-string as)))
              (sel (or (and key (resolve doc (dict-get n key)))
                       ;; no /AS: take the sole entry if there is exactly one
                       (and (= (hash-table-count n) 1)
                            (loop for v being the hash-values of n return (resolve doc v))))))
         (and (pdf-stream-p sel) sel)))
      (t nil))))

(defun annot-hidden-p (doc annot)
  "True when the annotation must not be rendered: the Hidden (2) or NoView (32)
flag is set, or it is a /Popup or /Link (link rects carry no visible ink)."
  (let* ((f (let ((x (resolve doc (dict-get annot "F")))) (if (integerp x) x 0)))
         (st (resolve doc (dict-get annot "Subtype")))
         (sub (and (pdf-name-p st) (pdf-name-string st))))
    (or (logbitp 1 f) (logbitp 5 f)
        (member sub '("Link" "Popup") :test #'equal))))

(defun appearance-transform (doc ap rect)
  "The matrix that maps an appearance stream's /Matrix-transformed /BBox onto the
annotation /Rect (§12.5.5, algorithm 8.1) -> a gesso 6-tuple, or NIL."
  (let* ((d (pdf-stream-dict ap))
         (matrix (or (mat-from-array doc (dict-get d "Matrix")) (gesso:mat-identity)))
         (b (resolve doc (dict-get d "BBox"))))
    (unless (and (pdf-array-p b) (= (length b) 4) (= (length rect) 4)
                 (every #'numberp rect))
      (return-from appearance-transform nil))
    (destructuring-bind (bx0 by0 bx1 by1)
        (mapcar (lambda (x) (num (resolve doc x))) (coerce b 'list))
      ;; transform the four BBox corners by Matrix, take the bounding box
      (let (txs tys)
        (dolist (c (list (list bx0 by0) (list bx1 by0) (list bx1 by1) (list bx0 by1)))
          (multiple-value-bind (px py) (gesso:mat-apply matrix (first c) (second c))
            (push px txs) (push py tys)))
        (let* ((tx0 (reduce #'min txs)) (tx1 (reduce #'max txs))
               (ty0 (reduce #'min tys)) (ty1 (reduce #'max tys))
               (rx0 (min (first rect) (third rect))) (rx1 (max (first rect) (third rect)))
               (ry0 (min (second rect) (fourth rect))) (ry1 (max (second rect) (fourth rect)))
               (tw (- tx1 tx0)) (th (- ty1 ty0)))
          (when (or (<= tw 0d0) (<= th 0d0)) (return-from appearance-transform nil))
          (let ((sx (/ (- rx1 rx0) tw)) (sy (/ (- ry1 ry0) th)))
            (list sx 0d0 0d0 sy (- rx0 (* sx tx0)) (- ry0 (* sy ty0)))))))))

(defun render-annotations (ip page)
  "Paint each page annotation's normal appearance stream (form fields, stamps,
checkmarks, signatures, ink, etc.) positioned into its /Rect.  Runs after the
page content, at the page base CTM (§12.5.5)."
  (let* ((doc (interp-doc ip))
         (annots (resolve doc (dict-get (page-dict page) "Annots"))))
    (when (pdf-array-p annots)
      (loop for aref across annots
            for annot = (resolve doc aref)
            when (hash-table-p annot) do
        (unless (annot-hidden-p doc annot)
          (let ((ap (annot-appearance-stream doc annot))
                (rect (let ((r (resolve doc (dict-get annot "Rect"))))
                        (when (and (pdf-array-p r) (= (length r) 4))
                          (mapcar (lambda (x) (num (resolve doc x))) (coerce r 'list))))))
            (when (and ap rect)
              (let ((a (appearance-transform doc ap rect)))
                (when a
                  (ignore-errors
                    (op-save ip)
                    (apply #'op-cm ip a)
                    (do-form-xobject ip ap)
                    (op-restore ip)))))))))))

;;; ---- image colour space ---------------------------------------------------
(defun image-color-space (ip csobj)
  "Resolve an image ColorSpace object to (values kind ncomp base).  KIND is one of
:gray :rgb :cmyk :indexed :separation.  NCOMP is the samples-per-pixel.  BASE, for
:indexed, is (list base-kind base-ncomp lut-bytes)."
  (let ((doc (interp-doc ip)))
    (setf csobj (resolve doc csobj))
    (cond
      ((null csobj) (values :gray 1 nil))
      ((pdf-name-p csobj)
       (let ((s (pdf-name-string csobj)))
         (cond
           ((member s '("DeviceGray" "G" "CalGray") :test #'string=) (values :gray 1 nil))
           ((member s '("DeviceRGB" "RGB" "CalRGB" "Lab") :test #'string=) (values :rgb 3 nil))
           ((member s '("DeviceCMYK" "CMYK") :test #'string=) (values :cmyk 4 nil))
           (t ;; a named colour space in /Resources /ColorSpace
            (let* ((csd (and (interp-resources ip)
                             (resolve doc (dict-get (interp-resources ip) "ColorSpace"))))
                   (spec (and (hash-table-p csd) (dict-get csd s))))
              (if spec (image-color-space ip spec) (values :gray 1 nil)))))))
      ((pdf-array-p csobj)
       (let ((head (and (plusp (length csobj)) (resolve doc (aref csobj 0)))))
         (if (pdf-name-p head)
             (let ((s (pdf-name-string head)))
               (cond
                 ((string= s "ICCBased")
                  (let* ((strm (resolve doc (aref csobj 1)))
                         (n (and (pdf-stream-p strm)
                                 (resolve doc (dict-get (pdf-stream-dict strm) "N")))))
                    (case n (1 (values :gray 1 nil)) (4 (values :cmyk 4 nil))
                          (t (values :rgb 3 nil)))))
                 ((member s '("CalRGB" "Lab") :test #'string=) (values :rgb 3 nil))
                 ((string= s "CalGray") (values :gray 1 nil))
                 ((string= s "Separation") (values :separation 1 nil))
                 ((string= s "DeviceN")
                  (let ((names (resolve doc (aref csobj 1))))
                    (values :separation (if (pdf-array-p names) (length names) 1) nil)))
                 ((or (string= s "Indexed") (string= s "I"))
                  (multiple-value-bind (bk bn) (image-color-space ip (aref csobj 1))
                    (let* ((lut (resolve doc (aref csobj 3)))
                           (bytes (cond ((pdf-string-p lut) (pdf-string-bytes lut))
                                        ((pdf-stream-p lut)
                                         (ignore-errors
                                          (decode-stream lut :resolver (lambda (v) (resolve doc v)))))
                                        (t nil))))
                      (values :indexed 1 (list bk bn bytes)))))
                 (t (values :rgb 3 nil))))
             (values :rgb 3 nil))))
      (t (values :gray 1 nil)))))

;;; ---- sample unpacking -----------------------------------------------------
(declaim (inline sample-ref))
(defun sample-ref (bytes row-bytes row col ncomp comp bpc)
  "Fetch one BPC-bit sample at pixel (ROW,COL) component COMP, row-major with rows
padded to a byte boundary.  Returns an integer 0..(2^BPC - 1)."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (fixnum row-bytes row col ncomp comp bpc))
  (let* ((base (the fixnum (* row row-bytes))))
    (case bpc
      (8 (let ((i (+ base (* (+ (* col ncomp) comp)))))
           (if (< i (length bytes)) (aref bytes i) 0)))
      (16 (let ((i (+ base (* 2 (+ (* col ncomp) comp)))))
            (if (< (1+ i) (length bytes))
                (logior (ash (aref bytes i) 8) (aref bytes (1+ i))) 0)))
      (t ;; 1/2/4 bit
       (let* ((bit (* (+ (* col ncomp) comp) bpc))
              (bi (+ base (floor bit 8))))
         (if (< bi (length bytes))
             (let* ((within (mod bit 8))
                    (shift (- 8 within bpc))
                    (mask (1- (ash 1 bpc))))
               (logand (ash (aref bytes bi) (- shift)) mask))
             0))))))

(defun decode-map (s maxval dmin dmax)
  "Map sample S (0..MAXVAL) to [DMIN,DMAX] per a /Decode entry (0..1 by default)."
  (+ dmin (* (- dmax dmin) (/ (float s 1d0) (float maxval 1d0)))))

;;; ---- image XObject / inline image core ------------------------------------
(defun decode-image-to-rgba (ip dict data left)
  "Turn a filter-decoded image (DATA octets, LEFT = trailing undecoded filters)
described by DICT into a straight-alpha RGBA vector.  Returns (values rgba w h)
or NIL if the image cannot be produced (deferred codec, etc.)."
  (let* ((doc (interp-doc ip))
         (w (let ((x (img-dget doc dict "Width" "W"))) (and x (round (num x)))))
         (h (let ((x (img-dget doc dict "Height" "H"))) (and x (round (num x)))))
         (mask? (let ((v (img-dget doc dict "ImageMask" "IM"))) (or (eq v :true) (eq v t)))))
    (when (or (null w) (null h) (<= w 0) (<= h 0)) (return-from decode-image-to-rgba nil))
    ;; deferred image codecs (JPX/CCITT/JBIG2): note and skip.  DCT is handled by
    ;; the caller (it needs the raw JPEG bytes, not the unpacked samples).
    (when left
      (let ((f (car left)))
        (unless (member f '("DCTDecode" "DCT") :test #'string=)
          (pushnew f *deferred-image-filters* :test #'string=)
          (return-from decode-image-to-rgba nil))))
    (let* ((bpc (if mask? 1 (let ((b (img-dget doc dict "BitsPerComponent" "BPC")))
                              (if b (round (num b)) 8))))
           ;; /Decode may be a real pdf-array (XObject image) OR a Lisp list (an
           ;; inline image, whose dict is read by the content lexer).
           (decode (let ((a (img-dget doc dict "Decode" "D")))
                     (when (or (pdf-array-p a) (consp a))
                       (map 'list (lambda (x) (num (resolve doc x))) a))))
           (rgba (make-array (* w h 4) :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
      (declare (type (simple-array (unsigned-byte 8) (*)) rgba))
      (unless (typep data '(simple-array (unsigned-byte 8) (*)))
        (setf data (coerce data '(simple-array (unsigned-byte 8) (*)))))
      (cond
        ;; ---- 1-bit stencil mask (§8.9.6.2): paint the current fill colour ----
        (mask?
         (let* ((row-bytes (ceiling w 8))
                (fill (gesso:current-fill (interp-ctx ip)))
                (fr (first fill)) (fg (second fill)) (fb (third fill))
                ;; default Decode [0 1]: a decoded value of 0 is PAINTED
                (paint-on-zero (or (null decode) (< (first decode) (second decode)))))
           (dotimes (row h)
             (dotimes (col w)
               (let* ((s (sample-ref data row-bytes row col 1 0 1))
                      (painted (if paint-on-zero (= s 0) (= s 1)))
                      (o (* 4 (+ (* row w) col))))
                 (when painted
                   (setf (aref rgba o) fr (aref rgba (+ o 1)) fg
                         (aref rgba (+ o 2)) fb (aref rgba (+ o 3)) 255)))))))
        ;; ---- ordinary sampled image ----
        (t
         (multiple-value-bind (kind ncomp base)
             (image-color-space ip (img-dget doc dict "ColorSpace" "CS"))
           (let* ((row-bytes (ceiling (* w ncomp bpc) 8))
                  (maxval (1- (ash 1 bpc)))
                  (comps (make-array (max 4 ncomp))))
             (declare (type (simple-array t (*)) comps))
             (dotimes (row h)
               (dotimes (col w)
                 (dotimes (k ncomp)
                   (setf (aref comps k) (sample-ref data row-bytes row col ncomp k bpc)))
                 (multiple-value-bind (r g b)
                     (image-pixel-rgb kind base comps ncomp maxval decode)
                   (let ((o (* 4 (+ (* row w) col))))
                     (setf (aref rgba o) r (aref rgba (+ o 1)) g
                           (aref rgba (+ o 2)) b (aref rgba (+ o 3)) 255)))))))))
      ;; ---- soft mask / stencil mask alpha ----
      (apply-soft-mask ip dict rgba w h)
      (values rgba w h))))

(defun image-pixel-rgb (kind base comps ncomp maxval decode)
  "Convert the NCOMP integer samples in COMPS (0..MAXVAL) to (values r g b)."
  (flet ((cv (k) ;; component K as a 0..1 float, honouring /Decode
           (let ((s (aref comps k)))
             (if decode
                 (decode-map s maxval (nth (* 2 k) decode) (nth (1+ (* 2 k)) decode))
                 (/ (float s 1d0) (float maxval 1d0)))))
         (b8 (x) (max 0 (min 255 (round (* 255d0 x))))))
    (case kind
      (:gray (let ((v (b8 (cv 0)))) (values v v v)))
      (:rgb (values (b8 (cv 0)) (b8 (cv 1)) (b8 (cv 2))))
      (:cmyk (let ((rgb (cmyk->rgb (cv 0) (cv 1) (cv 2) (cv 3))))
               (values (first rgb) (second rgb) (third rgb))))
      (:separation (let ((v (b8 (- 1d0 (cv 0))))) (values v v v)))
      (:indexed
       (destructuring-bind (bk bn lut) base
         (let* ((idx (aref comps 0))       ; palette index = raw sample
                (bn (or bn (components-per (or bk :rgb)))))
           (if (and lut (<= (* (1+ idx) bn) (length lut)))
               (let ((rgb (interpret-color (or bk :rgb) nil
                                           (loop for i below bn
                                                 collect (/ (aref lut (+ (* idx bn) i)) 255d0)))))
                 (values (first rgb) (second rgb) (third rgb)))
               (values 0 0 0)))))
      (t ;; unknown: best effort by component count
       (case ncomp
         (1 (let ((v (b8 (cv 0)))) (values v v v)))
         (3 (values (b8 (cv 0)) (b8 (cv 1)) (b8 (cv 2))))
         (4 (let ((rgb (cmyk->rgb (cv 0) (cv 1) (cv 2) (cv 3))))
              (values (first rgb) (second rgb) (third rgb))))
         (t (values 0 0 0)))))))

;;; ---- soft-mask / mask alpha (§11.6.5.2, §8.9.6.3) --------------------------
(defun apply-soft-mask (ip dict rgba w h)
  "If DICT carries an /SMask (grayscale alpha) or a stencil /Mask, resample it to
W x H and fold it into the RGBA alpha channel."
  (let* ((doc (interp-doc ip))
         (sm (resolve doc (dict-get dict "SMask")))
         (mk (resolve doc (dict-get dict "Mask"))))
    (cond
      ((pdf-stream-p sm) (fold-mask ip rgba w h sm nil))
      ((pdf-stream-p mk) (fold-mask ip rgba w h mk t))
      ;; colour-key masking (/Mask array): minimal — left unhandled beyond note
      (t nil))))

(defun fold-mask (ip rgba w h mstream stencil)
  "Multiply the RGBA alpha by the mask image MSTREAM (resampled to W x H).  When
STENCIL, MSTREAM is a 1-bit /Mask (sample 1 => hidden per default Decode); else it
is an /SMask giving alpha directly from its grayscale samples."
  (let* ((doc (interp-doc ip))
         (md (pdf-stream-dict mstream)))
    (multiple-value-bind (mdata left)
        (decode-stream mstream :resolver (lambda (v) (resolve doc v)))
      (when left (return-from fold-mask))   ; can't decode the mask's codec
      (unless (typep mdata '(simple-array (unsigned-byte 8) (*)))
        (setf mdata (coerce mdata '(simple-array (unsigned-byte 8) (*)))))
      (let* ((mw (round (num (img-dget doc md "Width" "W"))))
             (mh (round (num (img-dget doc md "Height" "H"))))
             (mbpc (if stencil 1 (let ((b (img-dget doc md "BitsPerComponent" "BPC")))
                                   (if b (round (num b)) 8))))
             (mmax (1- (ash 1 mbpc)))
             (mrow (ceiling (* mw mbpc) 8))
             (decode (let ((a (img-dget doc md "Decode" "D")))
                       (when (pdf-array-p a)
                         (map 'list (lambda (x) (num (resolve doc x))) a))))
             (invert (and stencil decode (> (first decode) (second decode)))))
        (when (or (<= mw 0) (<= mh 0)) (return-from fold-mask))
        (dotimes (row h)
          (let ((mrY (min (1- mh) (floor (* row mh) h))))
            (dotimes (col w)
              (let* ((mrX (min (1- mw) (floor (* col mw) w)))
                     (s (sample-ref mdata mrow mrY mrX 1 0 mbpc))
                     (o (* 4 (+ (* row w) col)))
                     (alpha (if stencil
                                ;; default: sample 1 hides, 0 shows
                                (if invert (if (= s 1) 255 0) (if (= s 1) 0 255))
                                (round (* 255d0 (/ (float s 1d0) (float mmax 1d0)))))))
                (setf (aref rgba (+ o 3))
                      (round (* (aref rgba (+ o 3)) alpha) 255))))))))))

;;; ---- CCITT fax images (§7.4.6) --------------------------------------------
(defun ccitt-decode-image (ip dict raw)
  "Decode the CCITTFaxDecode-filtered RAW bytes of the image DICT into a packed
1-bit-per-pixel DeviceGray sample buffer (row-major, MSB-first, byte-padded rows)
that the ordinary BPC-1 image path consumes.  Reads /DecodeParms (or the inline
abbreviation /DP): /K /Columns /Rows /BlackIs1 /EncodedByteAlign.  NIL on failure."
  (let* ((doc (interp-doc ip))
         (parms (let ((p (resolve doc (or (dict-get dict "DecodeParms")
                                          (dict-get dict "DP")))))
                  (cond ((hash-table-p p) p)
                        ((pdf-array-p p)     ; multi-filter: the last dict entry
                         (loop for i from (1- (length p)) downto 0
                               for e = (resolve doc (aref p i))
                               when (hash-table-p e) return e))
                        (t nil))))
         (width (let ((x (img-dget doc dict "Width" "W"))) (and x (round (num x)))))
         (height (let ((x (img-dget doc dict "Height" "H"))) (and x (round (num x)))))
         (pget (lambda (full abbr default)
                 (let ((v (and (hash-table-p parms)
                               (resolve doc (or (dict-get parms full)
                                                (and abbr (dict-get parms abbr)))))))
                   (if (or (null v) (pdf-null-p v)) default v))))
         (k (let ((v (funcall pget "K" nil 0))) (if (numberp v) (round v) 0)))
         (columns (let ((v (funcall pget "Columns" nil nil)))
                    (cond ((numberp v) (round v)) (width width) (t 1728))))
         (rows (let ((v (funcall pget "Rows" nil nil)))
                 (cond ((numberp v) (round v)) (height height) (t 0))))
         (black1 (let ((v (funcall pget "BlackIs1" nil nil))) (or (eq v :true) (eq v t))))
         (align (let ((v (funcall pget "EncodedByteAlign" nil nil)))
                  (or (eq v :true) (eq v t)))))
    (when (and columns (> columns 0))
      (ignore-errors
       (values (ccitt-decode raw :columns columns :rows (or rows 0)
                                 :k k :black-is-1 black1 :byte-align align))))))

;;; ---- JBIG2 fax images (§7.4.7) --------------------------------------------
(defun jbig2-decode-image (ip dict raw)
  "Decode the JBIG2Decode-filtered RAW bytes of image DICT into a packed 1-bit
DeviceGray sample buffer.  Reads /DecodeParms /JBIG2Globals (a shared segment
stream) if present.  NIL on failure."
  (let* ((doc (interp-doc ip))
         (parms (let ((p (resolve doc (or (dict-get dict "DecodeParms")
                                          (dict-get dict "DP")))))
                  (cond ((hash-table-p p) p)
                        ((pdf-array-p p)
                         (loop for i from (1- (length p)) downto 0
                               for e = (resolve doc (aref p i))
                               when (hash-table-p e) return e))
                        (t nil))))
         (gstream (and (hash-table-p parms)
                       (resolve doc (dict-get parms "JBIG2Globals"))))
         (globals (when (pdf-stream-p gstream)
                    (ignore-errors
                     (nth-value 0 (decode-stream gstream
                                                 :resolver (lambda (v) (resolve doc v)))))))
         (width (let ((x (img-dget doc dict "Width" "W"))) (and x (round (num x)))))
         (height (let ((x (img-dget doc dict "Height" "H"))) (and x (round (num x))))))
    (when (and width height (> width 0) (> height 0))
      (ignore-errors (jbig2-decode raw globals width height)))))

;;; ---- painting an image XObject --------------------------------------------
(defun draw-image-xobject (ip xobj)
  "Decode image XObject XOBJ and paint it through the CTM."
  (let* ((doc (interp-doc ip))
         (dict (pdf-stream-dict xobj)))
    (multiple-value-bind (data left)
        (decode-stream xobj :resolver (lambda (v) (resolve doc v)))
      (paint-decoded-image ip dict data left))))

(defun blit-rgba (ip rgba w h)
  "Draw an RGBA image through the CTM, honouring the current fill constant-alpha."
  (let ((ctx (interp-ctx ip)))
    (gesso:set-global-alpha ctx (gstate2-fill-alpha (interp-state ip)))
    (gesso:draw-image-rgba ctx rgba w h)
    (gesso:set-global-alpha ctx 1d0)))

(defun paint-decoded-image (ip dict data left)
  "Shared by image XObjects and inline images: build RGBA (DCT via pigment, else
sample-unpacked) and blit it through the CTM onto the canvas."
  (let ((doc (interp-doc ip)))
    (cond
      ;; ---- DCTDecode: hand the raw JPEG bytes to pigment ----
      ((and left (member (car left) '("DCTDecode" "DCT") :test #'string=))
       (let ((img (ignore-errors (pigment:jpeg-decode
                                  (if (typep data '(simple-array (unsigned-byte 8) (*)))
                                      data (coerce data '(simple-array (unsigned-byte 8) (*))))))))
         (if (typep img 'pigment:img)
             (let ((rgba (pigment:img-rgba img))
                   (w (pigment:img-w img)) (h (pigment:img-h img)))
               ;; a JPEG can carry an /SMask that lives outside the codec
               (apply-soft-mask ip dict rgba w h)
               (blit-rgba ip rgba w h))
             (pushnew "DCTDecode" *deferred-image-filters* :test #'string=))))
      ;; ---- CCITT Group 3/4 fax: decode to a 1-bit sample buffer, then run the
      ;;      ordinary BPC-1 / colour-space / ImageMask / Decode path ----
      ((and left (member (car left) '("CCITTFaxDecode" "CCF") :test #'string=))
       (let ((bits (ccitt-decode-image ip dict data)))
         (if bits
             (multiple-value-bind (rgba w h) (decode-image-to-rgba ip dict bits nil)
               (when rgba (blit-rgba ip rgba w h)))
             (pushnew (car left) *deferred-image-filters* :test #'string=))))
      ;; ---- JBIG2: decode the bilevel segments to a 1-bit sample buffer, then
      ;;      run the ordinary BPC-1 / colour-space / ImageMask / Decode path ----
      ((and left (member (car left) '("JBIG2Decode") :test #'string=))
       (let ((bits (jbig2-decode-image ip dict data)))
         (if bits
             (multiple-value-bind (rgba w h) (decode-image-to-rgba ip dict bits nil)
               (when rgba (blit-rgba ip rgba w h)))
             (pushnew (car left) *deferred-image-filters* :test #'string=))))
      ;; ---- deferred codecs ----
      ((and left (member (car left)
                         '("JPXDecode") :test #'string=))
       (pushnew (car left) *deferred-image-filters* :test #'string=))
      ;; ---- Flate/raw samples ----
      (t
       (multiple-value-bind (rgba w h) (decode-image-to-rgba ip dict data left)
         (when rgba
           (blit-rgba ip rgba w h)))))))

;;; ---- inline images (BI <dict> ID <bytes> EI) ------------------------------
(defun read-inline-dict (lx)
  "LX just past BI: read key/value pairs into a name-keyed hash-table, stopping at
the ID keyword.  Leaves LX positioned just after ID."
  (let ((h (make-hash-table :test 'equal)))
    (loop for tok = (lex-next lx)
          until (or (eq (token-type tok) :eof)
                    (and (eq (token-type tok) :keyword)
                         (string= (token-value tok) "ID")))
          do (when (eq (token-type tok) :name)
               (setf (gethash (token-value tok) h) (read-one-operand lx))))
    h))

(defun inline-ncomp (ip dict)
  "Samples-per-pixel for an inline image (for computing its unfiltered length)."
  (let ((doc (interp-doc ip)))
    (if (let ((v (img-dget doc dict "ImageMask" "IM"))) (or (eq v :true) (eq v t)))
        1
        (nth-value 1 (image-color-space ip (img-dget doc dict "ColorSpace" "CS"))))))

(defun do-inline-image (ip lx)
  "Parse an inline image at LX (positioned after BI), paint it, and leave LX just
past the terminating EI."
  (let* ((doc (interp-doc ip))
         (dict (read-inline-dict lx))
         (buf (lexer-buf lx)) (end (lexer-end lx)) (p (lexer-pos lx)))
    ;; one whitespace byte separates ID from the binary data
    (when (and (< p end) (ws-byte-p (aref buf p))) (incf p))
    (let* ((data-start p)
           (has-filter (or (dict-get dict "F") (dict-get dict "Filter")))
           data-end)
      (if (not has-filter)
          ;; unfiltered: exact length = rows * ceil(w*ncomp*bpc/8)
          (let* ((w (round (num (img-dget doc dict "Width" "W"))))
                 (h (round (num (img-dget doc dict "Height" "H"))))
                 (mask? (let ((v (img-dget doc dict "ImageMask" "IM")))
                          (or (eq v :true) (eq v t))))
                 (bpc (if mask? 1 (let ((b (img-dget doc dict "BitsPerComponent" "BPC")))
                                    (if b (round (num b)) 8))))
                 (ncomp (inline-ncomp ip dict))
                 (row-bytes (ceiling (* w ncomp bpc) 8))
                 (len (* row-bytes h)))
            (setf data-end (min end (+ data-start len)))
            ;; advance past the data and the trailing EI (+ optional whitespace)
            (let ((q data-end))
              (loop while (and (< q end) (ws-byte-p (aref buf q))) do (incf q))
              (when (and (< (1+ q) end) (= (aref buf q) 69) (= (aref buf (1+ q)) 73))
                (setf q (+ q 2)))
              (setf (lexer-pos lx) q (lexer-peeked lx) nil)))
          ;; filtered: scan for a whitespace-delimited EI
          (progn
            (loop with q = data-start
                  while (< (1+ q) end) do
                    (if (and (= (aref buf q) 69) (= (aref buf (1+ q)) 73) ; E I
                             (or (= q 0) (ws-byte-p (aref buf (1- q))))
                             (or (>= (+ q 2) end) (ws-byte-p (aref buf (+ q 2)))
                                 (delim-byte-p (aref buf (+ q 2)))))
                        (progn (setf data-end q
                                     (lexer-pos lx) (+ q 2) (lexer-peeked lx) nil)
                               (return))
                        (incf q))
                  finally (setf data-end end
                                (lexer-pos lx) end (lexer-peeked lx) nil))))
      ;; build a stream over the captured bytes and paint it
      (let* ((raw (subseq buf data-start (or data-end data-start)))
             (strm (make-pdf-stream dict raw)))
        (multiple-value-bind (dec left)
            (ignore-errors (decode-stream strm :resolver (lambda (v) (resolve doc v))))
          (when dec (paint-decoded-image ip dict dec left)))))))
