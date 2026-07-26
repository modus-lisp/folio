;;;; src/render.lisp — page rasterization: device transform + the render entry.
;;;;
;;;; PDF user space is y-UP with the origin at the MediaBox lower-left; the output
;;;; canvas is y-DOWN with the origin at the top-left.  We build a base CTM that
;;;; maps a page point through: translate the MediaBox origin to 0, flip y, apply
;;;; the page /Rotate (0/90/180/270, clockwise), and scale to output pixels.  That
;;;; base CTM seeds gesso's transform; the content stream then concatenates `cm`.

(in-package #:folio)

(defun page-render-box (page)
  "The clipping rectangle used for rendering: the CropBox if present and sane,
else the MediaBox.  Returns (x0 y0 x1 y1) as doubles, normalised so x0<x1,y0<y1."
  (let* ((mb (page-mediabox page))
         (cb (page-cropbox page))
         (box (if (and (consp cb) (= (length cb) 4)) cb mb)))
    (destructuring-bind (x0 y0 x1 y1) box
      (list (min x0 x1) (min y0 y1) (max x0 x1) (max y0 y1)))))

(defun device-transform (x0 y0 x1 y1 rotate scale)
  "Return (values matrix width height): the base user->device CTM (a gesso
6-tuple) plus the output canvas pixel size, for a page box (X0 Y0 X1 Y1) rotated
ROTATE degrees (0/90/180/270 clockwise) and rendered at SCALE device units/pt."
  (let* ((pw (- x1 x0)) (ph (- y1 y0))
         (s (float scale 1d0))
         (wp (* pw s)) (hp (* ph s))
         ;; P: page point -> unrotated device coords (y-down, [0,wp]x[0,hp]).
         ;; x' = s*x - s*x0 ; y' = -s*y + s*y1
         (p (list s 0d0 0d0 (- s) (* (- s) x0) (* s y1))))
    (multiple-value-bind (rot cw ch)
        (ecase (mod (or rotate 0) 360)
          (0   (values (gesso:mat-identity) wp hp))
          (90  (values (list 0d0 1d0 -1d0 0d0 hp 0d0) hp wp))
          (180 (values (list -1d0 0d0 0d0 -1d0 wp hp) wp hp))
          (270 (values (list 0d0 -1d0 1d0 0d0 0d0 wp) hp wp)))
      (values (gesso:mat-mul rot p)
              (max 1 (ceiling (- cw 1d-6)))
              (max 1 (ceiling (- ch 1d-6)))))))

(defun render-page (doc n &key (scale 1.0))
  "Render page N (0-based) of DOC to a straight-alpha RGBA PIGMENT:IMG at SCALE
device units per PDF point.  Vector content (paths, fills, strokes, clips, colour
under the CTM) is painted; text and images are parsed but not yet drawn."
  (let* ((page (get-page doc n))
         (box (page-render-box page)))
    (destructuring-bind (x0 y0 x1 y1) box
      (multiple-value-bind (m cw ch)
          (device-transform x0 y0 x1 y1 (page-rotate page) scale)
        (let* ((ctx (gesso:make-context cw ch :background '(255 255 255)))
               (ip (make-interp :ctx ctx
                                :state (%make-gstate2 :ctm (copy-list m))
                                :resources (page-resources page)
                                :doc doc)))
          ;; seed gesso's transform with the base device CTM
          (apply #'gesso:set-transform ctx m)
          ;; clip to the render box so content outside the crop is not painted
          (clip-to-box ctx x0 y0 x1 y1)
          (ignore-errors (run-content ip (page-content-bytes page)))
          (canvas->img (gesso:context-canvas ctx)))))))

(defun clip-to-box (ctx x0 y0 x1 y1)
  "Intersect the clip with the page render box (in user space, via the CTM)."
  (gesso:begin-path ctx)
  (gesso:move-to ctx x0 y0) (gesso:line-to ctx x1 y0)
  (gesso:line-to ctx x1 y1) (gesso:line-to ctx x0 y1) (gesso:close-path ctx)
  (gesso:clip ctx)
  (gesso:begin-path ctx))

(defun canvas->img (cv)
  "Convert an opaque scribe canvas to a straight-alpha (opaque) PIGMENT:IMG."
  (if (scribe:canvas-alpha cv)
      (pigment:rgba-canvas->img cv)
      (let* ((w (scribe:canvas-width cv)) (h (scribe:canvas-height cv))
             (px (scribe:canvas-pixels cv))
             (rgba (make-array (* w h 4) :element-type '(unsigned-byte 8))))
        (dotimes (j (* w h))
          (let ((i3 (* 3 j)) (i4 (* 4 j)))
            (setf (aref rgba i4)       (aref px i3)
                  (aref rgba (+ i4 1)) (aref px (+ i3 1))
                  (aref rgba (+ i4 2)) (aref px (+ i3 2))
                  (aref rgba (+ i4 3)) 255)))
        (pigment:make-img :w w :h h :rgba rgba))))

(defun render-page-to-png (doc n path &key (scale 1.0))
  "Render page N of DOC and write it to PATH as a PNG.  Returns PATH."
  (let* ((page (get-page doc n))
         (box (page-render-box page)))
    (destructuring-bind (x0 y0 x1 y1) box
      (multiple-value-bind (m cw ch)
          (device-transform x0 y0 x1 y1 (page-rotate page) scale)
        (let* ((ctx (gesso:make-context cw ch :background '(255 255 255)))
               (ip (make-interp :ctx ctx
                                :state (%make-gstate2 :ctm (copy-list m))
                                :resources (page-resources page)
                                :doc doc)))
          (apply #'gesso:set-transform ctx m)
          (clip-to-box ctx x0 y0 x1 y1)
          (ignore-errors (run-content ip (page-content-bytes page)))
          (gesso:write-png ctx path)
          path)))))
