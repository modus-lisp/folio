;;;; src/pattern.lisp — tiling patterns (ISO 32000-1 §8.7.3.1, /PatternType 1).
;;;;
;;;; A tiling pattern's cell is a self-contained content stream (like a form
;;;; XObject) that is painted repeatedly across the region a path fills.  When
;;;; scn/SCN selects a Pattern-colourspace name resolving to a PatternType-1
;;;; pattern, filling a path (f/F/f*/B/b) tiles the cell over the path, clipped to
;;;; the path.  Two kinds:
;;;;   PaintType 1 (colored)   — the cell draws its own colours.
;;;;   PaintType 2 (uncolored) — the cell is a stencil painted in the single colour
;;;;                             given as the extra scn operands (the interp's
;;;;                             UNCOLORED-COLOR gates colour operators in the cell).
;;;;
;;;; Approach (per tile): the path is set as the clip; the pattern-space -> device
;;;; transform is base-CTM · pattern/Matrix (pattern space maps to the page default
;;;; coordinate system, NOT the current CTM); we map the clip's device bbox back to
;;;; pattern space to find which cell origins (i·XStep, j·YStep) overlap, and for
;;;; each run the cell stream with the cell's BBox clip + its /Resources + a fresh
;;;; graphics state whose CTM is offset to that cell origin.  Everything composites
;;;; through the current alpha / blend mode / soft mask (gesso's chokepoint).

(in-package #:folio)

(defparameter *tiling-tile-cap* 20000
  "Maximum number of pattern cells painted for one fill (a guard against a tiny
XStep/YStep over a large area); clamped with a note if exceeded.")

(defun tiling-pattern-obj (doc pat)
  "PAT if it is a tiling pattern (a content STREAM with /PatternType 1), else NIL."
  (let ((d (and (pdf-stream-p pat) (pdf-stream-dict pat))))
    (when (hash-table-p d)
      (let ((pt (resolve doc (dict-get d "PatternType"))))
        (when (and (numberp pt) (= (round pt) 1)) pat)))))

(defun uncolored-pattern-p (doc pat)
  "True when PAT is a tiling pattern with /PaintType 2 (an uncolored stencil)."
  (let ((d (and (pdf-stream-p pat) (pdf-stream-dict pat))))
    (when (hash-table-p d)
      (let ((pt (resolve doc (dict-get d "PatternType")))
            (paint (resolve doc (dict-get d "PaintType"))))
        (and (numberp pt) (= (round pt) 1)
             (numberp paint) (= (round paint) 2))))))

(defun pattern-uncolored-rgb (ip base nums)
  "Interpret the numeric scn operands NUMS of an uncolored pattern in the Pattern
space's base colour space BASE, returning an 8-bit (r g b).  Defaults to black."
  (if base
      (multiple-value-bind (cvt) (make-color-converter ip base)
        (funcall cvt (mapcar (lambda (x) (float x 1d0)) nums)))
      (interpret-color (case (length nums) (1 :gray) (3 :rgb) (4 :cmyk) (t :other))
                       nil nums)))

(defun fill-with-tiling-pattern (ip pat fill-rule uncolored-color)
  "Fill the current path with tiling pattern PAT: clip to the path, then replicate
the pattern cell across the clipped region.  UNCOLORED-COLOR (an (r g b) or NIL)
is the stencil colour for a PaintType-2 pattern."
  (when (or (>= (interp-form-depth ip) *form-depth-limit*)
            (member pat (interp-form-stack ip) :test #'eq))
    (return-from fill-with-tiling-pattern))
  (let* ((doc (interp-doc ip))
         (ctx (interp-ctx ip)) (s (interp-state ip))
         (d (pdf-stream-dict pat))
         (paint-type (let ((x (resolve doc (dict-get d "PaintType"))))
                       (if (numberp x) (round x) 1)))
         (uncolored (= paint-type 2))
         (bbox (let ((b (resolve doc (dict-get d "BBox"))))
                 (when (and (pdf-array-p b) (= (length b) 4))
                   (mapcar (lambda (x) (num (resolve doc x))) (coerce b 'list)))))
         (xstep (num (resolve doc (dict-get d "XStep"))))
         (ystep (num (resolve doc (dict-get d "YStep"))))
         (pmatrix (or (mat-from-array doc (dict-get d "Matrix")) (gesso:mat-identity)))
         (pres (let ((r (resolve doc (dict-get d "Resources"))))
                 (if (hash-table-p r) r (interp-resources ip))))
         (base (or (interp-base-ctm ip) (gstate2-ctm s)))
         (pctm (gesso:mat-mul base pmatrix))
         (pinv (gesso:mat-invert pctm)))
    (unless (and bbox pinv (> (abs xstep) 1d-6) (> (abs ystep) 1d-6))
      (return-from fill-with-tiling-pattern))
    (destructuring-bind (bx0 by0 bx1 by1) bbox
      (let ((bx0 (min bx0 bx1)) (bx1 (max bx0 bx1))
            (by0 (min by0 by1)) (by1 (max by0 by1)))
        (gesso:save ctx)
        (gesso:clip ctx fill-rule)               ; confine to the filled path
        (multiple-value-bind (dx0 dy0 dx1 dy1) (gesso:clip-bounds ctx)
          (when (and (< dx0 dx1) (< dy0 dy1))
            ;; map the device clip bbox corners back to pattern space
            (let ((minx 1d30) (miny 1d30) (maxx -1d30) (maxy -1d30))
              (dolist (corner (list (cons dx0 dy0) (cons dx1 dy0)
                                    (cons dx1 dy1) (cons dx0 dy1)))
                (multiple-value-bind (ux uy) (gesso:mat-apply pinv (car corner) (cdr corner))
                  (setf minx (min minx ux) maxx (max maxx ux)
                        miny (min miny uy) maxy (max maxy uy))))
              ;; cell origins (i·XStep, j·YStep) whose BBox overlaps [minx,maxx]x[miny,maxy]
              (let* ((axs (abs xstep)) (ays (abs ystep))
                     (i0 (floor (- minx bx1) axs)) (i1 (ceiling (- maxx bx0) axs))
                     (j0 (floor (- miny by1) ays)) (j1 (ceiling (- maxy by0) ays))
                     (ni (1+ (- i1 i0))) (nj (1+ (- j1 j0)))
                     (total (* ni nj)))
                (when (> total *tiling-tile-cap*)
                  (format *error-output*
                          "~&; folio: tiling pattern ~D cells clamped to ~D~%"
                          total *tiling-tile-cap*)
                  ;; shrink the j-range symmetrically to fit the cap
                  (let ((maxj (max 1 (floor *tiling-tile-cap* (max 1 ni)))))
                    (setf j1 (min j1 (+ j0 (1- maxj))))))
                (let ((cip (make-interp
                            :ctx ctx
                            :state (%make-gstate2 :fill-alpha (gstate2-fill-alpha s)
                                                  :stroke-alpha (gstate2-fill-alpha s))
                            :resources pres
                            :doc doc
                            :base-ctm base
                            :font-cache (interp-font-cache ip)
                            :form-depth (1+ (interp-form-depth ip))
                            :form-stack (cons pat (interp-form-stack ip))
                            :uncolored-color (and uncolored
                                                  (or uncolored-color '(0 0 0)))))
                      (bytes (ignore-errors
                              (decode-stream pat :resolver (lambda (v) (resolve doc v))))))
                  (when bytes
                    (loop for jj from j0 to j1 do
                      (loop for ii from i0 to i1 do
                        (let ((cell-ctm (gesso:mat-mul
                                         pctm (gesso:mat-translate (* ii xstep)
                                                                   (* jj ystep)))))
                          (gesso:save ctx)
                          (apply #'gesso:set-transform ctx cell-ctm)
                          (setf (gstate2-ctm (interp-state cip)) (copy-list cell-ctm)
                                (interp-gstack cip) nil)
                          ;; seed the stencil colour for an uncolored cell
                          (when uncolored
                            (let ((c (or uncolored-color '(0 0 0))))
                              (gesso:set-fill ctx c) (gesso:set-stroke ctx c)))
                          ;; clip to the cell BBox (in the cell's own coordinates)
                          (clip-to-rect ctx bx0 by0 bx1 by1)
                          (ignore-errors (run-content cip bytes))
                          (gesso:restore ctx))))))))))
        (gesso:restore ctx)))))
