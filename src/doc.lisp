;;;; src/doc.lisp — the document model: catalog, page tree, page access.
;;;; (ISO 32000-1 §7.7.2 catalog, §7.7.3 page tree with inheritable attributes.)

(in-package #:folio)

;;; ---- top level --------------------------------------------------------

(defun parse-pdf (bytes)
  "Parse a PDF from the octet vector BYTES into a DOCUMENT: read the header
version, build the cross-reference table (classic + streams, /Prev chain, with a
scan fallback) and the trailer, then index the page tree."
  (let* ((v (coerce bytes '(simple-array (unsigned-byte 8) (*))))
         (doc (%make-document :bytes v)))
    (setf (document-version doc) (read-version v))
    (read-xref doc)
    (index-pages doc)
    doc))

(defun document-root (doc)
  "The document catalog dictionary (trailer /Root), resolved."
  (resolve doc (dict-get (document-trailer doc) "Root")))

;;; ---- page tree --------------------------------------------------------

(defparameter *inheritable* '("MediaBox" "CropBox" "Resources" "Rotate")
  "Page attributes inherited down the page tree (§7.7.3.4, table 30).")

(defun index-pages (doc)
  "Walk the page tree into a flat vector of leaf-page plists, caching it on DOC.
Falls back to scanning for /Type /Page objects if the tree is broken."
  (let ((out (make-array 8 :adjustable t :fill-pointer 0))
        (seen (make-hash-table)))
    (labels ((inherit (node acc)
               (loop for k in *inheritable*
                     for v = (dict-get node k)
                     when v collect (cons k v) into new
                     finally (return (append new acc))))
             (walk (node acc depth)
               (let ((node (resolve doc node)))
                 (when (and (hash-table-p node) (< depth 100))
                   (let ((kids (resolve doc (dict-get node "Kids"))))
                     (cond
                       ((and (pdf-array-p kids)
                             (not (name= (dict-get node "Type") "Page")))
                        (let ((acc2 (inherit node acc)))
                          (loop for kid across kids
                                for id = (and (pdf-ref-p kid) (pdf-ref-num kid))
                                unless (and id (gethash id seen))
                                  do (when id (setf (gethash id seen) t))
                                     (walk kid acc2 (1+ depth)))))
                       (t             ; leaf page
                        (vector-push-extend
                         (leaf-plist node (inherit node acc)) out))))))))
      (let ((root (document-root doc)))
        (when (hash-table-p root)
          (walk (dict-get root "Pages") nil 0))))
    (when (zerop (length out))
      (scan-pages doc out))
    (setf (document-pages doc) out)))

(defun leaf-plist (dict inherited)
  "Resolve a leaf page's effective (inherited-then-overridden) attributes."
  (flet ((eff (k) (or (dict-get dict k) (cdr (assoc k inherited :test #'string=)))))
    (list :dict dict
          :mediabox (eff "MediaBox")
          :cropbox  (eff "CropBox")
          :resources (eff "Resources")
          :rotate   (eff "Rotate"))))

(defun scan-pages (doc out)
  "Fallback: collect every object whose dict is /Type /Page, in object order."
  (let ((nums '()))
    (maphash (lambda (num e) (declare (ignore e)) (push num nums))
             (document-xref doc))
    (dolist (num (sort nums #'<))
      (let ((obj (resolve doc num)))
        (when (and (hash-table-p obj) (name= (dict-get obj "Type") "Page"))
          (vector-push-extend (leaf-plist obj nil) out))))))

(defun page-count (doc)
  "Number of pages in the document."
  (length (or (document-pages doc) #())))

;;; ---- page struct ------------------------------------------------------

(defstruct (page (:constructor %make-page))
  dict
  number                                ; 0-based index in the document
  mediabox cropbox rotate resources
  (%content nil)                        ; cached decoded content bytes
  %doc)

(defun get-page (doc n)
  "Return the Nth page (0-based) as a PAGE struct with inherited MediaBox /
CropBox / Rotate / Resources resolved.  Content bytes are decoded lazily."
  (let ((pages (document-pages doc)))
    (unless (and pages (< -1 n (length pages)))
      (error "folio: page index ~D out of range (0..~D)" n (1- (page-count doc))))
    (let ((pl (aref pages n)))
      (%make-page :dict (getf pl :dict)
                  :number n
                  :mediabox (rect->list doc (getf pl :mediabox)
                                        '(0d0 0d0 612d0 792d0))
                  :cropbox (rect->list doc (getf pl :cropbox) nil)
                  :rotate (let ((r (resolve doc (getf pl :rotate))))
                            (if (integerp r) (mod r 360) 0))
                  :resources (resolve doc (getf pl :resources))
                  :%doc doc))))

(defun rect->list (doc arr default)
  "Resolve a 4-element PDF rectangle to a list of doubles."
  (let ((arr (resolve doc arr)))
    (if (and (pdf-array-p arr) (>= (length arr) 4))
        (loop for i below 4
              for v = (resolve doc (aref arr i))
              collect (coerce (if (numberp v) v 0) 'double-float))
        default)))

;; PAGE-MEDIABOX / PAGE-CROPBOX / PAGE-ROTATE / PAGE-RESOURCES are the DEFSTRUCT
;; slot accessors, exported from the package.

(defun page-content-bytes (page)
  "The page's concatenated, fully DECODED content-stream bytes (§7.8.2).
/Contents may be a single stream or an array of streams joined with a newline."
  (or (page-%content page)
      (setf (page-%content page)
            (collect-content (page-%doc page) (dict-get (page-dict page) "Contents")))))

(defun collect-content (doc contents)
  (let ((contents (resolve doc contents)))
    (cond
      ((pdf-stream-p contents)
       (decode-stream contents :resolver (lambda (v) (resolve doc v))))
      ((pdf-array-p contents)
       (let ((parts '()))
         (loop for c across contents
               for s = (resolve doc c)
               when (pdf-stream-p s)
                 do (push (decode-stream s :resolver (lambda (v) (resolve doc v)))
                          parts)
                    (push #(10) parts))  ; whitespace separator between streams
         (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                (nreverse parts))))
      (t (make-array 0 :element-type '(unsigned-byte 8))))))
