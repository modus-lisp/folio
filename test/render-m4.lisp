;;;; test/render-m4.lisp — render a page to PNG, report deferred image filters.
;;;; usage: sbcl --script render-m4.lisp <pdf> <page0based> <out.png> [scale]
(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :folio :verbose nil :silent t)
(in-package #:folio)

(defun rfb (p)
  (with-open-file (f p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length f) :element-type '(unsigned-byte 8))))
      (read-sequence v f) v)))

(let* ((args (cdr sb-ext:*posix-argv*))
       (pdf (first args))
       (pg (parse-integer (second args)))
       (out (third args))
       (scale (if (fourth args) (read-from-string (fourth args)) 2.0)))
  (setf *deferred-image-filters* nil)
  (let ((doc (parse-pdf (rfb pdf))))
    (format t "~&pages=~a rendering page ~a @~a~%" (page-count doc) pg scale)
    (render-page-to-png doc pg out :scale scale)
    (format t "wrote ~a~%" out)
    (when *deferred-image-filters*
      (format t "DEFERRED image filters: ~a~%" *deferred-image-filters*))))
