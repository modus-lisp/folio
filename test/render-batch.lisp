;;;; test/render-batch.lisp — batch page renderer for the conformance harness.
;;;;
;;;; usage: sbcl --script render-batch.lisp <manifest.txt> <out-dir>
;;;;
;;;; Each manifest line: <name> <pdf-path> <page0> <scale>
;;;; Writes <out-dir>/folio-<name>-p<page>.png and prints one status line per
;;;; render: STATUS name page ok|err  enc=<status>  deferred=<filters>  ms=<t>
;;;; A render never aborts the batch (each page is guarded).

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(ql:quickload :folio :verbose nil :silent t)
(in-package #:folio)

(defun rfb (p)
  (with-open-file (f p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length f) :element-type '(unsigned-byte 8))))
      (read-sequence v f) v)))

(defun split-ws (line)
  (let ((out '()) (start nil))
    (dotimes (i (length line))
      (let ((ws (member (char line i) '(#\Space #\Tab))))
        (cond ((and (not ws) (not start)) (setf start i))
              ((and ws start) (push (subseq line start i) out) (setf start nil)))))
    (when start (push (subseq line start) out))
    (nreverse out)))

(let* ((args (cdr sb-ext:*posix-argv*))
       (manifest (first args))
       (out-dir (second args)))
  (with-open-file (mf manifest)
    (loop for line = (read-line mf nil nil) while line do
      (let ((line (string-trim '(#\Space #\Tab #\Return) line)))
        (when (and (> (length line) 0) (not (char= (char line 0) #\#)))
          (let* ((parts (split-ws line))
                 (name (first parts))
                 (pdf (second parts))
                 (page (parse-integer (third parts)))
                 (scale (read-from-string (fourth parts)))
                 (out (format nil "~a/folio-~a-p~a.png" out-dir name page))
                 (t0 (get-internal-real-time)))
            (setf *deferred-image-filters* nil)
            (handler-case
                (let ((doc (parse-pdf (rfb pdf))))
                  (let ((status (encryption-status doc))
                        (npages (page-count doc)))
                    (if (< page npages)
                        (progn
                          (render-page-to-png doc page out :scale scale)
                          (format t "STATUS ~a ~a ok enc=~a deferred=~a pages=~a ms=~a~%"
                                  name page status
                                  (or *deferred-image-filters* "-") npages
                                  (round (* 1000 (/ (- (get-internal-real-time) t0)
                                                    internal-time-units-per-second)))))
                        (format t "STATUS ~a ~a err=nopage enc=~a pages=~a~%"
                                name page status npages))))
              (error (e)
                (format t "STATUS ~a ~a err=~a~%" name page
                        (substitute #\Space #\Newline (format nil "~a" e)))))
            (finish-output)))))))
