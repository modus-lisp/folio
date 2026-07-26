;;;; test/dump.lisp — load folio and dump parse results for each test PDF.
;;;; Run:  sbcl --script test/dump.lisp
(handler-bind ((warning #'muffle-warning))
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
  (funcall (read-from-string "ql:quickload") "folio" :silent t))

(in-package #:folio)

(defun read-file-bytes (path)
  (with-open-file (f path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length f) :element-type '(unsigned-byte 8))))
      (read-sequence v f)
      v)))

(defun printable (bytes n)
  "First N bytes as a readable string (non-printables shown as '.')."
  (with-output-to-string (s)
    (loop for i below (min n (length bytes))
          for b = (aref bytes i)
          do (write-char (if (<= 32 b 126) (code-char b) #\.) s))))

(defun dump (path)
  (format t "~%========================================================~%")
  (format t "FILE: ~a~%" (file-namestring path))
  (handler-case
      (let* ((bytes (read-file-bytes path))
             (doc (parse-pdf bytes)))
        (format t "  PDF version    : ~a~a~%" (document-version doc)
                (if (document-recovered doc) "  (xref recovered by scan)" ""))
        (format t "  object count   : ~D~%" (object-count doc))
        (format t "  page count     : ~D~%" (page-count doc))
        (when (plusp (page-count doc))
          (let* ((pg (get-page doc 0))
                 (content (page-content-bytes pg)))
            (format t "  page-1 MediaBox: ~a~%" (page-mediabox pg))
            (format t "  page-1 Rotate  : ~a~%" (page-rotate pg))
            (format t "  page-1 content : ~D bytes decoded~%" (length content))
            (format t "  first ~D bytes : |~a|~%"
                    (min 200 (length content)) (printable content 200)))))
    (error (e) (format t "  !! PARSE ERROR: ~a~%" e))))

(let ((dir (merge-pathnames "" *load-pathname*)))
  (dolist (n '("a-rect.pdf" "b-flate.pdf" "c-xrefstm.pdf" "d-array.pdf"))
    (dump (merge-pathnames n dir)))
  ;; any real PDFs dropped alongside
  (dolist (p (directory (merge-pathnames "real-*.pdf" dir)))
    (dump p)))
(format t "~%done.~%")
