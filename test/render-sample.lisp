(asdf:load-system "folio")
(in-package :folio)
(defun rfb (path)
  (with-open-file (f path :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length f) :element-type '(unsigned-byte 8))))
      (read-sequence v f) v)))

(let* ((doc (parse-pdf (rfb "test/real-sample.pdf")))
       (page (get-page doc 0))
       (res (page-resources page))
       (fonts (resolve doc (dict-get res "Font"))))
  ;; report font loading
  (loop for k being the hash-keys of fonts using (hash-value v)
        for fd = (resolve doc v)
        for pf = (load-simple-font doc fd) do
    (format t "~&/~a face=~a embedded=~a upem=~a~%" k
            (if (pdf-font-face pf) "YES" "nil") (pdf-font-embedded pf) (pdf-font-upem pf))
    ;; sample a few codes
    (dolist (ch '(#\S #\a #\m #\p #\l #\e))
      (let* ((code (char-code ch))
             (gid (font-code->gid pf code))
             (w (font-code-width pf code)))
        (format t "   '~a' code=~a name=~a gid=~a w=~a~%"
                ch code (aref (pdf-font-encoding pf) code) gid w))))
  (render-page-to-png doc 0 "test/out-sample.png" :scale 2.0)
  (format t "~&wrote test/out-sample.png~%"))
