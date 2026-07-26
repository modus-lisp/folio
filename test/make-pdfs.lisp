;;;; test/make-pdfs.lisp — generate the hand-authored test PDFs.
;;;; Run:  sbcl --script test/make-pdfs.lisp
;;;; Emits four minimal PDFs with byte-correct classic xref tables (and one
;;;; PDF-1.5 xref-stream + object-stream file) into test/.

(require :sb-posix)
(load (merge-pathnames "cram-load.lisp" *load-pathname*) :if-does-not-exist nil)
(handler-bind ((warning #'muffle-warning))
  (let ((ql (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file ql) (load ql))))
(handler-bind ((warning #'muffle-warning))
  (funcall (read-from-string "ql:quickload") "cram" :silent t))

(defpackage #:folio-mkpdf (:use #:cl))
(in-package #:folio-mkpdf)

(defun s->o (s) (map '(vector (unsigned-byte 8)) #'char-code s))

(defun write-pdf (path parts)
  "PARTS is a list of strings/octet-vectors; write concatenated to PATH."
  (with-open-file (f path :direction :output :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (dolist (p parts)
      (let ((o (if (stringp p) (s->o p) p)))
        (write-sequence o f)))))

;;; Build a classic-xref single-file PDF from a list of object BODY strings.
;;; Objects are numbered 1..N in list order.  Returns the full byte string.
(defun build-classic (objects root-num &key (version "1.7"))
  "OBJECTS: list of strings, each the FULL `N 0 obj ... endobj\\n' text.
Computes the xref offsets from the running byte length."
  (let* ((header (format nil "%PDF-~a~%%~c~c~c~c~%" version (code-char 226) (code-char 227) (code-char 207) (code-char 211)))
         (buf (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (offsets '()))
    (flet ((emit (thing)
             (let ((o (if (stringp thing) (s->o thing) thing)))
               (loop for b across o do (vector-push-extend b buf)))))
      (emit header)
      (loop for body in objects
            do (push (fill-pointer buf) offsets)
               (emit body))
      (setf offsets (nreverse offsets))
      (let* ((n (length objects))
             (xref-start (fill-pointer buf))
             (xref (with-output-to-string (x)
                     (format x "xref~%0 ~D~%" (1+ n))
                     (format x "0000000000 65535 f ~c~c" #\Return #\Linefeed)
                     (dolist (off offsets)
                       (format x "~10,'0D 00000 n ~c~c" off #\Return #\Linefeed))))
             (trailer (format nil "trailer~%<< /Size ~D /Root ~D 0 R >>~%startxref~%~D~%%%EOF~%"
                              (1+ n) root-num xref-start)))
        (emit xref)
        (emit trailer)))
    (subseq buf 0 (fill-pointer buf))))

;;; ---- (a) uncompressed content stream: a filled rectangle --------------
(defun make-a ()
  (let* ((content "q 1 0 0 RG 0 0 1 rg 100 100 200 150 re f Q")
         (objs (list
                (format nil "1 0 obj~%<< /Type /Catalog /Pages 2 0 R >>~%endobj~%")
                (format nil "2 0 obj~%<< /Type /Pages /Kids [3 0 R] /Count 1 >>~%endobj~%")
                (format nil "3 0 obj~%<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 4 0 R /Resources << >> >>~%endobj~%")
                (format nil "4 0 obj~%<< /Length ~D >>~%stream~%~a~%endstream~%endobj~%"
                        (length content) content))))
    (build-classic objs 1)))

;;; ---- (b) FlateDecode-compressed content stream ------------------------
(defun make-b ()
  (let* ((content (s->o "q 0 1 0 rg 50 50 300 400 re f Q"))
         (comp (funcall (read-from-string "cram:zlib-compress") content))
         (objs (list
                (format nil "1 0 obj~%<< /Type /Catalog /Pages 2 0 R >>~%endobj~%")
                (format nil "2 0 obj~%<< /Type /Pages /Kids [3 0 R] /Count 1 >>~%endobj~%")
                (format nil "3 0 obj~%<< /Type /Page /Parent 2 0 R /MediaBox [0 0 400 500] /Contents 4 0 R >>~%endobj~%")
                (concatenate '(vector (unsigned-byte 8))
                             (s->o (format nil "4 0 obj~%<< /Length ~D /Filter /FlateDecode >>~%stream~%" (length comp)))
                             comp
                             (s->o (format nil "~%endstream~%endobj~%"))))))
    (build-classic objs 1)))

;;; ---- (d) /Contents as an array of two streams -------------------------
(defun make-d ()
  (let* ((c1 "q 1 0 0 rg 10 10 100 100 re f Q")
         (c2 "q 0 0 1 rg 200 200 150 150 re f Q")
         (objs (list
                (format nil "1 0 obj~%<< /Type /Catalog /Pages 2 0 R >>~%endobj~%")
                (format nil "2 0 obj~%<< /Type /Pages /Kids [3 0 R] /Count 1 >>~%endobj~%")
                (format nil "3 0 obj~%<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents [4 0 R 5 0 R] >>~%endobj~%")
                (format nil "4 0 obj~%<< /Length ~D >>~%stream~%~a~%endstream~%endobj~%" (length c1) c1)
                (format nil "5 0 obj~%<< /Length ~D >>~%stream~%~a~%endstream~%endobj~%" (length c2) c2))))
    (build-classic objs 1)))

;;; ---- (c) PDF-1.5 cross-reference STREAM + object stream ---------------
;;; Objects 1 (Catalog) and 2 (Pages) live inside an object stream (obj 5).
;;; Object 3 is the Page (uncompressed).  Object 4 is the content stream.
;;; Object 6 is the XRef stream.  We build byte offsets by hand.
(defun be (n width)
  (let ((v (make-array width :element-type '(unsigned-byte 8))))
    (dotimes (i width v) (setf (aref v (- width 1 i)) (logand (ash n (* -8 i)) #xff)))))

(defun make-c ()
  (let* ((buf (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (offs (make-hash-table)))       ; obj-num -> byte offset (for uncompressed)
    (flet ((emit (thing)
             (let ((o (if (stringp thing) (s->o thing) thing)))
               (loop for b across o do (vector-push-extend b buf))))
           (mark (num) (setf (gethash num offs) (fill-pointer buf))))
      ;; header
      (emit (format nil "%PDF-1.5~%%~c~c~c~c~%" (code-char 226) (code-char 227) (code-char 207) (code-char 211)))
      ;; --- object stream (obj 5) holding objects 1 and 2 ---
      (let* ((o1 "<< /Type /Catalog /Pages 2 0 R >>")
             (o2 "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
             (off1 0)
             (off2 (length (s->o (concatenate 'string o1 " "))))
             (header-str (format nil "1 ~D 2 ~D " off1 off2))
             (body (concatenate 'string header-str o1 " " o2))
             (raw (s->o body))
             (comp (funcall (read-from-string "cram:zlib-compress") raw))
             (first (length (s->o header-str))))
        (mark 5)
        (emit (format nil "5 0 obj~%<< /Type /ObjStm /N 2 /First ~D /Length ~D /Filter /FlateDecode >>~%stream~%"
                      first (length comp)))
        (emit comp)
        (emit (format nil "~%endstream~%endobj~%"))
        ;; --- object 4: content stream (uncompressed) ---
        (let ((content "q .8 0 .2 rg 40 40 500 700 re f Q"))
          (mark 4)
          (emit (format nil "4 0 obj~%<< /Length ~D >>~%stream~%~a~%endstream~%endobj~%"
                        (length content) content)))
        ;; --- object 3: the Page ---
        (mark 3)
        (emit (format nil "3 0 obj~%<< /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842] /Contents 4 0 R /Resources << >> >>~%endobj~%"))
        ;; --- object 6: the XRef stream ---
        (let* ((xref-off (fill-pointer buf))
               ;; W = [1 3 1]; entries for objects 0..6
               (w1 1) (w2 3) (w3 1)
               (entries
                 (list
                  (list 0 0 255)                          ; 0: free head
                  (list 2 5 0)                            ; 1: in objstm 5, idx 0
                  (list 2 5 1)                            ; 2: in objstm 5, idx 1
                  (list 1 (gethash 3 offs) 0)            ; 3: offset
                  (list 1 (gethash 4 offs) 0)            ; 4: offset
                  (list 1 (gethash 5 offs) 0)            ; 5: offset (the objstm)
                  (list 1 xref-off 0)))                  ; 6: this xref stream
               (xdata (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
          (dolist (e entries)
            (destructuring-bind (f1 f2 f3) e
              (loop for b across (be f1 w1) do (vector-push-extend b xdata))
              (loop for b across (be f2 w2) do (vector-push-extend b xdata))
              (loop for b across (be f3 w3) do (vector-push-extend b xdata))))
          (let ((xcomp (funcall (read-from-string "cram:zlib-compress")
                                (subseq xdata 0 (fill-pointer xdata)))))
            (emit (format nil "6 0 obj~%<< /Type /XRef /Size 7 /Root 1 0 R /W [1 3 1] /Length ~D /Filter /FlateDecode >>~%stream~%"
                          (length xcomp)))
            (emit xcomp)
            (emit (format nil "~%endstream~%endobj~%")))
          (emit (format nil "startxref~%~D~%%%EOF~%" xref-off)))))
    (subseq buf 0 (fill-pointer buf))))

;;; ---- write them out ---------------------------------------------------
(let ((dir (merge-pathnames "" *load-pathname*)))
  (flet ((out (name bytes)
           (with-open-file (f (merge-pathnames name dir)
                              :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
             (write-sequence bytes f))
           (format t "wrote ~a (~D bytes)~%" name (length bytes))))
    (out "a-rect.pdf" (make-a))
    (out "b-flate.pdf" (make-b))
    (out "c-xrefstm.pdf" (make-c))
    (out "d-array.pdf" (make-d))))
