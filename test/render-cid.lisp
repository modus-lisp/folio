;;;; test/render-cid.lisp — render the M3 Type0/CID + embedded-CFF fixtures.
;;;; Fixtures are built by test/gen-cid-pdfs.py + test/gen-cid-keyed.py (they need
;;;; fonttools + the DejaVu / URW-base35 system fonts) and land as gitignored
;;;; test/real-*.pdf.  Compare the PNGs against pypdfium2 with test/oracle.py.
(asdf:load-system "folio")
(in-package :folio)
(defun rfb (p)
  (with-open-file (f p :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length f) :element-type '(unsigned-byte 8))))
      (read-sequence v f) v)))
(dolist (name '("real-cid-tt"       ; Type0 / CIDFontType2 (TrueType, Identity-H)
                "real-cid-cff"       ; Type0 / CIDFontType0 (non-CID CFF)
                "real-cid-keyed"     ; Type0 / CIDFontType0 (CID-keyed CFF, FDArray)
                "real-simple-cff"))  ; simple Type1 with a FontFile3 (Type1C) program
  (let ((path (format nil "test/~a.pdf" name)))
    (when (probe-file path)
      (let ((doc (parse-pdf (rfb path))))
        (render-page-to-png doc 0 (format nil "test/out-~a.png" name) :scale 2.0)
        (format t "~&rendered ~a~%" name)))))
