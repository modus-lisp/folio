;;;; folio.asd — a pure Common Lisp PDF renderer.
(asdf:defsystem "folio"
  :description "A pure-Common-Lisp PDF renderer: parse PDF files (objects,
classic xref + xref streams, object streams, stream filters, the page tree) and
render pages by painting through the sibling pure-CL graphics stack
(gesso/scribe/pigment/cram).  Fills the CL ecosystem gap — cl-pdf generates PDFs
but nothing renders them.  No FFI, no Poppler, no MuPDF.  Milestone 0: the parser."
  :version "0.0.1"
  :author "ynniv"
  :license "MIT"
  :depends-on ("gesso" "scribe" "pigment" "cram")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "lexer")
                             (:file "object")
                             (:file "filters")
                             (:file "xref")
                             (:file "encrypt")
                             (:file "doc")
                             (:file "enc-data")
                             (:file "cff")
                             (:file "type1")
                             (:file "font")
                             (:file "content")
                             (:file "image")
                             (:file "render")))))
