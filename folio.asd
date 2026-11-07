;;;; folio.asd — a pure Common Lisp PDF renderer.
(asdf:defsystem "folio"
  :description "A from-scratch PDF renderer in pure Common Lisp: parse PDF files
(objects, classic xref + xref streams, object streams, stream filters, the page
tree, RC4/AES encryption) and render pages to pixels — full vector graphics and
text (every font type: Type1, TrueType, CFF/CID, Type3, CJK CMaps), images (JPEG
incl. CMYK, Flate, CCITT G3/G4, JBIG2), transparency (soft masks, blend modes,
groups), shadings and patterns, and annotation/AcroForm appearances — by painting
through the sibling pure-CL graphics stack (gesso/scribe/pigment/cram).  Verified
page-by-page against pdfium.  Fills the CL ecosystem gap: cl-pdf generates PDFs
but nothing renders them.  No FFI, no Poppler, no MuPDF."
  :version "0.1.0"
  :author "ynniv"
  :license "MIT"
  :depends-on ("gesso" "scribe" "pigment" "cram" "natrium")
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
                             (:file "cmap")
                             (:file "font")
                             (:file "content")
                             (:file "ccitt")
                             (:file "jbig2")
                             (:file "image")
                             (:file "shading")
                             (:file "pattern")
                             (:file "render")
                             (:file "form")))))
