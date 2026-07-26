;;;; src/packages.lisp — folio package definition.
(defpackage #:folio
  (:use #:cl)
  (:local-nicknames (#:cram #:cram))
  (:export
   ;; ---- object model: structs, constructors, predicates ----
   #:pdf-name #:make-pdf-name #:pdf-name-p #:pdf-name-string
   #:pdf-string #:make-pdf-string #:pdf-string-p #:pdf-string-bytes
   #:pdf-ref #:make-pdf-ref #:pdf-ref-p #:pdf-ref-num #:pdf-ref-gen
   #:pdf-stream #:make-pdf-stream #:pdf-stream-p
   #:pdf-stream-dict #:pdf-stream-raw
   #:pdf-null #:pdf-null-p
   #:pdf-dict-p #:pdf-array-p
   ;; ---- dict / array accessors ----
   #:dict-get #:dict-get* #:name= #:name->keyword
   #:make-pdf-dict
   ;; ---- lexer ----
   #:make-lexer #:lexer #:lex-next #:lex-peek
   ;; ---- object parsing ----
   #:parse-object #:parse-indirect-object #:parse-object-from-bytes
   ;; ---- document / xref ----
   #:document #:document-p #:parse-pdf
   #:document-trailer #:document-root #:document-version
   #:resolve #:object-count
   ;; ---- encryption (standard security handler) ----
   #:encryption-status #:encryption-reason
   #:md5 #:rc4 #:sha256 #:sha384 #:sha512 #:aes-cbc-decrypt
   #:page-count #:get-page
   ;; ---- page struct ----
   #:page #:page-p #:page-dict #:page-number
   #:page-mediabox #:page-cropbox #:page-rotate #:page-resources
   #:page-content-bytes
   ;; ---- filters ----
   #:decode-stream #:apply-filter
   ;; ---- content-stream interpreter / rendering (M1) ----
   #:render-page #:render-page-to-png
   #:run-content #:make-interp #:interp #:device-transform
   ;; ---- interactive form recognition (AcroForm, §12.7) ----
   #:has-acroform-p #:acroform-info #:acroform-has-xfa-p
   #:form-fields #:describe-form #:pdf-text-string
   #:form-field #:form-field-p
   #:form-field-type #:form-field-name #:form-field-partial-name
   #:form-field-tooltip #:form-field-value #:form-field-default-value
   #:form-field-flags #:form-field-raw-flags #:form-field-options
   #:form-field-indices #:form-field-signed #:form-field-widgets
   #:form-field-rect #:form-field-page #:form-field-state #:form-field-dict
   #:form-widget #:form-widget-p #:form-widget-rect #:form-widget-page
   #:form-widget-state #:form-widget-on-states))
