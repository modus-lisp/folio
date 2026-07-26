;;;; src/form.lisp — interactive form (AcroForm) recognition.
;;;; (ISO 32000-1 §12.7: interactive forms; §12.7.3 field dictionaries;
;;;;  §12.7.4 field types; §12.5.6.19 widget annotations.)
;;;;
;;;; This module RECOGNISES fillable forms: it detects an AcroForm, walks the
;;;; field tree (recursing through /Kids, honouring the inheritable /FT, /Ff and
;;;; /DA), and produces a flat list of FORM-FIELD descriptors carrying each
;;;; field's type, fully-qualified name, value, decoded flag bits, and — for
;;;; each of its widget annotations — the /Rect, page index and on/off state.
;;;; It does NOT fill or edit fields; it is a queryable structural model that
;;;; sits alongside the M7 appearance-stream renderer (which already DISPLAYS
;;;; the same widgets).

(in-package #:folio)

;;; ---- text-string decoding (§7.9.2.2) ----------------------------------

(defun pdf-text-string (obj &optional doc)
  "Decode a PDF text string OBJ to a Lisp string.  Honours a UTF-16BE / UTF-16LE
byte-order mark; otherwise treats the bytes as PDFDocEncoding (≈ Latin-1, whose
low range coincides with Unicode).  Returns NIL for a non-string.  DOC, if
given, resolves an indirect reference first."
  (when (and doc (pdf-ref-p obj)) (setf obj (resolve doc obj)))
  (unless (pdf-string-p obj) (return-from pdf-text-string nil))
  (let ((b (pdf-string-bytes obj)))
    (cond
      ((and (>= (length b) 2) (= (aref b 0) #xFE) (= (aref b 1) #xFF)) ; UTF-16BE
       (with-output-to-string (s)
         (loop for i from 2 below (1- (length b)) by 2
               do (write-char (code-char (logior (ash (aref b i) 8) (aref b (1+ i)))) s))))
      ((and (>= (length b) 2) (= (aref b 0) #xFF) (= (aref b 1) #xFE)) ; UTF-16LE
       (with-output-to-string (s)
         (loop for i from 2 below (1- (length b)) by 2
               do (write-char (code-char (logior (ash (aref b (1+ i)) 8) (aref b i))) s))))
      (t                                    ; PDFDocEncoding / Latin-1
       (map 'string #'code-char b)))))

;;; ---- the field descriptor --------------------------------------------

(defstruct (form-widget (:constructor %make-form-widget))
  "One widget annotation of a field: where it appears on the page and, for a
checkbox/radio, its /AS state and the set of 'on' state names."
  (rect nil)                              ; (x0 y0 x1 y1) doubles, or NIL
  (page nil)                              ; 0-based page index, or NIL
  (state nil)                             ; /AS as a string ("Off", "1", ...), or NIL
  (on-states '()))                        ; on-state names from /AP /N (sans "Off")

(defstruct (form-field (:constructor %make-form-field))
  "A recognised interactive-form field (ISO 32000-1 §12.7.3).  Descriptor only —
recognition, not editing.  RECT/PAGE/STATE are conveniences drawn from the first
widget; WIDGETS holds them all (a radio group has several)."
  (type :unknown)      ; :text :dropdown :listbox :signature
                       ; :pushbutton :radio :checkbox :unknown
  (name "")            ; fully-qualified name: ancestors' /T joined with "."
  (partial-name nil)   ; this node's own /T
  (tooltip nil)        ; /TU (alternate/user name)
  (value nil)          ; decoded /V (string, name string, or list for choices)
  (default-value nil)  ; decoded /DV
  (flags '())          ; decoded /Ff bits as keywords
  (raw-flags 0)        ; the raw /Ff integer
  (options '())        ; /Opt for a choice: list of strings or (export . display)
  (indices '())        ; /I selected indices for a multi-select choice
  (signed nil)         ; signature field: T when /V (a signature dict) is present
  (widgets '())        ; list of FORM-WIDGET
  (rect nil)           ; first widget's /Rect
  (page nil)           ; first widget's page index
  (state nil)          ; first widget's /AS
  (dict nil))          ; the underlying field dictionary

;;; ---- detection --------------------------------------------------------

(defun acroform-dict (doc)
  "The catalog's /AcroForm dictionary (resolved), or NIL."
  (let* ((root (and (document-p doc) (document-root doc)))
         (af (and (hash-table-p root) (resolve doc (dict-get root "AcroForm")))))
    (and (hash-table-p af) af)))

(defun has-acroform-p (doc)
  "True when DOC carries an interactive form: an /AcroForm dictionary with a
non-empty /Fields array (§12.7.2)."
  (let ((af (acroform-dict doc)))
    (and af
         (let ((fields (resolve doc (dict-get af "Fields"))))
           (and (pdf-array-p fields) (plusp (length fields))))
         t)))

(defun acroform-has-xfa-p (doc)
  "True when the AcroForm carries an XFA (Adobe XML Forms Architecture) packet.
XFA describes the same fields in a parallel XML tree; folio recognises the
AcroForm side, which for hybrid forms mirrors it (§12.7.8, XFA is out of scope)."
  (let ((af (acroform-dict doc)))
    (and af (let ((x (resolve doc (dict-get af "XFA"))))
              (and x (not (pdf-null-p x)))))))

(defun acroform-info (doc)
  "AcroForm-level defaults as a plist: :da :dr :q :need-appearances :sig-flags
:xfa (§12.7.2, table 218).  NIL when there is no form."
  (let ((af (acroform-dict doc)))
    (when af
      (let ((na (resolve doc (dict-get af "NeedAppearances"))))
        (list :da (pdf-text-string (resolve doc (dict-get af "DA")))
              :dr (resolve doc (dict-get af "DR"))
              :q (let ((q (resolve doc (dict-get af "Q")))) (and (integerp q) q))
              :need-appearances (eq na :true)
              :sig-flags (let ((s (resolve doc (dict-get af "SigFlags")))) (and (integerp s) s))
              :xfa (acroform-has-xfa-p doc))))))

;;; ---- flag / type decoding (§12.7.3.1, tables 226/228/230) -------------

(defun field-type-of (ft ff)
  "Classify a field from its (inherited) /FT name and /Ff integer."
  (let ((f (if (integerp ff) ff 0)))
    (cond
      ((name= ft "Tx")  :text)
      ((name= ft "Ch")  (if (logbitp 17 f) :dropdown :listbox)) ; Combo = bit 18
      ((name= ft "Sig") :signature)
      ((name= ft "Btn")
       (cond ((logbitp 16 f) :pushbutton)  ; Pushbutton = bit 17
             ((logbitp 15 f) :radio)       ; Radio      = bit 16
             (t :checkbox)))
      (t :unknown))))

(defun decode-field-flags (ff type)
  "Decode the /Ff integer into keywords appropriate to TYPE.  Bit numbers are the
1-based positions of §12.7.3.1 (common) and tables 226/228/230."
  (let ((f (if (integerp ff) ff 0)) (out '()))
    (flet ((b (n kw) (when (logbitp (1- n) f) (push kw out))))
      (b 1 :readonly) (b 2 :required) (b 3 :noexport)   ; table 221, common
      (case type
        ((:pushbutton :radio :checkbox)                 ; table 226, button
         (b 15 :no-toggle-to-off) (b 16 :radio) (b 17 :pushbutton)
         (b 18 :radios-in-unison))
        ((:text)                                         ; table 228, text
         (b 13 :multiline) (b 14 :password) (b 21 :fileselect)
         (b 23 :do-not-spell-check) (b 24 :do-not-scroll) (b 25 :comb)
         (b 26 :rich-text))
        ((:dropdown :listbox)                            ; table 230, choice
         (b 18 :combo) (b 19 :edit) (b 20 :sort) (b 22 :multiselect)
         (b 23 :do-not-spell-check) (b 27 :commit-on-sel-change))))
    (nreverse out)))

;;; ---- value / option decoding ------------------------------------------

(defun decode-field-value (doc v)
  "Decode a /V or /DV value: a text string, a name (returned as its string), or —
for a multi-select choice — a list of strings.  NIL / null / absent -> NIL."
  (let ((v (resolve doc v)))
    (cond
      ((or (null v) (pdf-null-p v)) nil)
      ((pdf-string-p v) (pdf-text-string v))
      ((pdf-name-p v) (pdf-name-string v))  ; checkbox/radio export value
      ((pdf-array-p v)
       (loop for e across v for d = (resolve doc e)
             collect (or (pdf-text-string d) (and (pdf-name-p d) (pdf-name-string d)))))
      (t nil))))

(defun decode-options (doc opt)
  "Decode a choice field's /Opt (§12.7.4.4).  Each entry is either a text string
or a two-element [export display] array; returns a list of strings or
(export . display) conses."
  (let ((opt (resolve doc opt)))
    (when (pdf-array-p opt)
      (loop for e across opt for d = (resolve doc e)
            collect (cond
                      ((pdf-string-p d) (pdf-text-string d))
                      ((and (pdf-array-p d) (>= (length d) 2))
                       (cons (pdf-text-string (resolve doc (aref d 0)))
                             (pdf-text-string (resolve doc (aref d 1)))))
                      (t (decode-field-value doc d)))))))

(defun decode-indices (doc i)
  "Decode a choice field's /I selected-index array to a list of integers."
  (let ((i (resolve doc i)))
    (when (pdf-array-p i)
      (loop for e across i for v = (resolve doc e) when (integerp v) collect v))))

;;; ---- widget geometry: /Rect, on-states, page index --------------------

(defun %rect-list (doc r)
  "Resolve a 4-element /Rect to a list of doubles, or NIL."
  (let ((r (resolve doc r)))
    (when (and (pdf-array-p r) (= (length r) 4))
      (loop for i below 4 for v = (resolve doc (aref r i))
            collect (coerce (if (numberp v) v 0) 'double-float)))))

(defun %on-states (doc widget)
  "The 'on' appearance-state names of a checkbox/radio WIDGET: the keys of its
/AP /N sub-dictionary other than \"Off\" (§12.7.4.2.3)."
  (let* ((ap (resolve doc (dict-get widget "AP")))
         (n (and (hash-table-p ap) (resolve doc (dict-get ap "N")))))
    (when (hash-table-p n)
      (loop for k being the hash-keys of n
            unless (string= k "Off") collect k))))

(defun build-widget-page-maps (doc)
  "Return (values ref-num->page page-dict->index): maps to locate the page a
widget appears on, one from each page's /Annots (widget object number -> index),
the other keyed by page-dict identity for a /P fallback."
  (let ((by-num (make-hash-table))
        (by-dict (make-hash-table :test 'eq))
        (pages (document-pages doc)))
    (when pages
      (loop for i below (length pages)
            for pl = (aref pages i)
            for dict = (getf pl :dict) do
        (when (hash-table-p dict) (setf (gethash dict by-dict) i))
        (let ((annots (resolve doc (dict-get dict "Annots"))))
          (when (pdf-array-p annots)
            (loop for a across annots
                  when (pdf-ref-p a) do (setf (gethash (pdf-ref-num a) by-num) i))))))
    (values by-num by-dict)))

;;; ---- the field-tree walk ----------------------------------------------

(defun widget-kid-p (doc kid)
  "True when KID (resolved) is a pure widget annotation of its parent field —
/Subtype /Widget with no partial name /T — rather than a sub-field (§12.7.3.1)."
  (and (hash-table-p kid)
       (name= (dict-get kid "Subtype") "Widget")
       (null (dict-get kid "T"))
       (null (dict-get kid "FT"))))

(defun form-fields (doc)
  "Enumerate DOC's interactive-form fields as a flat list of FORM-FIELD
descriptors (ISO 32000-1 §12.7).  Walks /AcroForm /Fields, recursing through
/Kids and inheriting /FT, /Ff and /DA; handles the merged (field-is-its-own-
widget), kids-widgets (e.g. radio group) and hierarchical (grouping /T parent)
shapes.  Returns NIL when there is no form."
  (let ((af (acroform-dict doc)))
    (unless af (return-from form-fields '()))
    (let ((out '())
          (seen (make-hash-table)))        ; ref-num visited guard (cycles)
      (multiple-value-bind (num->page dict->page) (build-widget-page-maps doc)
        (labels
            ((page-of (num dict)
               (or (and num (gethash num num->page))
                   (let ((p (resolve doc (dict-get dict "P"))))
                     (and (hash-table-p p) (gethash p dict->page)))))
             (widget-of (dict num)
               (%make-form-widget
                :rect (%rect-list doc (dict-get dict "Rect"))
                :page (page-of num dict)
                :state (let ((as (resolve doc (dict-get dict "AS"))))
                         (and (pdf-name-p as) (pdf-name-string as)))
                :on-states (%on-states doc dict)))
             (emit-field (node num name-parts ft ff)
               ;; NODE is a terminal field: gather its widgets and descriptor.
               (let* ((type (field-type-of ft ff))
                      (kids (resolve doc (dict-get node "Kids")))
                      (widgets '()))
                 (if (pdf-array-p kids)
                     (loop for k across kids
                           for kd = (resolve doc k)
                           when (and (hash-table-p kd) (name= (dict-get kd "Subtype") "Widget"))
                             do (push (widget-of kd (and (pdf-ref-p k) (pdf-ref-num k))) widgets))
                     ;; merged: the field dict is itself the widget
                     (when (dict-get node "Rect")
                       (push (widget-of node num) widgets)))
                 (setf widgets (nreverse widgets))
                 (let* ((w0 (first widgets))
                        (v (dict-get node "V"))
                        (field
                          (%make-form-field
                           :type type
                           :name (format nil "~{~A~^.~}" name-parts)
                           :partial-name (pdf-text-string (dict-get node "T") doc)
                           :tooltip (pdf-text-string (dict-get node "TU") doc)
                           :value (decode-field-value doc v)
                           :default-value (decode-field-value doc (dict-get node "DV"))
                           :flags (decode-field-flags ff type)
                           :raw-flags (if (integerp ff) ff 0)
                           :options (when (member type '(:dropdown :listbox))
                                      (decode-options doc (dict-get node "Opt")))
                           :indices (when (member type '(:dropdown :listbox))
                                      (decode-indices doc (dict-get node "I")))
                           :signed (and (eq type :signature)
                                        (let ((rv (resolve doc v)))
                                          (and rv (not (pdf-null-p rv)))))
                           :widgets widgets
                           :rect (and w0 (form-widget-rect w0))
                           :page (and w0 (form-widget-page w0))
                           :state (and w0 (form-widget-state w0))
                           :dict node)))
                   (push field out))))
             (walk (ref name-parts ft ff da)
               (let* ((num (and (pdf-ref-p ref) (pdf-ref-num ref)))
                      (node (resolve doc ref)))
                 (when (and num (gethash num seen)) (return-from walk))
                 (when num (setf (gethash num seen) t))
                 (unless (hash-table-p node) (return-from walk))
                 (let* ((tt (pdf-text-string (dict-get node "T") doc))
                        (parts (if tt (append name-parts (list tt)) name-parts))
                        (ft* (or (dict-get node "FT") ft))
                        (ff* (let ((x (resolve doc (dict-get node "Ff")))) (if (integerp x) x ff)))
                        (da* (or (pdf-text-string (dict-get node "DA") doc) da))
                        (kids (resolve doc (dict-get node "Kids")))
                        ;; kids that are sub-FIELDS (not pure widgets)
                        (field-kids (when (pdf-array-p kids)
                                      (loop for k across kids
                                            for kd = (resolve doc k)
                                            unless (widget-kid-p doc kd) collect k))))
                   (cond
                     (field-kids            ; grouping node: recurse into subfields
                      (dolist (k field-kids) (walk k parts ft* ff* da*)))
                     (ft*                    ; terminal field
                      (emit-field node num parts ft* ff*))
                     (t nil))))))
          (let ((fields (resolve doc (dict-get af "Fields"))))
            (when (pdf-array-p fields)
              (loop for f across fields do (walk f '() nil nil nil))))))
      (nreverse out))))

;;; ---- human-readable dump ----------------------------------------------

(defun describe-form (doc &optional (stream *standard-output*))
  "Print a readable table of DOC's form fields (type, name, value, rect, page,
flags, options / on-states) to STREAM.  A recognition aid, not a renderer."
  (unless (has-acroform-p doc)
    (format stream "No interactive form (AcroForm) in this document.~%")
    (return-from describe-form (values)))
  (let* ((info (acroform-info doc))
         (fields (form-fields doc)))
    (format stream "AcroForm: ~D field~:P~@[  [XFA present — Adobe XML forms]~]~@[  NeedAppearances~]~%"
            (length fields) (getf info :xfa) (getf info :need-appearances))
    (format stream "~&~2TTYPE~12TNAME~48TVALUE~64TPAGE~70TRECT~%")
    (format stream "~&  ~A~%" (make-string 96 :initial-element #\-))
    (dolist (f fields)
      (format stream "~&~2T~A~12T~A~48T~A~64T~A~70T~A~%"
              (string-downcase (symbol-name (form-field-type f)))
              (%truncate (form-field-name f) 34)
              (%truncate (%value-string f) 14)
              (let ((p (form-field-page f))) (if p (1+ p) "-"))
              (%rect-string (form-field-rect f)))
      (when (form-field-flags f)
        (format stream "~12Tflags: ~{~(~A~)~^ ~}~%" (form-field-flags f)))
      (when (form-field-tooltip f)
        (format stream "~12Ttip:   ~A~%" (%truncate (form-field-tooltip f) 70)))
      (when (form-field-options f)
        (format stream "~12Topts:  ~A~%"
                (%truncate (format nil "~{~A~^, ~}"
                                   (mapcar (lambda (o) (if (consp o) (cdr o) o))
                                           (form-field-options f))) 70)))
      (let ((ons (remove-duplicates
                  (loop for w in (form-field-widgets f) append (form-widget-on-states w))
                  :test #'equal)))
        (when ons
          (format stream "~12Tstates: ~{~A~^ ~}~@[  (widgets: ~D)~]~%"
                  ons (when (> (length (form-field-widgets f)) 1)
                        (length (form-field-widgets f)))))))
    (values)))

(defun %value-string (f)
  (let ((v (form-field-value f)))
    (cond ((null v) "")
          ((listp v) (format nil "~{~A~^,~}" v))
          (t (princ-to-string v)))))

(defun %rect-string (rect)
  (if (and rect (= (length rect) 4))
      (format nil "[~{~,1F~^ ~}]" rect)
      ""))

(defun %truncate (s n)
  (let ((s (if (stringp s) s (princ-to-string s))))
    (if (> (length s) n) (concatenate 'string (subseq s 0 (max 0 (- n 1))) "…") s)))
