;;;; src/object.lisp — the PDF object model + recursive-descent parser.
;;;; (ISO 32000-1 §7.3 object types, §7.3.10 indirect objects.)
;;;;
;;;; Representation:
;;;;   dict     -> hash-table  (string name -> value); keys are name strings
;;;;   array    -> simple vector
;;;;   name     -> PDF-NAME struct (distinct from strings)
;;;;   string   -> PDF-STRING struct wrapping an octet vector
;;;;   number   -> Lisp integer or double-float
;;;;   boolean  -> keyword :TRUE / :FALSE
;;;;   null     -> the singleton (PDF-NULL)
;;;;   ref      -> PDF-REF{num gen}
;;;;   stream   -> PDF-STREAM{dict raw-bytes}

(in-package #:folio)

;;; ---- leaf structs -----------------------------------------------------

(defstruct (pdf-name (:constructor make-pdf-name (string)))
  (string "" :type string))

(defstruct (pdf-string (:constructor make-pdf-string (bytes)))
  (bytes #() :type (simple-array (unsigned-byte 8) (*))))

(defstruct (pdf-ref (:constructor make-pdf-ref (num gen)))
  (num 0 :type integer)
  (gen 0 :type integer))

(defstruct (pdf-stream (:constructor make-pdf-stream (dict raw)))
  dict                                  ; a dict hash-table
  (raw #() :type (simple-array (unsigned-byte 8) (*))))

;;; ---- null -------------------------------------------------------------

(defvar +pdf-null+ '%pdf-null)
(declaim (inline pdf-null pdf-null-p))
(defun pdf-null () +pdf-null+)
(defun pdf-null-p (x) (eq x +pdf-null+))

;;; ---- dict / array helpers ---------------------------------------------

(defun make-pdf-dict (&rest kv)
  "Build a dict from alternating string-key / value arguments."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on kv by #'cddr do (setf (gethash k h) v))
    h))

(defun pdf-dict-p (x) (hash-table-p x))
(defun pdf-array-p (x) (and (vectorp x) (not (typep x '(array (unsigned-byte 8))))))

(defun normalize-key (k)
  "Coerce a dict key to a plain string (accept a pdf-name or /-prefixed str)."
  (cond ((pdf-name-p k) (pdf-name-string k))
        ((and (stringp k) (> (length k) 0) (char= (char k 0) #\/)) (subseq k 1))
        (t k)))

(defun dict-get (dict key &optional default)
  "Look up KEY (string, /-string, or pdf-name) in DICT.  Returns DEFAULT if
absent.  Does NOT resolve indirect references (see FOLIO:RESOLVE)."
  (if (hash-table-p dict)
      (multiple-value-bind (v present) (gethash (normalize-key key) dict)
        (if present v default))
      default))

(defun dict-get* (dict key)
  "Like DICT-GET but returns (values value present-p)."
  (if (hash-table-p dict)
      (gethash (normalize-key key) dict)
      (values nil nil)))

(defun name= (val string)
  "True if VAL is a PDF name equal to STRING (with or without leading slash)."
  (and (pdf-name-p val)
       (string= (pdf-name-string val)
                (if (and (> (length string) 0) (char= (char string 0) #\/))
                    (subseq string 1) string))))

(defun name->keyword (val)
  (when (pdf-name-p val) (intern (string-upcase (pdf-name-string val)) :keyword)))

;;; ---- recursive-descent object parser ----------------------------------

(defun parse-object (lx &optional resolver)
  "Parse one PDF object value from lexer LX.  RESOLVER, if given, is called on
a PDF-REF to resolve an indirect /Length while reading a stream body."
  (let ((tok (lex-next lx)))
    (%parse-from-token lx tok resolver)))

(defun %parse-from-token (lx tok resolver)
  (ecase (token-type tok)
    (:int    (%parse-int-or-ref lx (token-value tok)))
    (:real   (token-value tok))
    (:string (make-pdf-string (token-value tok)))
    (:name   (make-pdf-name (token-value tok)))
    (:array-open (%parse-array lx resolver))
    (:dict-open  (%parse-dict-or-stream lx resolver))
    (:keyword (%parse-keyword lx tok))
    (:array-close (error "folio: unexpected ']'"))
    (:dict-close  (error "folio: unexpected '>>'"))
    (:eof (error "folio: unexpected end of input while parsing object"))))

(defun %parse-keyword (lx tok)
  (declare (ignore lx))
  (let ((s (token-value tok)))
    (cond ((string= s "true")  :true)
          ((string= s "false") :false)
          ((string= s "null")  (pdf-null))
          ;; a bareword we can't place — surface it as a name-like token so
          ;; content-stream reuse is possible; here in object context it's rare.
          (t (make-pdf-name s)))))

(defun %parse-int-or-ref (lx first)
  "Given a leading integer FIRST already consumed, detect `N G R` (indirect
reference) with two-token lookahead; otherwise the integer stands alone."
  (let ((save-pos (lexer-pos lx))
        (save-peek (lexer-peeked lx))
        (t2 (lex-next lx)))
    (if (eq (token-type t2) :int)
        (let ((t3 (lex-next lx)))
          (if (and (eq (token-type t3) :keyword) (string= (token-value t3) "R"))
              (make-pdf-ref first (token-value t2))
              (progn (setf (lexer-pos lx) save-pos (lexer-peeked lx) save-peek)
                     first)))
        (progn (setf (lexer-pos lx) save-pos (lexer-peeked lx) save-peek)
               first))))

(defun %parse-array (lx resolver)
  (let ((out (make-array 8 :adjustable t :fill-pointer 0)))
    (loop for tok = (lex-next lx)
          do (case (token-type tok)
               (:array-close (return))
               (:eof (return))
               (t (vector-push-extend (%parse-from-token lx tok resolver) out))))
    (coerce out 'simple-vector)))

(defun %parse-dict-or-stream (lx resolver)
  "Parse `<< ... >>` then, if a `stream` keyword follows, its body."
  (let ((dict (make-hash-table :test 'equal)))
    (loop for ktok = (lex-next lx)
          do (case (token-type ktok)
               (:dict-close (return))
               (:eof (return))
               (:name
                (let ((val (parse-object lx resolver)))
                  (setf (gethash (token-value ktok) dict) val)))
               (t                       ; skip stray token, stay lenient
                nil)))
    ;; Is this the dict of a stream object?
    (if (eq (token-type (lex-peek lx)) :keyword)
        (let ((kw (token-value (lex-peek lx))))
          (if (string= kw "stream")
              (progn (lex-next lx)      ; consume `stream`; pos now right after it
                     (make-pdf-stream dict (%read-stream-body lx dict resolver)))
              dict))
        dict)))

(defun %read-stream-body (lx dict resolver)
  "LX positioned immediately after the `stream` keyword.  Read the raw bytes:
skip the EOL after `stream`, honour /Length when it is present and correct, and
fall back to scanning for `endstream` when it is missing or wrong (§7.3.8.1)."
  (let ((buf (lexer-buf lx))
        (end (lexer-end lx)))
    ;; EOL after `stream` shall be CRLF or LF (be lenient about lone CR).
    (when (and (< (lexer-pos lx) end) (= (aref buf (lexer-pos lx)) 13))
      (incf (lexer-pos lx)))
    (when (and (< (lexer-pos lx) end) (= (aref buf (lexer-pos lx)) 10))
      (incf (lexer-pos lx)))
    (let* ((start (lexer-pos lx))
           (len (let ((l (dict-get dict "Length")))
                  (when (and resolver (pdf-ref-p l))
                    (setf l (funcall resolver l)))
                  (and (integerp l) (>= l 0) l)))
           (raw nil)
           (after nil))
      ;; Trust /Length only if `endstream` really follows.
      (when (and len (<= (+ start len) end))
        (let ((p (+ start len)))
          (loop while (and (< p end) (ws-byte-p (aref buf p))) do (incf p))
          (when (bytes-match-p buf p end "endstream")
            (setf raw (subseq buf start (+ start len))
                  after (+ p 9)))))
      (unless raw
        (let ((es (search-bytes buf start end "endstream")))
          (if es
              (let ((e es))
                (when (and (> e start) (= (aref buf (1- e)) 10)) (decf e))
                (when (and (> e start) (= (aref buf (1- e)) 13)) (decf e))
                (setf raw (subseq buf start e) after (+ es 9)))
              (setf raw (subseq buf start end) after end))))
      (setf (lexer-pos lx) after (lexer-peeked lx) nil)
      (coerce raw '(simple-array (unsigned-byte 8) (*))))))

;;; ---- byte search helpers ----------------------------------------------

(defun bytes-match-p (buf pos end string)
  "True if the ASCII STRING appears in BUF starting at POS (within END)."
  (let ((n (length string)))
    (and (<= (+ pos n) end)
         (loop for i below n
               always (= (aref buf (+ pos i)) (char-code (char string i)))))))

(defun search-bytes (buf start end string)
  "Return the index in BUF of the first ASCII match of STRING within
[START, END), or NIL."
  (let ((n (length string)))
    (loop for i from start to (- end n)
          when (bytes-match-p buf i end string) do (return i))))

;;; ---- indirect object --------------------------------------------------

(defun parse-indirect-object (buf offset &key resolver (end (length buf)))
  "Parse `N G obj <object> endobj` located at byte OFFSET in BUF.
Returns (values object num gen)."
  (let* ((lx (make-lexer buf :start offset :end end))
         (t1 (lex-next lx))
         (t2 (lex-next lx))
         (t3 (lex-next lx)))
    (unless (and (eq (token-type t1) :int)
                 (eq (token-type t2) :int)
                 (eq (token-type t3) :keyword)
                 (string= (token-value t3) "obj"))
      (error "folio: expected `N G obj` at offset ~D" offset))
    (let ((obj (parse-object lx resolver)))
      ;; optional trailing `endobj` — ignore whatever follows
      (values obj (token-value t1) (token-value t2)))))

(defun parse-object-from-bytes (bytes &key resolver)
  "Convenience: parse a single object value from a standalone octet vector."
  (parse-object (make-lexer bytes) resolver))
