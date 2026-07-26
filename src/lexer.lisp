;;;; src/lexer.lisp — tokenizer for PDF object syntax (ISO 32000-1 §7.2).
;;;;
;;;; PDF is byte-oriented; we tokenize directly on an octet buffer.  The lexer
;;;; is peekable (one token of lookahead) and exposes its raw position so the
;;;; object parser can grab the verbatim bytes of a `stream ... endstream`.

(in-package #:folio)

;;; ---- character classes (PDF 32000-1 §7.2.2, tables 1 & 2) --------------

(declaim (inline ws-byte-p delim-byte-p regular-byte-p digit-byte-p
                 hex-byte-p))

(defun ws-byte-p (b)
  "PDF white-space: NUL TAB LF FF CR SP."
  (or (= b 0) (= b 9) (= b 10) (= b 12) (= b 13) (= b 32)))

(defun delim-byte-p (b)
  "PDF delimiter: ( ) < > [ ] { } / %."
  (case b ((40 41 60 62 91 93 123 125 47 37) t)))

(defun regular-byte-p (b)
  "A regular character is anything that is neither white-space nor delimiter."
  (not (or (ws-byte-p b) (delim-byte-p b))))

(defun digit-byte-p (b) (<= 48 b 57))

(defun hex-byte-p (b)
  (or (<= 48 b 57) (<= 65 b 70) (<= 97 b 102)))

(defun hex-val (b)
  (cond ((<= 48 b 57) (- b 48))
        ((<= 65 b 70) (+ 10 (- b 65)))
        ((<= 97 b 102) (+ 10 (- b 97)))
        (t nil)))

;;; ---- token ------------------------------------------------------------
;;; type is one of:
;;;   :int :real            value = number
;;;   :string               value = octet vector (decoded)
;;;   :name                 value = string (decoded name, no leading slash)
;;;   :array-open :array-close :dict-open :dict-close   value = nil
;;;   :keyword              value = string (obj endobj R stream endstream
;;;                                         true false null xref trailer
;;;                                         startxref, or any bareword)
;;;   :eof                  value = nil

(defstruct (token (:constructor make-token (type value)))
  type value)

;;; ---- lexer ------------------------------------------------------------

(defstruct (lexer (:constructor %make-lexer))
  (buf (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (pos 0 :type fixnum)
  (end 0 :type fixnum)
  (peeked nil))                         ; cached token (one-token lookahead)

(defun make-lexer (buf &key (start 0) (end (length buf)))
  "Make a peekable lexer over the octet vector BUF within [START, END)."
  (%make-lexer :buf (coerce buf '(simple-array (unsigned-byte 8) (*)))
               :pos start :end end :peeked nil))

(declaim (inline lx-byte lx-at-end))
(defun lx-byte (lx) (aref (lexer-buf lx) (lexer-pos lx)))
(defun lx-at-end (lx) (>= (lexer-pos lx) (lexer-end lx)))

(defun skip-ws-and-comments (lx)
  "Advance over white-space and %-comments (comment runs to EOL, §7.2.3)."
  (loop with buf = (lexer-buf lx)
        while (< (lexer-pos lx) (lexer-end lx))
        for b = (aref buf (lexer-pos lx))
        do (cond ((ws-byte-p b) (incf (lexer-pos lx)))
                 ((= b 37)              ; %  comment
                  (loop while (and (< (lexer-pos lx) (lexer-end lx))
                                   (let ((c (aref buf (lexer-pos lx))))
                                     (and (/= c 10) (/= c 13))))
                        do (incf (lexer-pos lx))))
                 (t (return)))))

(defun lex-peek (lx)
  "Return the next token without consuming it."
  (or (lexer-peeked lx)
      (setf (lexer-peeked lx) (%lex-read lx))))

(defun lex-next (lx)
  "Consume and return the next token."
  (if (lexer-peeked lx)
      (prog1 (lexer-peeked lx) (setf (lexer-peeked lx) nil))
      (%lex-read lx)))

;;; ---- the scanner ------------------------------------------------------

(defun %lex-read (lx)
  (skip-ws-and-comments lx)
  (when (lx-at-end lx)
    (return-from %lex-read (make-token :eof nil)))
  (let ((b (lx-byte lx)))
    (cond
      ((= b 91) (incf (lexer-pos lx)) (make-token :array-open nil))
      ((= b 93) (incf (lexer-pos lx)) (make-token :array-close nil))
      ((= b 40) (%lex-literal-string lx))       ; (
      ((= b 47) (%lex-name lx))                 ; /
      ((= b 60)                                 ; <  -> dict << or hex string
       (if (and (< (1+ (lexer-pos lx)) (lexer-end lx))
                (= (aref (lexer-buf lx) (1+ (lexer-pos lx))) 60))
           (progn (incf (lexer-pos lx) 2) (make-token :dict-open nil))
           (%lex-hex-string lx)))
      ((= b 62)                                 ; >  -> dict >>
       (if (and (< (1+ (lexer-pos lx)) (lexer-end lx))
                (= (aref (lexer-buf lx) (1+ (lexer-pos lx))) 62))
           (progn (incf (lexer-pos lx) 2) (make-token :dict-close nil))
           ;; stray '>' — skip and retry (lenient)
           (progn (incf (lexer-pos lx)) (%lex-read lx))))
      ((= b 123) (incf (lexer-pos lx)) (make-token :keyword "{"))  ; PostScript-calc { }
      ((= b 125) (incf (lexer-pos lx)) (make-token :keyword "}"))
      ((= b 41) (incf (lexer-pos lx)) (%lex-read lx))              ; stray ')' — skip
      ((or (digit-byte-p b) (= b 43) (= b 45) (= b 46))            ; number  + - .
       (%lex-number lx))
      (t (%lex-keyword lx)))))

(defun %lex-number (lx)
  "Numbers: integer or real, optional leading + - and leading/embedded '.'."
  (let* ((buf (lexer-buf lx))
         (start (lexer-pos lx))
         (realp nil))
    (when (and (< (lexer-pos lx) (lexer-end lx))
               (let ((b (aref buf (lexer-pos lx)))) (or (= b 43) (= b 45))))
      (incf (lexer-pos lx)))
    (loop while (< (lexer-pos lx) (lexer-end lx))
          for b = (aref buf (lexer-pos lx))
          do (cond ((digit-byte-p b) (incf (lexer-pos lx)))
                   ((= b 46) (setf realp t) (incf (lexer-pos lx)))
                   ((or (= b 43) (= b 45)) (incf (lexer-pos lx))) ; lenient: 1-2 etc.
                   (t (return))))
    (let ((s (map 'string #'code-char (subseq buf start (lexer-pos lx)))))
      (if realp
          (make-token :real (or (parse-pdf-real s) 0))
          (make-token :int (or (ignore-errors (parse-integer s :junk-allowed t)) 0))))))

(defun parse-pdf-real (s)
  "Parse a PDF real like \"-.12\", \"3.\", \"+4.5\" -> double-float."
  (let* ((s (string-trim " " s))
         (neg (and (> (length s) 0) (char= (char s 0) #\-)))
         (s (if (and (> (length s) 0) (member (char s 0) '(#\+ #\-)))
                (subseq s 1) s))
         (dot (position #\. s))
         (int-part (if dot (subseq s 0 dot) s))
         (frac-part (if dot (subseq s (1+ dot)) "")))
    (let ((iv (if (plusp (length int-part))
                  (or (ignore-errors (parse-integer int-part :junk-allowed t)) 0) 0))
          (fv 0) (scale 1))
      (loop for c across frac-part
            while (digit-char-p c)
            do (setf fv (+ (* fv 10) (digit-char-p c)) scale (* scale 10)))
      (let ((v (+ iv (/ (float fv 1d0) scale))))
        (if neg (- v) v)))))

(defun %lex-name (lx)
  "/Name with #xx hex escapes (§7.3.5).  Returns a :name token (string value)."
  (let ((buf (lexer-buf lx))
        (out (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (incf (lexer-pos lx))               ; skip '/'
    (loop while (< (lexer-pos lx) (lexer-end lx))
          for b = (aref buf (lexer-pos lx))
          while (regular-byte-p b)
          do (cond ((and (= b 35)        ; '#' hex escape
                         (< (+ 2 (lexer-pos lx)) (lexer-end lx))
                         (hex-byte-p (aref buf (+ 1 (lexer-pos lx))))
                         (hex-byte-p (aref buf (+ 2 (lexer-pos lx)))))
                    (vector-push-extend
                     (+ (* 16 (hex-val (aref buf (+ 1 (lexer-pos lx)))))
                        (hex-val (aref buf (+ 2 (lexer-pos lx)))))
                     out)
                    (incf (lexer-pos lx) 3))
                   (t (vector-push-extend b out) (incf (lexer-pos lx)))))
    ;; Names are conventionally UTF-8/ASCII text; keep as latin1 string.
    (make-token :name (map 'string #'code-char out))))

(defun %lex-literal-string (lx)
  "Literal string (...) with balanced parens & backslash escapes (§7.3.4.2)."
  (let ((buf (lexer-buf lx))
        (out (make-array 32 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (depth 0))
    (incf (lexer-pos lx))               ; skip '('
    (incf depth)
    (loop while (and (> depth 0) (< (lexer-pos lx) (lexer-end lx)))
          for b = (aref buf (lexer-pos lx))
          do (cond
               ((= b 92)                ; backslash escape
                (incf (lexer-pos lx))
                (when (< (lexer-pos lx) (lexer-end lx))
                  (let ((e (aref buf (lexer-pos lx))))
                    (case e
                      (110 (vector-push-extend 10 out) (incf (lexer-pos lx)))  ; \n
                      (114 (vector-push-extend 13 out) (incf (lexer-pos lx)))  ; \r
                      (116 (vector-push-extend 9 out)  (incf (lexer-pos lx)))  ; \t
                      (98  (vector-push-extend 8 out)  (incf (lexer-pos lx)))  ; \b
                      (102 (vector-push-extend 12 out) (incf (lexer-pos lx)))  ; \f
                      (40  (vector-push-extend 40 out) (incf (lexer-pos lx)))  ; \(
                      (41  (vector-push-extend 41 out) (incf (lexer-pos lx)))  ; \)
                      (92  (vector-push-extend 92 out) (incf (lexer-pos lx)))  ; \\
                      ((13 10)          ; line continuation: \<EOL> -> nothing
                       (incf (lexer-pos lx))
                       (when (and (= e 13) (< (lexer-pos lx) (lexer-end lx))
                                  (= (aref buf (lexer-pos lx)) 10))
                         (incf (lexer-pos lx))))
                      (t (if (<= 48 e 55)  ; \ddd octal (1-3 digits)
                             (let ((val 0) (n 0))
                               (loop while (and (< n 3)
                                                (< (lexer-pos lx) (lexer-end lx))
                                                (<= 48 (aref buf (lexer-pos lx)) 55))
                                     do (setf val (+ (* val 8)
                                                     (- (aref buf (lexer-pos lx)) 48)))
                                        (incf n) (incf (lexer-pos lx)))
                               (vector-push-extend (logand val #xff) out))
                             (progn (vector-push-extend e out)
                                    (incf (lexer-pos lx)))))))))
               ((= b 40) (incf depth) (vector-push-extend 40 out) (incf (lexer-pos lx)))
               ((= b 41) (decf depth)
                (when (> depth 0) (vector-push-extend 41 out))
                (incf (lexer-pos lx)))
               (t (vector-push-extend b out) (incf (lexer-pos lx)))))
    (make-token :string (subseq-octets out))))

(defun subseq-octets (adj)
  "Copy an adjustable octet buffer into a simple (unsigned-byte 8) vector."
  (let ((v (make-array (length adj) :element-type '(unsigned-byte 8))))
    (replace v adj)
    v))

(defun %lex-hex-string (lx)
  "Hex string <...>  (§7.3.4.3).  Odd final nibble is padded with 0."
  (let ((buf (lexer-buf lx))
        (out (make-array 32 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (hi nil))
    (incf (lexer-pos lx))               ; skip '<'
    (loop while (< (lexer-pos lx) (lexer-end lx))
          for b = (aref buf (lexer-pos lx))
          do (cond ((= b 62) (incf (lexer-pos lx)) (return)) ; '>'
                   ((ws-byte-p b) (incf (lexer-pos lx)))
                   ((hex-byte-p b)
                    (let ((v (hex-val b)))
                      (if hi
                          (progn (vector-push-extend (+ (* 16 hi) v) out) (setf hi nil))
                          (setf hi v)))
                    (incf (lexer-pos lx)))
                   (t (incf (lexer-pos lx))))) ; ignore stray
    (when hi (vector-push-extend (* 16 hi) out)) ; odd nibble -> pad low 0
    (make-token :string (subseq-octets out))))

(defun %lex-keyword (lx)
  "A bareword keyword: obj endobj R stream endstream true false null xref
trailer startxref, or any other run of regular characters."
  (let ((buf (lexer-buf lx))
        (start (lexer-pos lx)))
    (loop while (and (< (lexer-pos lx) (lexer-end lx))
                     (regular-byte-p (aref buf (lexer-pos lx))))
          do (incf (lexer-pos lx)))
    (if (= (lexer-pos lx) start)
        ;; not a regular char (shouldn't happen) — skip one byte, retry
        (progn (incf (lexer-pos lx)) (%lex-read lx))
        (make-token :keyword (map 'string #'code-char
                                  (subseq buf start (lexer-pos lx)))))))
