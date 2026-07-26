;;;; src/xref.lisp — file structure: cross-reference + indirect resolution.
;;;; (ISO 32000-1 §7.5: file structure, classic xref, xref streams, object
;;;; streams, incremental updates via /Prev, and a scan-based recovery path.)

(in-package #:folio)

;;; ---- the document -----------------------------------------------------

(defstruct (document (:constructor %make-document))
  (bytes #() :type (simple-array (unsigned-byte 8) (*)))
  (xref (make-hash-table))              ; object-number -> entry
  (gens (make-hash-table))              ; object-number -> generation
  trailer                               ; merged trailer dict
  (version "1.0")
  (cache (make-hash-table))             ; object-number -> resolved object
  (objstm-cache (make-hash-table))      ; objstm-number -> (data first pairs)
  (pages nil)                           ; cached vector of leaf page entries
  (crypt nil)                           ; CRYPT struct when /Encrypt is present
  (recovered nil))                      ; T if built by scan fallback

;; entry ::= (:offset byte-offset) | (:compressed objstm-num index)

(defun add-entry (doc num entry gen &optional overwrite)
  "Record NUM -> ENTRY unless already present (newer xref sections are processed
first, so the first write wins), or unconditionally when OVERWRITE."
  (when (or overwrite (not (gethash num (document-xref doc))))
    (setf (gethash num (document-xref doc)) entry
          (gethash num (document-gens doc)) gen)))

(defun object-count (doc)
  "Number of in-use objects known to the cross-reference table."
  (hash-table-count (document-xref doc)))

(defun be-int (data pos n)
  "Read an N-byte big-endian unsigned integer from DATA at POS."
  (let ((v 0))
    (dotimes (i n v) (setf v (logior (ash v 8) (aref data (+ pos i)))))))

;;; ---- resolution -------------------------------------------------------

(defun resolve (doc x)
  "Resolve X to a concrete object.  X may be a PDF-REF, an object number
(integer), or any direct value (returned unchanged).  Lazily loaded + cached;
follows object-stream indirection.  Never signals: a dangling ref -> PDF null."
  (cond ((pdf-ref-p x) (resolve-num doc (pdf-ref-num x)))
        ((integerp x)  x)               ; plain integers are values, not refs
        (t x)))

(defun resolve-ref-num (doc num)
  "Resolve a bare object NUMBER to its object (the (num . nil) form)."
  (resolve-num doc num))

(defun resolve-num (doc num)
  (multiple-value-bind (cached hit) (gethash num (document-cache doc))
    (if hit
        cached
        (let ((v (handler-case (load-object doc num)
                   (error () (pdf-null)))))
          (setf (gethash num (document-cache doc)) v)
          v))))

(defun load-object (doc num)
  (let ((e (gethash num (document-xref doc))))
    (cond
      ((null e) (pdf-null))
      ((eq (first e) :offset)
       (multiple-value-bind (obj onum ogen)
           (parse-indirect-object (document-bytes doc) (second e)
                                  :resolver (lambda (v) (resolve doc v)))
         (maybe-decrypt-object doc obj (or onum num) (or ogen 0))))
      ((eq (first e) :compressed)
       (load-compressed doc (second e) (third e)))
      (t (pdf-null)))))

(defun load-compressed (doc objstm-num index)
  "Load object at INDEX from object stream OBJSTM-NUM (§7.5.7)."
  (let ((entry (gethash objstm-num (document-objstm-cache doc))))
    (unless entry
      (let ((stm (resolve-num doc objstm-num)))
        (unless (pdf-stream-p stm)
          (return-from load-compressed (pdf-null)))
        (let* ((res (lambda (v) (resolve doc v)))
               (data (decode-stream stm :resolver res))
               (dict (pdf-stream-dict stm))
               (n (resolve doc (dict-get dict "N")))
               (first (resolve doc (dict-get dict "First")))
               (lx (make-lexer data))
               (pairs (make-array (max 0 (if (integerp n) n 0)))))
          (dotimes (i (length pairs))
            (let ((onum (lex-next lx)) (ooff (lex-next lx)))
              (setf (aref pairs i) (cons (token-value onum) (token-value ooff)))))
          (setf entry (list data first pairs))
          (setf (gethash objstm-num (document-objstm-cache doc)) entry))))
    (destructuring-bind (data first pairs) entry
      (if (< index (length pairs))
          (let ((off (+ first (cdr (aref pairs index)))))
            (values (parse-object (make-lexer data :start off)
                                  (lambda (v) (resolve doc v)))))
          (pdf-null)))))

;;; ---- startxref + version ----------------------------------------------

(defun find-startxref (bytes)
  "Return the byte offset named by the last `startxref` near the file tail."
  (let* ((n (length bytes))
         (from (max 0 (- n 2048)))
         (pos (search-bytes bytes from n "startxref")))
    ;; take the LAST occurrence
    (loop for p = (and pos (search-bytes bytes (1+ pos) n "startxref"))
          while p do (setf pos p))
    (when pos
      (let ((lx (make-lexer bytes :start (+ pos 9) :end n)))
        (let ((tk (lex-next lx)))
          (when (eq (token-type tk) :int) (token-value tk)))))))

(defun read-version (bytes)
  "Parse the `%PDF-x.y` header (may be preceded by junk in the first 1 KB)."
  (let ((p (search-bytes bytes 0 (min (length bytes) 1024) "%PDF-")))
    (if (and p (<= (+ p 8) (length bytes)))
        (map 'string #'code-char (subseq bytes (+ p 5) (+ p 8)))
        "1.0")))

;;; ---- cross-reference reading ------------------------------------------

(defun read-xref (doc)
  "Populate DOC's xref table + trailer from the file, following /Prev and
/XRefStm; fall back to a whole-file scan on any failure or a missing /Root."
  (let ((ok (ignore-errors (load-xref-chain doc) t)))
    (when (or (not ok)
              (not (hash-table-p (document-trailer doc)))
              (not (dict-get (document-trailer doc) "Root"))
              (zerop (object-count doc)))
      (scan-rebuild doc)))
  doc)

(defun load-xref-chain (doc)
  (let ((start (find-startxref (document-bytes doc)))
        (visited (make-hash-table))
        (trailers '()))
    (unless start (error "folio: no startxref"))
    (labels ((visit (off)
               (when (and (integerp off) (>= off 0)
                          (< off (length (document-bytes doc)))
                          (not (gethash off visited)))
                 (setf (gethash off visited) t)
                 (let ((tr (parse-one-xref doc off)))
                   (when (hash-table-p tr)
                     (push tr trailers)
                     (let ((xs (dict-get tr "XRefStm"))
                           (pv (dict-get tr "Prev")))
                       (when (integerp xs) (visit xs))
                       (when (integerp pv) (visit pv))))))))
      (visit start))
    ;; trailers list is oldest-at-front (deepest /Prev pushed last); merging in
    ;; that order lets the newest section's keys win.
    (let ((h (make-hash-table :test 'equal)))
      (dolist (tr trailers)
        (maphash (lambda (k v) (setf (gethash k h) v)) tr))
      (setf (document-trailer doc) h))
    (when (zerop (object-count doc)) (error "folio: empty xref"))))

(defun parse-one-xref (doc off)
  "Parse the cross-reference section at OFF (classic table or xref stream);
return its trailer dict."
  (let* ((bytes (document-bytes doc))
         (lx (make-lexer bytes :start off))
         (tk (lex-peek lx)))
    (if (and (eq (token-type tk) :keyword) (string= (token-value tk) "xref"))
        (parse-classic-xref doc lx)
        (parse-xref-stream doc off))))

(defun parse-classic-xref (doc lx)
  (lex-next lx)                         ; consume `xref`
  (loop
    (let ((tk (lex-peek lx)))
      (cond
        ((and (eq (token-type tk) :keyword) (string= (token-value tk) "trailer"))
         (lex-next lx)
         (return (parse-object lx)))
        ((eq (token-type tk) :int)
         (let ((start (token-value (lex-next lx)))
               (count (token-value (lex-next lx))))
           (dotimes (i count)
             (let ((o (lex-next lx)) (g (lex-next lx)) (ty (lex-next lx)))
               (when (and (eq (token-type o) :int)
                          (eq (token-type ty) :keyword)
                          (string= (token-value ty) "n"))
                 (add-entry doc (+ start i) (list :offset (token-value o))
                            (if (eq (token-type g) :int) (token-value g) 0)))))))
        (t (return nil))))))

(defun parse-xref-stream (doc off)
  (let ((obj (parse-indirect-object (document-bytes doc) off
                                    :resolver (lambda (v) (resolve doc v)))))
    (unless (pdf-stream-p obj) (return-from parse-xref-stream nil))
    (let* ((dict (pdf-stream-dict obj))
           (data (decode-stream obj :resolver (lambda (v) (resolve doc v))))
           (w (mapcar (lambda (x) (resolve doc x))
                      (%as-list (dict-get dict "W"))))
           (w1 (or (first w) 1)) (w2 (or (second w) 1)) (w3 (or (third w) 1))
           (rec (+ w1 w2 w3))
           (size (let ((s (resolve doc (dict-get dict "Size")))) (if (integerp s) s 0)))
           (index (let ((ix (dict-get dict "Index")))
                    (if (pdf-array-p ix)
                        (map 'list (lambda (x) (resolve doc x)) ix)
                        (list 0 size))))
           (pos 0))
      (loop for (start cnt) on index by #'cddr
            while (integerp start) do
        (dotimes (i (if (integerp cnt) cnt 0))
          (when (<= (+ pos rec) (length data))
            (let ((f1 (if (zerop w1) 1 (be-int data pos w1)))
                  (f2 (be-int data (+ pos w1) w2))
                  (f3 (be-int data (+ pos w1 w2) w3))
                  (num (+ start i)))
              (case f1
                (1 (add-entry doc num (list :offset f2) f3))
                (2 (add-entry doc num (list :compressed f2 f3) 0)))
              (incf pos rec)))))
      dict)))

;;; ---- scan-based recovery (§ "malformed files are the norm") -----------

(defun scan-rebuild (doc)
  "Rebuild the xref by scanning the whole file for `N G obj`; later definitions
win.  Expand any object streams found, and synthesize a trailer by locating the
/Type /Catalog object when none is present."
  (setf (document-recovered doc) t)
  (clrhash (document-xref doc))
  (clrhash (document-cache doc))
  (clrhash (document-objstm-cache doc))
  (let* ((bytes (document-bytes doc))
         (n (length bytes)))
    ;; pass 1: every `<int> <int> obj`
    (let ((i 0))
      (loop while (< i n) do
        (let ((p (search-bytes bytes i n " obj")))
          (unless p (return))
          ;; walk back over `<int> <int>` before " obj"
          (multiple-value-bind (num gen start) (backscan-obj-header bytes p)
            (when num
              (add-entry doc num (list :offset start) (or gen 0) t)))
          (setf i (+ p 4)))))
    ;; pass 2: expand object streams so their members resolve
    (let ((objstms '()))
      (maphash (lambda (num e)
                 (when (eq (first e) :offset)
                   (let ((obj (ignore-errors
                                (values (parse-indirect-object bytes (second e)
                                          :resolver (lambda (v) (resolve doc v)))))))
                     (when (and (pdf-stream-p obj)
                                (name= (dict-get (pdf-stream-dict obj) "Type") "ObjStm"))
                       (push num objstms)))))
               (document-xref doc))
      (dolist (osnum objstms)
        (ignore-errors
          (let* ((stm (resolve-num doc osnum))
                 (data (decode-stream stm :resolver (lambda (v) (resolve doc v))))
                 (dict (pdf-stream-dict stm))
                 (cnt (resolve doc (dict-get dict "N")))
                 (lx (make-lexer data)))
            (dotimes (idx (if (integerp cnt) cnt 0))
              (let ((onum (token-value (lex-next lx))))
                (lex-next lx)           ; skip offset
                (when (integerp onum)
                  (add-entry doc onum (list :compressed osnum idx) 0 t))))))))
    ;; trailer: prefer an existing one, else find the Catalog
    (let ((tr (or (find-trailer-dict doc) (synthesize-trailer doc))))
      (setf (document-trailer doc) tr)))
  doc)

(defun backscan-obj-header (bytes obj-pos)
  "Given the index of the space before `obj`, recover the `N G` before it.
Returns (values num gen start-offset) or NILs."
  (let ((i obj-pos))
    (flet ((skip-ws-back () (loop while (and (> i 0) (ws-byte-p (aref bytes (1- i)))) do (decf i)))
           (read-int-back ()
             (let ((e i))
               (loop while (and (> i 0) (digit-byte-p (aref bytes (1- i)))) do (decf i))
               (if (< i e)
                   (values (parse-integer (map 'string #'code-char (subseq bytes i e))) i)
                   (values nil nil)))))
      (skip-ws-back)
      (multiple-value-bind (gen gstart) (read-int-back)
        (declare (ignore gstart))
        (unless gen (return-from backscan-obj-header (values nil nil nil)))
        (skip-ws-back)
        (multiple-value-bind (num nstart) (read-int-back)
          (if num (values num gen nstart) (values nil nil nil)))))))

(defun find-trailer-dict (doc)
  "Scan for the last `trailer` keyword and parse its dict; keep it only if it
carries a /Root."
  (let* ((bytes (document-bytes doc))
         (n (length bytes))
         (pos nil))
    (loop for p = (search-bytes bytes (if pos (1+ pos) 0) n "trailer")
          while p do (setf pos p))
    (when pos
      (let ((tr (ignore-errors
                  (parse-object (make-lexer bytes :start (+ pos 7))))))
        (when (and (hash-table-p tr) (dict-get tr "Root")) tr)))))

(defun synthesize-trailer (doc)
  "Find the /Type /Catalog object and fabricate a trailer pointing /Root at it."
  (let ((root-num nil))
    (maphash (lambda (num e)
               (declare (ignore e))
               (unless root-num
                 (let ((obj (resolve-num doc num)))
                   (when (and (hash-table-p obj)
                              (name= (dict-get obj "Type") "Catalog"))
                     (setf root-num num)))))
             (document-xref doc))
    (let ((h (make-hash-table :test 'equal)))
      (when root-num
        (setf (gethash "Root" h) (make-pdf-ref root-num 0)))
      h)))
