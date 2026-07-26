;;;; src/cmap.lisp — CMap parsing + Adobe predefined CJK CMap resolution (§9.7.5).
;;;;
;;;; A Type0 font's /Encoding is a CMap: it (a) defines the codespace ranges that
;;;; segment a text-showing byte string into 1..4-byte codes (CJK CMaps are
;;;; mixed-width, e.g. Shift-JIS uses 1- and 2-byte codes) and (b) maps each code
;;;; to a CID in the descendant CIDFont's registry.  The /Encoding may be
;;;;   * a name Identity-H / Identity-V     -> 2-byte code, CID = code;
;;;;   * a named predefined Adobe CMap      -> resolved from the vendored bundle
;;;;     (Adobe-Japan1 / GB1 / CNS1 / Korea1 / KR, UCS2/UTF16 + the national
;;;;     RKSJ/EUC/Big5/GBK/KSC encodings, both -H and -V);
;;;;   * an embedded CMap stream            -> parsed by PARSE-EMBEDDED-CMAP.
;;;;
;;;; The predefined CMap data is preprocessed by test/gen-cmap-data.py from the
;;;; public Adobe cmap-resources set (BSD-style licence) into data/cmaps.bin: a
;;;; single indexed bundle of per-CMap deflate-compressed range tables.  usecmap
;;;; inheritance is stored as a parent-name reference and resolved here, so a
;;;; shared base CMap (e.g. a -H that its -V usecmaps) is stored only once.

(in-package #:folio)

;;; ---- the decoded CMap ------------------------------------------------------
(defstruct cmapo
  (identity nil)        ; T for Identity-H / Identity-V (2-byte code, CID = code)
  (wmode 0)             ; 0 = horizontal, 1 = vertical (from /WMode)
  (ranges nil)          ; codespace: list of (nbytes lo hi)
  (chars nil)           ; hash code -> CID (begincidchar)
  (cidranges nil)       ; construction-time list of (lo hi cid) (begincidrange)
  (cidrange-vec nil)    ; finalized simple-vector of #(lo hi cid) sorted by lo
  (parent nil))         ; usecmap base CMapo (mappings fall through), or NIL

(defun bytes->uint (v &optional (start 0) (end (length v)))
  (loop with n = 0 for i from start below end do (setf n (logior (ash n 8) (aref v i)))
        finally (return n)))

;;; ---- finalize: sort cidranges into a binary-searchable vector --------------
(defun finalize-cmap (cm)
  "Sort CM's cidrange list into CIDRANGE-VEC for O(log n) lookup and return CM."
  (let ((lst (cmapo-cidranges cm)))
    (when lst
      (let ((vec (map 'vector (lambda (r) (vector (first r) (second r) (third r))) lst)))
        (sort vec #'< :key (lambda (e) (aref e 0)))
        (setf (cmapo-cidrange-vec cm) vec))))
  cm)

(defun %cidrange-lookup (vec code)
  "Binary-search VEC (sorted #(lo hi cid) by lo) for the range containing CODE;
return its CID or NIL."
  (when vec
    (let ((lo 0) (hi (1- (length vec))) (best nil))
      (loop while (<= lo hi) do
        (let* ((mid (ash (+ lo hi) -1)) (e (aref vec mid)))
          (cond ((< code (aref e 0)) (setf hi (1- mid)))
                (t (setf best e lo (1+ mid))))))    ; last entry with lo <= code
      (when (and best (<= (aref best 0) code (aref best 1)))
        (+ (aref best 2) (- code (aref best 0)))))))

;;; ---- byte-string -> code segmentation + code -> CID ------------------------
(defun cmap-codespace (cm)
  "The codespace ranges governing CM: its own if any, else the nearest parent's."
  (or (cmapo-ranges cm)
      (let ((p (cmapo-parent cm))) (and p (cmap-codespace p)))))

(defun %code-matches (bytes i nbytes lo hi)
  "True when the NBYTES bytes at BYTES[I..] lie byte-wise within [LO,HI]."
  (loop for j from 0 below nbytes
        for shift = (* 8 (- nbytes 1 j))
        for b = (aref bytes (+ i j))
        always (<= (ldb (byte 8 shift) lo) b (ldb (byte 8 shift) hi))))

(defun cmap-next-code (cm bytes i)
  "Return (values code nbytes) for the code beginning at BYTES[I], sizing it with
CM's codespace ranges (mixed-width aware: 1..4 bytes)."
  (let ((n (length bytes)))
    (when (cmapo-identity cm)
      (let ((nb (min 2 (- n i))))
        (return-from cmap-next-code (values (bytes->uint bytes i (+ i nb)) (max 1 nb)))))
    (let ((ranges (cmap-codespace cm)))
      (unless ranges
        (let ((nb (min 2 (- n i))))
          (return-from cmap-next-code (values (bytes->uint bytes i (+ i nb)) (max 1 nb)))))
      ;; try codespace ranges shortest-first; a byte-wise match fixes the width
      (loop for len from 1 to 4 do
        (dolist (r ranges)
          (destructuring-bind (nb lo hi) r
            (when (and (= nb len) (<= (+ i nb) n) (%code-matches bytes i nb lo hi))
              (return-from cmap-next-code (values (bytes->uint bytes i (+ i nb)) nb))))))
      ;; no range matched (malformed byte): consume the shortest codespace width
      (let* ((minw (reduce #'min ranges :key #'first :initial-value 4))
             (nb (max 1 (min minw (- n i)))))
        (values (bytes->uint bytes i (+ i nb)) nb)))))

(defun %cmap-lookup (cm code)
  "CID for CODE via CM's own tables then its usecmap parent, or NIL if unmapped."
  (when cm
    (or (let ((h (cmapo-chars cm))) (and h (gethash code h)))
        (%cidrange-lookup (cmapo-cidrange-vec cm) code)
        (%cmap-lookup (cmapo-parent cm) code))))

(defun cmap-code->cid (cm code)
  "Map a CODE to a CID through CM (Identity -> CODE; else tables, then parent)."
  (if (cmapo-identity cm) code (or (%cmap-lookup cm code) 0)))

;;; ---- embedded CMap streams (the PostScript CMap format) --------------------
(defun parse-embedded-cmap (bytes &optional (resolve-usecmap #'load-predefined-cmap))
  "Parse a CMap stream body into a CMAPO.  Recognises begincodespacerange,
begincidrange, begincidchar, /WMode and usecmap (a named base, resolved via
RESOLVE-USECMAP, else Identity)."
  (let ((lx (make-lexer bytes))
        (cm (make-cmapo :chars (make-hash-table :test 'eql)))
        (pending '()))                  ; recent operands (newest first)
    (labels ((hexint (tok) (bytes->uint (token-value tok)))
             (hexlen (tok) (length (token-value tok))))
      (loop for tok = (lex-next lx)
            until (eq (token-type tok) :eof) do
        (case (token-type tok)
          ((:string :int :real :name) (push tok pending))
          (:keyword
           (let ((kw (token-value tok)))
             (cond
               ((string= kw "begincodespacerange")
                (loop for lo = (lex-next lx)
                      until (or (eq (token-type lo) :eof)
                                (and (eq (token-type lo) :keyword)
                                     (string= (token-value lo) "endcodespacerange")))
                      do (let ((hi (lex-next lx)))
                           (when (and (eq (token-type lo) :string) (eq (token-type hi) :string))
                             (push (list (hexlen lo) (hexint lo) (hexint hi))
                                   (cmapo-ranges cm))))))
               ((string= kw "begincidrange")
                (loop for lo = (lex-next lx)
                      until (or (eq (token-type lo) :eof)
                                (and (eq (token-type lo) :keyword)
                                     (string= (token-value lo) "endcidrange")))
                      do (let ((hi (lex-next lx)) (cid (lex-next lx)))
                           (when (and (eq (token-type lo) :string) (eq (token-type hi) :string)
                                      (member (token-type cid) '(:int :real)))
                             (push (list (hexint lo) (hexint hi) (round (token-value cid)))
                                   (cmapo-cidranges cm))))))
               ((string= kw "begincidchar")
                (loop for co = (lex-next lx)
                      until (or (eq (token-type co) :eof)
                                (and (eq (token-type co) :keyword)
                                     (string= (token-value co) "endcidchar")))
                      do (let ((cid (lex-next lx)))
                           (when (and (eq (token-type co) :string)
                                      (member (token-type cid) '(:int :real)))
                             (setf (gethash (hexint co) (cmapo-chars cm)) (round (token-value cid)))))))
               ((string= kw "def")
                ;; /WMode <int> def
                (let ((val (first pending)) (key (second pending)))
                  (when (and val key (eq (token-type key) :name)
                             (string= (token-value key) "WMode")
                             (member (token-type val) '(:int :real)))
                    (setf (cmapo-wmode cm) (round (token-value val))))))
               ((string= kw "usecmap")
                (let ((nm (find-if (lambda (tk) (eq (token-type tk) :name)) pending)))
                  (when nm
                    (let ((base (token-value nm)))
                      (cond ((search "Identity" base) (setf (cmapo-identity cm) t))
                            (t (let ((p (ignore-errors (funcall resolve-usecmap base))))
                                 (when p (setf (cmapo-parent cm) p)))))))))
               (t nil))
             (setf pending '())))
          (t (setf pending '())))))
    ;; a CMap with neither its own nor an inherited codespace defaults to 2-byte
    (unless (cmap-codespace cm) (setf (cmapo-ranges cm) (list (list 2 0 #xFFFF))))
    (finalize-cmap cm)))

;;; ===========================================================================
;;; Predefined Adobe CMap bundle (data/cmaps.bin)
;;; ===========================================================================
;;; Layout: "FCMB" u16:count  then a directory of {u8 namelen, name, u32 off,
;;; u32 len} entries, then the concatenated per-CMap blobs.  Each blob is a
;;; deflate-compressed FCM1 record (see test/gen-cmap-data.py).

(defvar *cmap-bundle-bytes* nil "Raw bytes of data/cmaps.bin, loaded once.")
(defvar *cmap-directory* nil     "Hash name -> (cons offset length) into the bundle.")
(defvar *predefined-cmap-cache* (make-hash-table :test 'equal)
  "Resolved predefined CMAPO objects, keyed by name (usecmap chains resolved).")

(defun %cmap-bundle-path ()
  (or (ignore-errors (asdf:system-relative-pathname :folio "data/cmaps.bin"))
      #.(when (or *compile-file-truename* *load-truename*)
          (merge-pathnames "../data/cmaps.bin"
                           (or *compile-file-truename* *load-truename*)))))

(defun ensure-cmap-bundle ()
  "Load and index data/cmaps.bin on first use.  Returns T if the bundle is
available, NIL if it could not be read (predefined CMaps then fall back)."
  (when *cmap-directory* (return-from ensure-cmap-bundle t))
  (let ((path (%cmap-bundle-path)))
    (when (and path (probe-file path))
      (handler-case
          (let ((bytes (with-open-file (s path :element-type '(unsigned-byte 8))
                         (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
                           (read-sequence b s) b))))
            (unless (and (>= (length bytes) 6)
                         (= (aref bytes 0) (char-code #\F)) (= (aref bytes 1) (char-code #\C))
                         (= (aref bytes 2) (char-code #\M)) (= (aref bytes 3) (char-code #\B)))
              (return-from ensure-cmap-bundle nil))
            (let* ((count (bytes->uint bytes 4 6))
                   (dir (make-hash-table :test 'equal))
                   (p 6))
              (dotimes (k count)
                (let* ((nl (aref bytes p))
                       (name (map 'string #'code-char (subseq bytes (+ p 1) (+ p 1 nl))))
                       (off (bytes->uint bytes (+ p 1 nl) (+ p 1 nl 4)))
                       (len (bytes->uint bytes (+ p 1 nl 4) (+ p 1 nl 8))))
                  (setf (gethash name dir) (cons off len))
                  (setf p (+ p 1 nl 8))))
              (setf *cmap-bundle-bytes* bytes *cmap-directory* dir)
              t))
        (error () nil)))))

(defun %read-u32 (v i) (bytes->uint v i (+ i 4)))

(defun parse-compact-cmap (blob)
  "Decode one decompressed FCM1 record into a CMAPO (parent left as a NAME string
in the returned second value for the caller to resolve)."
  (unless (and (>= (length blob) 6)
               (= (aref blob 0) (char-code #\F)) (= (aref blob 1) (char-code #\C))
               (= (aref blob 2) (char-code #\M)) (= (aref blob 3) (char-code #\1)))
    (return-from parse-compact-cmap (values nil nil)))
  (let* ((wmode (aref blob 4))
         (unl (aref blob 5))
         (p 6)
         (uname (when (plusp unl) (map 'string #'code-char (subseq blob p (+ p unl)))))
         (cm (make-cmapo :wmode wmode :chars (make-hash-table :test 'eql))))
    (incf p unl)
    (let ((ncs (bytes->uint blob p (+ p 2))))
      (incf p 2)
      (dotimes (k ncs)
        (let ((nb (aref blob p)) (lo (%read-u32 blob (+ p 1))) (hi (%read-u32 blob (+ p 5))))
          (push (list nb lo hi) (cmapo-ranges cm))
          (incf p 9))))
    (let ((ncr (%read-u32 blob p)))
      (incf p 4)
      (let ((lst '()))
        (dotimes (k ncr)
          (let ((lo (%read-u32 blob p)) (hi (%read-u32 blob (+ p 4)))
                (cid (bytes->uint blob (+ p 8) (+ p 10))))
            (push (list lo hi cid) lst)
            (incf p 10)))
        (setf (cmapo-cidranges cm) lst)))
    (let ((ncc (%read-u32 blob p)))
      (incf p 4)
      (dotimes (k ncc)
        (let ((code (%read-u32 blob p)) (cid (bytes->uint blob (+ p 4) (+ p 6))))
          (setf (gethash code (cmapo-chars cm)) cid)
          (incf p 6))))
    (values (finalize-cmap cm) uname)))

(defun load-predefined-cmap (name &optional (seen '()))
  "Return the CMAPO for a predefined CMap NAME from the bundle (cached), resolving
its usecmap parent chain.  Identity-* -> an identity CMAPO.  Unknown / missing ->
NIL (caller falls back to 2-byte identity)."
  (cond
    ((null name) nil)
    ((search "Identity" name) (make-cmapo :identity t :ranges (list (list 2 0 #xFFFF))))
    ((gethash name *predefined-cmap-cache*))
    ((member name seen :test #'string=) nil)   ; usecmap cycle guard
    ((not (ensure-cmap-bundle)) nil)
    (t
     (let ((ent (gethash name *cmap-directory*)))
       (when ent
         (let* ((off (car ent)) (len (cdr ent))
                (comp (subseq *cmap-bundle-bytes* off (+ off len)))
                (blob (ignore-errors (cram:zlib-decompress comp))))
           (when blob
             (multiple-value-bind (cm uname) (parse-compact-cmap blob)
               (when cm
                 (when uname
                   (setf (cmapo-parent cm) (load-predefined-cmap uname (cons name seen))))
                 (setf (gethash name *predefined-cmap-cache*) cm)
                 cm)))))))))

;;; ---- /Encoding value -> CMAPO ----------------------------------------------
(defun cmap-for-encoding (doc enc)
  "Build a CMAPO from a Type0 /Encoding value: a predefined name (Identity or a
bundled Adobe CJK CMap) or an embedded CMap stream.  Unknown names fall back to a
2-byte identity CMap so text still advances."
  (cond
    ((pdf-name-p enc)
     (let ((n (pdf-name-string enc)))
       (cond
         ((search "Identity" n) (make-cmapo :identity t :ranges (list (list 2 0 #xFFFF))))
         ((load-predefined-cmap n))
         (t (make-cmapo :identity t :ranges (list (list 2 0 #xFFFF)))))))
    ((pdf-stream-p enc)
     (or (ignore-errors
          (parse-embedded-cmap (decode-stream enc :resolver (lambda (v) (resolve doc v)))))
         (make-cmapo :identity t :ranges (list (list 2 0 #xFFFF)))))
    (t (make-cmapo :identity t :ranges (list (list 2 0 #xFFFF))))))
