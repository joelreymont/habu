;;; habu0.lisp - Minimal standalone Habu compiler/interpreter
;;;
;;; This is the entry point for the self-hosting compiler.
;;; It reads a Lisp source file, parses it, and executes it.
;;;
;;; For now, it uses a simple expression evaluator.
;;; Full compilation to native code will be added later.

;; Cached operator symbols - set on first use during eval
;; Once a symbol is identified as an operator, we cache it for eq comparison
(defvar *op-quote* nil)
(defvar *op-if* nil)
(defvar *op-let* nil)
(defvar *op-let-star* nil)
(defvar *op-defun* nil)
(defvar *op-defvar* nil)
(defvar *op-while* nil)
(defvar *op-progn* nil)
(defvar *op-cond* nil)
(defvar *op-t* nil)
(defvar *op-plus* nil)
(defvar *op-minus* nil)
(defvar *op-mul* nil)
(defvar *op-div* nil)
(defvar *op-mod* nil)
(defvar *op-eq-num* nil)
(defvar *op-lt* nil)
(defvar *op-gt* nil)
(defvar *op-le* nil)
(defvar *op-ge* nil)
(defvar *op-cons* nil)
(defvar *op-car* nil)
(defvar *op-cdr* nil)
(defvar *op-cadr* nil)
(defvar *op-cddr* nil)
(defvar *op-caddr* nil)
(defvar *op-cadddr* nil)
(defvar *op-null* nil)
(defvar *op-consp* nil)
(defvar *op-list* nil)
(defvar *op-not* nil)
(defvar *op-and* nil)
(defvar *op-or* nil)
(defvar *op-defpackage* nil)
(defvar *op-in-package* nil)
(defvar *op-case* nil)
(defvar *op-when* nil)
(defvar *op-unless* nil)
(defvar *op-declaim* nil)
(defvar *op-setq* nil)
(defvar *op-error* nil)
;; Additional operators that were using string comparison
(defvar *op-symbolp* nil)
(defvar *op-numberp* nil)
(defvar *op-stringp* nil)
(defvar *op-keywordp* nil)
(defvar *op-string-length* nil)
(defvar *op-string-ref* nil)
(defvar *op-char-at* nil)
(defvar *op-string=* nil)
(defvar *op-string-equal* nil)
(defvar *op-sym-eq* nil)
(defvar *op-member* nil)
(defvar *op-symbol-name* nil)
(defvar *op-keyword-name* nil)
(defvar *op-logand* nil)
(defvar *op-logior* nil)
(defvar *op-ash* nil)
(defvar *op-eq* nil)
(defvar *op-eql* nil)
(defvar *op-get-tag* nil)
(defvar *op-set-tag* nil)
(defvar *op-length* nil)
(defvar *op-make-vector* nil)
(defvar *op-vector-length* nil)
(defvar *op-vector-set* nil)
(defvar *op-vector-ref* nil)
(defvar *op-reverse* nil)
(defvar *op-make-string-from-vector* nil)
(defvar *op-make-symbol-from-string* nil)
(defvar *op-caar* nil)
(defvar *op-cdar* nil)
(defvar *op-nth* nil)
(defvar *op-lognot* nil)
(defvar *op-neq* nil)
(defvar *op-lambda* nil)
(defvar *op-funcall* nil)
(defvar *op-setcar* nil)
(defvar *op-setcdr* nil)
(defvar *op-dolist* nil)
(defvar *op-flet* nil)
(defvar *op-labels* nil)
(defvar *op-mapcar* nil)
(defvar *op-ecase* nil)
(defvar *op-listp* nil)
(defvar *op-nil* nil)
(defvar *op-otherwise* nil)

;;; Package system globals
;;; Packages are ((name . symbols) ...) where symbols is ((name . sym) ...)
(defvar *packages* nil)
(defvar *current-package* nil)  ; string name of current package, nil = CL-USER

;;; Global variable environment for h0-eval
;;; Alist of (symbol . value) pairs for DEFVAR/SETQ globals
(defvar *h0-globals* nil)

;;; Register keyword dispatch table - maps keyword -> register number
;;; Populated by init-register-table, used by habu0-reg for O(1) lookup
(defvar *register-table* nil)

;;; Operator dispatch table - maps habu symbol -> dispatch ID (integer)
;;; Single table replaces 88 *op-* variables. Built by init-compile-ops.
;;; Dispatch IDs are assigned by define-ops macro for efficient match dispatch.
(defvar *op-dispatch-table* nil)

;;; Special keywords for other purposes (offset, imm)
;;; These are interned at startup for eq comparison
(defvar *kw-offset* nil)
(defvar *kw-imm* nil)

;;; ==========================================================
;;; Error Infrastructure - crash with message, no silent fallbacks
;;; ==========================================================

;;; Stack trace support for error diagnostics
;;; ARM64: x29 = frame pointer, [fp] = prev fp, [fp+8] = return addr

;; Convert a nibble (0-15) to hex character
(defun nibble-to-hex (n)
  (if (< n 10)
      (+ n 48)    ; '0' = 48
      (+ n 87)))  ; 'a' = 97, so a=10 -> 97-10=87

;; Print a 64-bit address in hex to stderr (16 hex digits)
;; addr is a raw pointer value
(defun print-hex (addr)
  (let ((buf (make-vector 18)))  ; "0x" + 16 hex digits
    (vector-set buf 0 48)   ; '0'
    (vector-set buf 1 120)  ; 'x'
    ;; Fill in hex digits from MSB to LSB
    (print-hex-loop addr buf 15)
    (sys-write 2 (make-string-from-vector buf) 18)))

(defun print-hex-loop (addr buf idx)
  (if (< idx 0)
      nil
      (let ((nibble (logand addr 15)))
        (vector-set buf (+ idx 2) (nibble-to-hex nibble))
        (print-hex-loop (ash addr -4) buf (- idx 1)))))

;;; Symbol table access for runtime symbolication
;;; The embedded symbol table is at (get-code-base) + (get-symtab-offset)
;;; Format: u64 count, then entries: (u64 offset, u64 name_len, name bytes padded to 8)

;; Get the address of the embedded symbol table
(defun get-symtab-addr ()
  (+ (get-code-base) (get-symtab-offset)))

;; Read a u64 from memory at addr
(defun read-u64 (addr)
  (mem-load-64 addr 0))

;; Read symbol table entry: returns (offset . name_len) at entry index
;; Entry format: offset(8) + name_len(8) + name(padded to 8)
;; Returns nil if index >= count
(defun symtab-entry-offset-at (symtab-addr idx)
  (let* ((count (read-u64 symtab-addr)))
    (if (>= idx count)
        nil
        (symtab-scan-to-entry symtab-addr (+ symtab-addr 8) idx))))

;; Scan to entry idx, accumulating offset as we go
;; ptr points to current entry, returns (entry-offset . name_len)
(defun symtab-scan-to-entry (base ptr idx)
  (let* ((entry-offset (read-u64 ptr))
         (name-len (read-u64 (+ ptr 8)))
         (padded-len (* (/ (+ name-len 8) 8) 8)))  ; round up to 8
    (if (= idx 0)
        (cons entry-offset name-len)
        (symtab-scan-to-entry base (+ ptr 16 padded-len) (- idx 1)))))

;; Get entry data at index: returns (offset name-len name-addr) or nil
(defun symtab-get-entry (symtab-addr idx)
  (let* ((count (read-u64 symtab-addr)))
    (if (>= idx count)
        nil
        (symtab-get-entry-scan (+ symtab-addr 8) idx))))

(defun symtab-get-entry-scan (ptr idx)
  (let* ((entry-offset (read-u64 ptr))
         (name-len (read-u64 (+ ptr 8)))
         (name-addr (+ ptr 16))
         (padded-len (* (/ (+ name-len 8) 8) 8)))
    (if (= idx 0)
        (list entry-offset name-len name-addr)
        (symtab-get-entry-scan (+ ptr 16 padded-len) (- idx 1)))))

;; Find the symbol containing addr using linear search
;; Returns (offset name-len name-addr) or nil if not found
;; addr is absolute, will be converted to relative offset from code base
(defun lookup-symbol (addr)
  (let* ((symtab-addr (get-symtab-addr))
         (count (read-u64 symtab-addr))
         (code-base (get-code-base))
         ;; Convert absolute addr to offset from code base (user code start)
         (rel-addr (- addr code-base)))
    ;; Iterative scan through entries
    (lookup-symbol-scan (+ symtab-addr 8) count rel-addr)))

;; Iterative scan through symbol table entries
;; Uses while loop for zero stack growth - handles any table size
;; Symbol table is sorted by offset, so we can stop early when we pass target
(defun lookup-symbol-scan (initial-ptr count target-offset)
  (let ((ptr initial-ptr)
        (remaining count)
        (best nil)
        (done nil))
    (while (and (> remaining 0) (not done))
      (let* ((entry-offset (read-u64 ptr))
             (name-len (read-u64 (+ ptr 8)))
             (name-addr (+ ptr 16))
             (padded-len (* (/ (+ name-len 8) 8) 8)))
        ;; Early termination: if entry > target and we have best, stop
        (if (and (> entry-offset target-offset) best)
            (setq done t)
            (progn
              ;; Check if this entry is a better match
              (if (and (<= entry-offset target-offset)
                       (or (null best)
                           (> entry-offset (car best))))
                  (setq best (list entry-offset name-len name-addr)))
              ;; Move to next entry
              (setq ptr (+ ptr 16 padded-len))
              (setq remaining (- remaining 1))))))
    best))

;; Print a symbol name from addr for len bytes (iterative)
;; Uses while loop for zero stack growth
;; Pattern: fill vector with bytes, convert to string, write string
(defun print-symbol-name (addr len)
  (let ((buf (make-vector 64))
        (idx 0)
        (remaining len))
    ;; Copy bytes to buffer (up to 63 chars)
    ;; mem-load-byte returns raw byte, vector-set stores it (compiler will tag)
    (while (and (> remaining 0) (< idx 63))
      (let ((byte (mem-load-byte addr idx)))
        (vector-set buf idx byte)
        (setq idx (+ idx 1))
        (setq remaining (- remaining 1))))
    ;; Convert vector to string and write (make-string-from-vector handles untagging)
    (sys-write 2 (make-string-from-vector buf) idx)))

;; Print symbolicated address: "0xADDR FUNC+0xOFF" or just hex if not found
(defun print-symbolicated-addr (addr)
  ;; First print the hex address
  (print-hex addr)
  ;; Try to add symbol info (function name and offset)
  (let ((sym (lookup-symbol addr)))
    (if (null sym)
        nil  ; no symbol found, just show hex
        ;; Found symbol: print " FUNC+0xOFFSET"
        (let* ((sym-offset (car sym))
               (name-len (cadr sym))
               (name-addr (caddr sym))
               (code-base (get-code-base))
               (rel-addr (- addr code-base))
               (offset-in-func (- rel-addr sym-offset)))
          (sys-write 2 " " 1)
          (print-symbol-name name-addr name-len)
          (sys-write 2 "+" 1)
          (print-hex offset-in-func)))))

;; Walk the stack and print symbolicated addresses
;; fp is the current frame pointer (from get-frame-pointer)
;; max-depth limits how many frames to print
(defun print-stack-trace (fp max-depth)
  (sys-write 2 "Stack trace:\n" 13)
  (print-stack-frames fp max-depth 0))

(defun print-stack-frames (fp depth count)
  ;; Standard ARM64 layout: [fp+0]=saved fp, [fp+8]=return address
  ;; (fn-fixed-prologue sets fp = sp + 0x3FF0 to match this)
  (if (or (= fp 0) (>= count depth))
      nil
      (let ((ret-addr (mem-load-64 fp 8))
            (prev-fp (mem-load-64 fp 0)))
        (sys-write 2 "  " 2)
        (print-symbolicated-addr ret-addr)
        (sys-write 2 "\n" 1)
        (print-stack-frames prev-fp depth (+ count 1)))))

;; Print error message with stack trace and exit
;; msg: error message string
(defun fatal-error (msg)
  (sys-write 2 "\n=== FATAL ERROR ===\n" 21)
  (sys-write 2 msg (string-length msg))
  (sys-write 2 "\n" 1)
  (print-stack-trace (get-frame-pointer) 10)
  (sys-exit 1))

;; Generate IR that will crash with error message and stack trace at runtime
;; Use this in h0-compile when encountering unhandled cases
(defun fatal-error-ir (msg)
  (list 'call-ir 'fatal-error (list (list 'str-lit msg))))

;;; ==========================================================
;;; Hash Table Implementation - O(1) amortized lookup
;;; ==========================================================
;;;
;;; Structure: vector of N buckets, each bucket is an alist
;;; Hash function: DJB2 (fast, good distribution)
;;; Collision handling: chaining (alist per bucket)

;; Hash table size - use literal value to avoid global variable lookup issues
(defconstant +hash-table-size+ 256)  ; Number of buckets (power of 2 for fast modulo)

;; DJB2 hash function for strings
;; Returns hash value as fixnum - iterative to avoid stack overflow
(defun string-hash (s)
  (if (null s)
      (error "string-hash: received nil")
      (let ((len (string-length s))
            (i 0)
            (hash 5381))
        (while (< i len)
          (let ((c (string-ref s i)))
            ;; hash = hash * 33 + c = (hash << 5) + hash + c
            (setq hash (+ (ash hash 5) hash c)))
          (setq i (+ i 1)))
        hash)))

;; Create empty hash table (vector of nil buckets)
;; Note: make-vector doesn't initialize elements, so we must fill with nil
(defun make-string-hash-table ()
  (let ((table (make-vector +hash-table-size+)))
    (fill-vector-nil table 0 +hash-table-size+)
    table))

;; Fill vector elements with nil from index start to end - iterative
(defun fill-vector-nil (vec start end)
  (let ((i start))
    (while (< i end)
      (vector-set vec i nil)
      (setq i (+ i 1)))))

;; Get bucket index for string key
(defun hash-bucket-index (key)
  (logand (string-hash key) (- +hash-table-size+ 1)))

;; Look up key in hash table, returns value or nil
;; Uses string= for key comparison (case-sensitive, already uppercased)
(defun hash-table-get (table key)
  (let* ((idx (hash-bucket-index key))
         (bucket (vector-ref table idx)))
    (hash-bucket-find key bucket)))

(defun hash-bucket-find (key bucket)
  ;; Check for empty bucket: nil (= 0 in hybrid tagging)
  ;; Note: (= bucket 0) would check for fixnum 0 (tagged as 1), not nil
  (if (null bucket)
      nil
      (let ((entry (car bucket)))
        (if (string= (car entry) key)
            (cdr entry)
            (hash-bucket-find key (cdr bucket))))))

;; Insert or update key-value pair in hash table
;; Returns the value
(defun hash-table-set (table key value)
  (let* ((idx (hash-bucket-index key))
         (bucket (vector-ref table idx))
         (existing (hash-bucket-find key bucket)))
    (if existing
        value  ; Already exists (shouldn't happen for intern tables)
        (progn
          (vector-set table idx (cons (cons key value) bucket))
          value))))

;;; ==========================================================
;;; Symbol interning - uses hash table for O(1) lookup
;;; ==========================================================
;;;
;;; The intern table stored at [x27+0] is now a hash table (vector)
;;; Each bucket is an alist of (name-string . symbol) pairs

;; Ensure intern table is a hash table (lazy initialization)
;; If table is nil, create new hash table and store it
(defun ensure-intern-hash-table ()
  (let ((table (get-intern-table)))
    (if (null table)
        (let ((new-table (make-string-hash-table)))
          (set-intern-table new-table)
          new-table)
        table)))

;; Search intern table (hash table) for name
;; Returns the symbol if found, nil otherwise
(defun find-interned (name table)
  (if (null table)
      nil
      (hash-table-get table name)))

;; Add symbol to intern table (hash table)
(defun intern-table-add (name sym)
  (let ((table (ensure-intern-hash-table)))
    (hash-table-set table name sym)
    sym))

;; NOTE: get-intern-table and set-intern-table are compiler primitives
;; They generate code to load/store from [x27+0] (intern table at GC globals base)
;; Do NOT define them here - they're built-in primitives handled by the compiler

;; Intern a string as a symbol
;; Handles pkg:sym syntax for package-qualified symbols
;; Returns existing symbol if found, else creates new and adds it
;; This ensures all symbols with the same name are eq
;; IMPORTANT: Always check global table FIRST - this is where register-symbol
;; puts SBCL symbols for mode 1024 cross-symbol-table compatibility.
(defun intern (name)
  (let* ((uname (string-upcase name))
         (parsed (parse-symbol-name uname))
         (pkg-name (car parsed))
         (sym-name (cdr parsed)))
    ;; First check global table - registered SBCL symbols live here
    (let ((global-existing (find-interned (if pkg-name sym-name uname) (get-intern-table))))
      (if global-existing
          global-existing
          ;; Not in global table, proceed with package interning
          (if pkg-name
              ;; Explicit package prefix: intern in that package
              (intern-in-package sym-name pkg-name)
              ;; No package prefix: use current package or create global
              (if *current-package*
                  (intern-in-package sym-name *current-package*)
                  ;; Default: add to global intern table (now a hash table)
                  (let ((sym (make-symbol-from-string uname)))
                    (intern-table-add uname sym))))))))

;;; Tag manipulation primitives
;;; HYBRID 1+3 BIT TAGGING (16-byte aligned objects):
;;;   bit0=1: fixnum (63-bit signed, val >> 1)
;;;   bit0=0: pointer | tag, nil = 0
;;;     0=cons, 2=symbol, 4=vector, 6=string, 8=closure, 10=keyword, 14=forward
;;;
;;; NOTE: get-tag and set-tag are COMPILER PRIMITIVES - they are recognized
;;; by both the bootstrap compiler and h0-compile as special forms that
;;; generate inline ARM64 code. Do NOT define them as functions here.

;;; Keyword interning - keywords use tag 10, stored in separate keyword table
;;; Keywords are self-evaluating symbols in the KEYWORD package
;;; Keywords have same memory layout as STRINGS: [length:8][chars:N]
;;; (NOT symbols - symbols have a pointer to name string at offset 0)

;; Get keyword name - keywords have STRING layout, just different tag
;; Layout: [length:8][chars:N] - same as strings, NOT symbols
;; keyword(10) → string(6): XOR with 12 toggles between them efficiently
(defun keyword-name (kw)
  ;; XOR tag bits: 10 XOR 12 = 6 (keyword → string)
  (logxor kw 12))

;; Make keyword from string - allocate string-like, apply keyword tag
;; make-symbol-from-string allocates [length:8][chars:N] with tag 2
;; We need: change tag 2 to tag 10 → add 8 (= 10 - 2)
;; Note: Can't use logxor because that operates on tagged fixnums,
;; not raw pointer bits. Simple addition works because we're just
;; changing the low tag bits.
(defun make-keyword-from-string (name)
  (let ((sym (make-symbol-from-string name)))
    ;; sym has tag 2, we want tag 10
    ;; Use set-tag to change low 4 bits to keyword tag (10)
    (set-tag sym 10)))

;; Keyword table primitives
;; NOTE: get-keyword-table and set-keyword-table are compiler primitives
;; that access the separate keyword table at [x27+128], distinct from
;; the intern (symbol) table at [x27+0]. This separation ensures that
;; keywords like :X0 remain distinct from symbols like X0.
;; The keyword table is now a hash table (vector) for O(1) lookup.

;; Ensure keyword table is a hash table (lazy initialization)
(defun ensure-keyword-hash-table ()
  (let ((table (get-keyword-table)))
    (if (null table)
        (let ((new-table (make-string-hash-table)))
          (set-keyword-table new-table)
          new-table)
        table)))

;; Search keyword table (hash table) for name
;; Returns the keyword if found, nil otherwise
(defun find-keyword (name table)
  "Look up keyword by name in table. Uses hash table for O(1) lookup."
  (if (null table)
      nil
      (hash-table-get table name)))

;; Add keyword to keyword table (hash table)
(defun keyword-table-add (name kw)
  (let ((table (ensure-keyword-hash-table)))
    (hash-table-set table name kw)
    kw))

;; Intern a keyword by name (without the leading colon)
;; Keywords are like symbols but with tag 10 instead of tag 2
(defun intern-keyword (name)
  (let* ((uname (string-upcase name))
         (existing (find-keyword uname (get-keyword-table))))
    (if existing
        existing
        (let ((kw (make-keyword-from-string uname)))
          (keyword-table-add uname kw)
          kw))))

;; String upcase helper - converts lowercase to uppercase
(defun string-upcase (s)
  "Convert string to uppercase. Uses iterative loop to avoid stack overflow."
  (if (null s)
      (error "string-upcase: received nil")
      (let* ((len (string-length s))
             (vec (make-vector len))
             (i 0))
        (while (< i len)
          (vector-set vec i (h0-char-upcase (string-ref s i)))
          (setq i (+ i 1)))
        (make-string-from-vector vec))))

;;; ============================================================
;;; String utilities for package system
;;; ============================================================

;; Find position of first colon in string, or nil if none
;; Uses iterative loop to avoid stack overflow during startup
(defun find-colon (str)
  (let ((len (string-length str))
        (i 0)
        (result nil)
        (done nil))
    (while (and (not done) (< i len))
      (if (= (string-ref str i) #x3A)  ; colon
          (progn (setq result i) (setq done t))
          (setq i (+ i 1))))
    result))

;; Extract substring from start to end (exclusive)
;; Uses iterative loop to avoid stack overflow
(defun substring (str start end)
  (let* ((len (- end start))
         (vec (make-vector len))
         (i 0))
    (while (< i len)
      (vector-set vec i (string-ref str (+ start i)))
      (setq i (+ i 1)))
    (make-string-from-vector vec)))

;; Concatenate two strings
(defun string-concat (s1 s2)
  (let* ((len1 (string-length s1))
         (len2 (string-length s2))
         (vec (make-vector (+ len1 len2))))
    (string-copy-to-vec s1 vec 0 len1)
    (string-copy-to-vec s2 vec len1 len2)
    (make-string-from-vector vec)))

;; Copy string to vector at offset - iterative
(defun string-copy-to-vec (src dst start len)
  (let ((i 0))
    (while (< i len)
      (vector-set dst (+ start i) (string-ref src i))
      (setq i (+ i 1)))
    dst))

;; Concatenate three strings
(defun string-concat3 (s1 s2 s3)
  (string-concat (string-concat s1 s2) s3))

;; Create a list of n nil values - iterative
(defun make-list (n)
  (let ((acc nil)
        (i n))
    (while (> i 0)
      (setq acc (cons nil acc))
      (setq i (- i 1)))
    acc))

;; Standard list utility functions (needed by register allocator)
(defun equal (a b)
  (cond
    ((eq a b) t)
    ((and (consp a) (consp b))
     (and (equal (car a) (car b))
          (equal (cdr a) (cdr b))))
    ((and (stringp a) (stringp b))
     (string= a b))
    (t nil)))

(defun member (item lst)
  (if (null lst)
      nil
      (if (eq item (car lst))
          lst
          (member item (cdr lst)))))

(defun assoc (key alist)
  (if (null alist)
      nil
      (if (eq key (caar alist))
          (car alist)
          (assoc key (cdr alist)))))

(defun copy-list (lst)
  (if (null lst)
      nil
      (cons (car lst) (copy-list (cdr lst)))))

;;; Type predicates for h0-eval-dispatch
;;; In SBCL-compiled mode, values are CL objects (cons, symbol, integer, etc.)
;;; These predicates wrap CL predicates for use in h0-eval-dispatch.

(defun h0-consp (x)
  ;; In SBCL mode, cons cells are CL cons cells
  (cl:consp x))

(defun h0-symbolp (x)
  ;; In SBCL mode, symbols are CL symbols
  (cl:symbolp x))

(defun h0-numberp (x)
  ;; In SBCL mode, numbers are CL integers
  (cl:numberp x))

(defun h0-stringp (x)
  ;; In SBCL mode, strings are CL strings
  (cl:stringp x))

(defun h0-vectorp (x)
  ;; In SBCL mode, vectors are CL vectors (but not strings)
  (and (cl:vectorp x) (not (cl:stringp x))))

(defun h0-keywordp (x)
  ;; In SBCL mode, keywords are CL keywords
  (cl:keywordp x))

(defun h0-listp (x)
  (or (null x) (h0-consp x)))

;; Compound car/cdr accessors (needed by register allocator and codegen)
(defun cadar (x) (car (cdr (car x))))
(defun cddr (x) (cdr (cdr x)))
(defun cdddr (x) (cdr (cdr (cdr x))))
(defun cddddr (x) (cdr (cdr (cdr (cdr x)))))
(defun caddr (x) (car (cdr (cdr x))))
(defun cadddr (x) (car (cdr (cdr (cdr x)))))

;; subseq - works on strings (for register allocator compatibility)
(defun subseq (s start &optional end)
  (substring s start (if end end (string-length s))))

;;; ============================================================
;;; Package system
;;; ============================================================

;; Find package by name string, returns (name . symbols) or nil
(defun find-package (name)
  (find-package-in *packages* name))

(defun find-package-in (pkgs name)
  (if (null pkgs)
      nil
      (if (string= (caar pkgs) name)
          (car pkgs)
          (find-package-in (cdr pkgs) name))))

;; Create new package if doesn't exist, returns package
(defun make-package (name)
  (let ((existing (find-package name)))
    (if existing
        existing
        (let ((pkg (cons name nil)))
          (setq *packages* (cons pkg *packages*))
          pkg))))

;; Get symbols from package
(defun package-symbols (pkg)
  (cdr pkg))

;; Add symbol to package
(defun package-add-symbol (pkg sym-name sym)
  (let ((new-symbols (cons (cons sym-name sym) (cdr pkg))))
    ;; Note: setcdr not available, rebuild package
    ;; Actually in habu0 we can use set-cdr if it exists
    ;; For now, update *packages* by rebuilding
    (setq *packages* (update-package-symbols *packages* (car pkg) new-symbols))))

(defun update-package-symbols (pkgs pkg-name new-symbols)
  (if (null pkgs)
      nil
      (if (string= (caar pkgs) pkg-name)
          (cons (cons pkg-name new-symbols) (cdr pkgs))
          (cons (car pkgs) (update-package-symbols (cdr pkgs) pkg-name new-symbols)))))

;; Find symbol in package's symbol list
(defun find-symbol-in-package (sym-name pkg)
  (find-in-alist sym-name (package-symbols pkg)))

(defun find-in-alist (key alist)
  (if (null alist)
      nil
      (if (string-equal (caar alist) key)
          (car alist)
          (find-in-alist key (cdr alist)))))

;; Parse symbol name: "PKG:SYM" -> (pkg-name . sym-name), "SYM" -> (nil . sym-name)
(defun parse-symbol-name (name)
  (let ((colon-pos (find-colon name)))
    (if colon-pos
        (cons (substring name 0 colon-pos)
              (substring name (+ colon-pos 1) (string-length name)))
        (cons nil name))))

;; Intern symbol in specific package
(defun intern-in-package (sym-name pkg-name)
  (let ((pkg (find-package pkg-name)))
    (if (null pkg)
        (setq pkg (make-package pkg-name)))
    ;; Look for existing symbol in package
    (let ((existing (find-symbol-in-package sym-name pkg)))
      (if existing
          (cdr existing)
          ;; Create new symbol with just the bare name (not qualified)
          ;; Package membership is tracked by the package, not the symbol name
          (let ((sym (make-symbol-from-string sym-name)))
            (package-add-symbol pkg sym-name sym)
            sym)))))

;; File I/O constants
(defun o-rdonly () #x0)

;; Convert a buffer (vector) to a string of given length
;; sys-read writes raw bytes to the buffer, so we must use buffer-byte-ref
;; to read individual bytes, not vector-ref (which reads 8-byte slots)
(defun buffer-to-string (buf len)
  (let ((vec (make-vector len)))
    (buffer-to-string-copy buf vec 0 len)
    (make-string-from-vector vec)))

(defun buffer-to-string-copy (src dst i len)
  (if (>= i len)
      nil
      (progn
        ;; Use buffer-byte-ref to read raw bytes from sys-read buffer
        ;; and vector-set to store them in the destination vector
        (vector-set dst i (buffer-byte-ref src i))
        (buffer-to-string-copy src dst (+ i 1) len))))

;; Read entire file into string
(defun native-read-file (path)
  (let* ((fd (sys-open path (o-rdonly) #x0)))
    (if (< fd #x0)
        nil
        (let* ((buf-size #x10000)
               (buf (make-vector buf-size))
               (bytes-read (sys-read fd buf buf-size)))
          (sys-close fd)
          (if (< bytes-read #x0)
              nil
              (buffer-to-string buf bytes-read))))))

;;; Character predicates
(defun whitespace? (ch)
  (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))

(defun digit? (ch)
  (and (>= ch #x30) (<= ch #x39)))

(defun hex-digit? (ch)
  (or (digit? ch)
      (and (>= ch #x41) (<= ch #x46))
      (and (>= ch #x61) (<= ch #x66))))

(defun alpha? (ch)
  (or (and (>= ch #x41) (<= ch #x5A))
      (and (>= ch #x61) (<= ch #x7A))))

(defun symbol-char? (ch)
  (or (alpha? ch) (digit? ch)
      (= ch #x2D) (= ch #x5F) (= ch #x2B) (= ch #x2A)
      (= ch #x2F) (= ch #x3D) (= ch #x3C) (= ch #x3E)
      (= ch #x21) (= ch #x3F) (= ch #x26) (= ch #x25) (= ch #x3A)))

(defun char-at (source pos)
  (if (< pos (string-length source))
      (string-ref source pos)
      #x0))

(defun digit-val (ch) (- ch #x30))

(defun hex-val (ch)
  (cond ((digit? ch) (- ch #x30))
        ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) #xA))
        ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) #xA))
        (t (fatal-error "hex-val: invalid hex character"))))

;; Convert lowercase letter to uppercase (a-z -> A-Z)
(defun h0-char-upcase (ch)
  (if (and (>= ch #x61) (<= ch #x7A))
      (- ch #x20)
      ch))

(defun skip-line (source pos)
  (let ((ch (char-at source pos)))
    (if (or (= ch #x0A) (= ch #x0))
        (+ pos #x1)
        (skip-line source (+ pos #x1)))))

(defun skip-ws (source pos)
  (let ((ch (char-at source pos)))
    (cond ((whitespace? ch) (skip-ws source (+ pos #x1)))
          ((= ch #x3B) (skip-ws source (skip-line source (+ pos #x1))))
          (t pos))))

(defun read-digits (source pos n)
  (let ((ch (char-at source pos)))
    (if (digit? ch)
        (read-digits source (+ pos #x1) (+ (* n #xA) (digit-val ch)))
        (cons n pos))))

(defun read-int (source pos)
  (let ((neg nil) (start pos))
    (let ((ch (char-at source pos)))
      (cond ((= ch #x2D) (setq neg t) (setq start (+ pos #x1)))
            ((= ch #x2B) (setq start (+ pos #x1)))))
    (let* ((r (read-digits source start #x0))
           (val (car r))
           (end (cdr r)))
      (cons (if neg (- #x0 val) val) end))))

(defun read-hex-digits (source pos n)
  (let ((ch (char-at source pos)))
    (if (hex-digit? ch)
        (read-hex-digits source (+ pos #x1) (+ (* n #x10) (hex-val ch)))
        (cons n pos))))

(defun read-hex (source pos)
  (read-hex-digits source pos #x0))

;; Skip symbol characters - iterative
(defun skip-symbol (source pos)
  (let ((p pos))
    (while (symbol-char? (char-at source p))
      (setq p (+ p 1)))
    p))

;; String equality check - iterative
(defun string= (s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (let ((i 0)
              (result t))
          (while (and result (< i len1))
            (if (= (string-ref s1 i) (string-ref s2 i))
                (setq i (+ i 1))
                (setq result nil)))
          result)
        nil)))

;; Case-insensitive string comparison - iterative
(defun string-equal-iter (s1 s2 len)
  (let ((i 0)
        (result t))
    (while (and result (< i len))
      (if (= (h0-char-upcase (string-ref s1 i))
             (h0-char-upcase (string-ref s2 i)))
          (setq i (+ i 1))
          (setq result nil)))
    result))

(defun string-equal (s1 s2)
  ;; Type-safe string comparison with nil handling.
  ;; Returns nil if either argument is nil, t if both are equal strings.
  (if (or (null s1) (null s2))
      (and (null s1) (null s2))  ; nil = nil
      (string= s1 s2)))

;; Symbol equality - uses eq on properly interned symbols
;; All operator symbols are registered in habu's intern table at startup.
;; habu-read looks up symbols and returns the registered object.
;; This enables O(1) eq comparison instead of O(n) string comparison.
(defun sym-eq (s1 s2)
  (eq s1 s2))
(defun op=quote (sym) (sym-eq sym *op-quote*))
(defun op=if (sym) (sym-eq sym *op-if*))
(defun op=let (sym) (sym-eq sym *op-let*))
(defun op=defun (sym) (sym-eq sym *op-defun*))
(defun op=defvar (sym) (sym-eq sym *op-defvar*))
(defun op=while (sym) (sym-eq sym *op-while*))
(defun op=t (sym) (sym-eq sym *op-t*))
(defun op=plus (sym) (sym-eq sym *op-plus*))
(defun op=minus (sym) (sym-eq sym *op-minus*))
(defun op=mul (sym) (sym-eq sym *op-mul*))
(defun op=div (sym) (sym-eq sym *op-div*))
(defun op=eq-num (sym) (sym-eq sym *op-eq-num*))
(defun op=lt (sym) (sym-eq sym *op-lt*))
(defun op=gt (sym) (sym-eq sym *op-gt*))
(defun op=le (sym) (sym-eq sym *op-le*))
(defun op=ge (sym) (sym-eq sym *op-ge*))
(defun op=let-star (sym) (sym-eq sym *op-let-star*))
(defun op=progn (sym) (sym-eq sym *op-progn*))
(defun op=cond (sym) (sym-eq sym *op-cond*))
(defun op=mod (sym) (sym-eq sym *op-mod*))
(defun op=cons (sym) (sym-eq sym *op-cons*))
(defun op=car (sym) (sym-eq sym *op-car*))
(defun op=cdr (sym) (sym-eq sym *op-cdr*))
(defun op=cadr (sym) (sym-eq sym *op-cadr*))
(defun op=cddr (sym) (sym-eq sym *op-cddr*))
(defun op=caddr (sym) (sym-eq sym *op-caddr*))
(defun op=cadddr (sym) (sym-eq sym *op-cadddr*))
(defun op=null (sym) (sym-eq sym *op-null*))
(defun op=consp (sym) (sym-eq sym *op-consp*))
(defun op=list (sym) (sym-eq sym *op-list*))
(defun op=not (sym) (sym-eq sym *op-not*))
(defun op=and (sym) (sym-eq sym *op-and*))
(defun op=or (sym) (sym-eq sym *op-or*))
(defun op=defpackage (sym) (sym-eq sym *op-defpackage*))
(defun op=in-package (sym) (sym-eq sym *op-in-package*))
(defun op=case (sym) (sym-eq sym *op-case*))
(defun op=when (sym) (sym-eq sym *op-when*))
(defun op=unless (sym) (sym-eq sym *op-unless*))
(defun op=declaim (sym) (sym-eq sym *op-declaim*))
(defun op=setq (sym) (sym-eq sym *op-setq*))
(defun op=error (sym) (sym-eq sym *op-error*))
;; Additional operators
(defun op=symbolp (sym) (sym-eq sym *op-symbolp*))
(defun op=numberp (sym) (sym-eq sym *op-numberp*))
(defun op=stringp (sym) (sym-eq sym *op-stringp*))
(defun op=keywordp (sym) (sym-eq sym *op-keywordp*))
(defun op=string-length (sym) (sym-eq sym *op-string-length*))
(defun op=string-ref (sym) (sym-eq sym *op-string-ref*))
(defun op=char-at (sym) (sym-eq sym *op-char-at*))
(defun op=string= (sym) (sym-eq sym *op-string=*))
(defun op=string-equal (sym) (sym-eq sym *op-string-equal*))
(defun op=sym-eq (sym) (sym-eq sym *op-sym-eq*))
(defun op=member (sym) (sym-eq sym *op-member*))
(defun op=symbol-name (sym) (sym-eq sym *op-symbol-name*))
(defun op=keyword-name (sym) (sym-eq sym *op-keyword-name*))
(defun op=logand (sym) (sym-eq sym *op-logand*))
(defun op=logior (sym) (sym-eq sym *op-logior*))
(defun op=ash (sym) (sym-eq sym *op-ash*))
(defun op=eq (sym) (sym-eq sym *op-eq*))
(defun op=eql (sym) (sym-eq sym *op-eql*))
(defun op=get-tag (sym) (sym-eq sym *op-get-tag*))
(defun op=set-tag (sym) (sym-eq sym *op-set-tag*))
(defun op=length (sym) (sym-eq sym *op-length*))
(defun op=make-vector (sym) (sym-eq sym *op-make-vector*))
(defun op=vector-length (sym) (sym-eq sym *op-vector-length*))
(defun op=vector-set (sym) (sym-eq sym *op-vector-set*))
(defun op=vector-ref (sym) (sym-eq sym *op-vector-ref*))
(defun op=reverse (sym) (sym-eq sym *op-reverse*))
(defun op=make-string-from-vector (sym) (sym-eq sym *op-make-string-from-vector*))
(defun op=make-symbol-from-string (sym) (sym-eq sym *op-make-symbol-from-string*))
(defun op=caar (sym) (sym-eq sym *op-caar*))
(defun op=cdar (sym) (sym-eq sym *op-cdar*))
(defun op=nth (sym) (sym-eq sym *op-nth*))
(defun op=lognot (sym) (sym-eq sym *op-lognot*))
(defun op=neq (sym) (sym-eq sym *op-neq*))
(defun op=lambda (sym) (sym-eq sym *op-lambda*))
(defun op=funcall (sym) (sym-eq sym *op-funcall*))
(defun op=setcar (sym) (sym-eq sym *op-setcar*))
(defun op=setcdr (sym) (sym-eq sym *op-setcdr*))
(defun op=dolist (sym) (sym-eq sym *op-dolist*))
(defun op=flet (sym) (sym-eq sym *op-flet*))
(defun op=labels (sym) (sym-eq sym *op-labels*))
(defun op=mapcar (sym) (sym-eq sym *op-mapcar*))
(defun op=ecase (sym) (sym-eq sym *op-ecase*))
(defun op=listp (sym) (sym-eq sym *op-listp*))
(defun op=nil (sym) (sym-eq sym *op-nil*))
(defun op=otherwise (sym) (sym-eq sym *op-otherwise*))

;; Generic symbol comparison - uses eq with interned symbol
;; WARNING: This calls intern which uses string= - avoid circular dependency
(defun op= (sym name)
  (eq sym (intern name)))

(defun chars-to-string (chars)
  (let* ((len (length chars))
         (vec (make-vector len)))
    (labels ((fill-vec (cs i)
               (if (null cs)
                   vec
                   (progn
                     (vector-set vec i (h0-char-upcase (car cs)))
                     (fill-vec (cdr cs) (+ i 1))))))
      (make-string-from-vector (fill-vec chars 0)))))

(defun read-sym-chars (source pos acc)
  (let ((ch (char-at source pos)))
    (if (symbol-char? ch)
        (read-sym-chars source (+ pos #x1) (cons (h0-char-upcase ch) acc))
        (cons (reverse acc) pos))))

(defun read-sym (source pos)
  (let* ((r (read-sym-chars source pos nil))
         (chars (car r))
         (end (cdr r)))
    ;; Use intern to ensure all symbols with same name are eq
    (cons (intern (chars-to-string chars)) end)))

;; Read keyword - starts after the ':'
(defun read-keyword (source pos)
  (let* ((r (read-sym-chars source pos nil))
         (chars (car r))
         (end (cdr r)))
    ;; Use intern-keyword to ensure all keywords with same name are eq
    (cons (intern-keyword (chars-to-string chars)) end)))

;; Read string literal
(defun read-str-chars (source pos acc)
  (let ((ch (char-at source pos)))
    (cond ((= ch #x22) (cons (reverse acc) (+ pos 1)))
          ((= ch #x5C)
           (let ((next (char-at source (+ pos 1))))
             (cond ((= next #x6E) (read-str-chars source (+ pos 2) (cons #x0A acc)))
                   ((= next #x74) (read-str-chars source (+ pos 2) (cons #x09 acc)))
                   ((= next #x22) (read-str-chars source (+ pos 2) (cons #x22 acc)))
                   ((= next #x5C) (read-str-chars source (+ pos 2) (cons #x5C acc)))
                   (t (read-str-chars source (+ pos 2) (cons next acc))))))
          ((= ch #x0) (cons (reverse acc) pos))
          (t (read-str-chars source (+ pos 1) (cons ch acc))))))

(defun read-str (source pos)
  (let* ((r (read-str-chars source (+ pos 1) nil))
         (chars (car r))
         (end (cdr r))
         (len (length chars))
         (vec (make-vector len)))
    (labels ((fill-vec (cs i)
               (if (null cs)
                   vec
                   (progn
                     (vector-set vec i (car cs))
                     (fill-vec (cdr cs) (+ i 1))))))
      (cons (make-string-from-vector (fill-vec chars 0)) end))))

;; Feature list for reader conditionals
;; habu0 includes the :habu feature but not :sbcl
(defun has-feature? (feature-name)
  (string= feature-name "HABU"))

;; Read a feature name (can be a symbol or keyword)
;; Returns (name . pos) where name is the uppercased string
(defun read-feature-name (source pos)
  (let* ((p2 (skip-ws source pos))
         (ch (char-at source p2)))
    (if (= ch #x3A)  ; colon - keyword syntax
        (let* ((r (read-sym-chars source (+ p2 #x1) nil))
               (chars (car r))
               (end (cdr r)))
          (cons (chars-to-string chars) end))
        (let* ((r (read-sym-chars source p2 nil))
               (chars (car r))
               (end (cdr r)))
          (cons (chars-to-string chars) end)))))

;; Reader functions as top-level defuns to enable TCO for self-recursive calls.
;; Previously used labels, but labels transforms recursive calls to funcall through FNTAB,
;; which prevents TCO from recognizing self-tail-calls.

(defun hr-read-list-elems (source p)
  "Read list elements until ) or EOF. Returns (elements . new-pos)."
  (let* ((p2 (skip-ws source p))
         (ch (char-at source p2)))
    (cond
      ((= ch #x29) (cons nil (+ p2 #x1)))  ; ) - end of list
      ((= ch #x2E)                          ; . - dotted pair
       (let* ((r (hr-read-one source (+ p2 #x1)))
              (cdr-val (car r))
              (p3 (cdr r))
              (p4 (skip-ws source p3)))
         (cons cdr-val (+ p4 #x1))))
      ((= ch #x0) (cons nil p2))            ; EOF
      (t (let* ((er (hr-read-one source p2))
                (el (car er))
                (p3 (cdr er))
                (rr (hr-read-list-elems source p3))  ; TCO: self-tail-call
                (rest-list (car rr))
                (rest-pos (cdr rr)))
           (cons (cons el rest-list) rest-pos))))))

(defun hr-read-list (source p)
  "Read a list starting at (. Returns (list . new-pos)."
  (hr-read-list-elems source (+ p #x1)))

(defun hr-read-sharp (source p)
  "Read # syntax. Returns (value . new-pos)."
  (let ((ch (char-at source (+ p #x1))))
    (cond
      ((or (= ch #x78) (= ch #x58)) (read-hex source (+ p #x2)))  ; #x or #X
      ((= ch #x27)                                                 ; #'
       (let* ((r (hr-read-one source (+ p #x2)))
              (val (car r))
              (pos (cdr r)))
         (cons (list 'function val) pos)))
      ;; #+ reader conditional - include if feature present
      ((= ch #x2B)
       (let* ((p2 (+ p #x2))
              (feat-result (read-feature-name source p2))
              (feat-name (car feat-result))
              (p3 (cdr feat-result))
              (form-result (hr-read-one source p3))
              (form (car form-result))
              (p4 (cdr form-result)))
         (if (has-feature? feat-name)
             (cons form p4)
             (hr-read-one source p4))))  ; TCO: tail-call to read-one
      ;; #- reader conditional - include if feature NOT present
      ((= ch #x2D)
       (let* ((p2 (+ p #x2))
              (feat-result (read-feature-name source p2))
              (feat-name (car feat-result))
              (p3 (cdr feat-result))
              (form-result (hr-read-one source p3))
              (form (car form-result))
              (p4 (cdr form-result)))
         (if (has-feature? feat-name)
             (hr-read-one source p4)  ; TCO: tail-call to read-one
             (cons form p4))))
      (t (fatal-error "read-sharp: unknown # syntax")))))

(defun hr-read-one (source p)
  "Read one form from source at position p. Returns (form . new-pos)."
  (let* ((p2 (skip-ws source p))
         (ch (char-at source p2)))
    (if (>= p2 (string-length source))
        (cons nil p2)
        (case ch
          (#x28 (hr-read-list source p2))                    ; (
          (#x27 (let* ((r (hr-read-one source (+ p2 #x1)))   ; '
                       (val (car r))
                       (pos (cdr r)))
                  (cons (list *op-quote* val) pos)))
          (#x22 (read-str source p2))                        ; "
          (#x23 (hr-read-sharp source p2))                   ; #
          (#x29 (cons nil (+ p2 #x1)))                       ; )
          (#x2D (if (digit? (char-at source (+ p2 #x1)))     ; -
                    (read-int source p2)
                    (read-sym source p2)))
          (#x2B (if (digit? (char-at source (+ p2 #x1)))     ; +
                    (read-int source p2)
                    (read-sym source p2)))
          (#x3A (read-keyword source (+ p2 #x1)))            ; :
          (otherwise                                          ; default
           (if (digit? ch)
               (read-int source p2)
               (if (symbol-char? ch)
                   (read-sym source p2)
                   (hr-read-one source (+ p2 #x1)))))))))  ; TCO: tail-call

;; Main entry point - just calls the top-level reader function
(defun habu-read (source pos)
  (hr-read-one source pos))

;; Reverse a list - iterative
(defun reverse (lst)
  (let ((l lst)
        (acc nil))
    (while l
      (setq acc (cons (car l) acc))
      (setq l (cdr l)))
    acc))

;; revappend - iterative
(defun revappend (lst tail)
  (let ((l lst)
        (acc tail))
    (while l
      (setq acc (cons (car l) acc))
      (setq l (cdr l)))
    acc))

;; append - iterative using reverse
(defun append (list1 list2)
  (if (null list1)
      list2
      (revappend (reverse list1) list2)))

;; length - count elements in list - iterative
(defun length (lst)
  (let ((l lst)
        (n 0))
    (while l
      (setq n (+ n 1))
      (setq l (cdr l)))
    n))

;; Read all forms from source - iterative
(defun read-all (source)
  (let ((len (string-length source))
        (pos 0)
        (acc nil))
    (while (< pos len)
      (setq pos (skip-ws source pos))
      (if (< pos len)
          (let ((r (habu-read source pos)))
            (setq acc (cons (car r) acc))
            (setq pos (cdr r)))))
    (reverse acc)))

(defun h0-read-from-string (s)
  (car (habu-read s 0)))

;;; Simple expression evaluator with function definitions
;;; This interpreter supports defun, let, and recursion.

;; Look up function by symbol in fenv
;; Uses eq first for O(1) when symbols are from the same intern table,
;; falls back to string comparison when symbols are from different intern tables.
(defun fenv-lookup (sym fenv)
  (let ((result (fenv-lookup-eq sym fenv)))
    (if result
        result
        (fenv-lookup-by-name (symbol-name sym) fenv))))

;; Fast path: eq comparison for same-intern-table symbols
(defun fenv-lookup-eq (sym fenv)
  (if (null fenv) nil
      (let ((entry (car fenv)))
        (if (eq sym (car entry))
            (cdr entry)
            (fenv-lookup-eq sym (cdr fenv))))))

;; Fallback: string comparison for cross-intern-table symbols
(defun fenv-lookup-by-name (name-str fenv)
  (if (null fenv) nil
      (let ((entry (car fenv)))
        (if (c-names-match name-str (symbol-name (car entry)))
            (cdr entry)
            (fenv-lookup-by-name name-str (cdr fenv))))))

;; Create binding list from params and args
;; Flat list format: interleaves symbols and values (sym1 val1 sym2 val2 ...)
;; More efficient than alist for native code generation
(defun bind-args (params args env)
  (if (null params) env
      (bind-args (cdr params) (cdr args)
                 (cons (car params) (cons (car args) env)))))

;;; ==========================================================================
;;; &key Lambda Support
;;; ==========================================================================
;;; Allows functions to accept keyword arguments:
;;;   (defun foo (x y &key opt1 (opt2 default)) ...)
;;;   (foo 1 2 :opt1 val1 :opt2 val2)

;; Check if a symbol is &KEY marker
(defun is-key-marker (sym)
  (if (symbolp sym)
      (string= (symbol-name sym) "&KEY")
      nil))

;; Split lambda list at &key marker
;; Returns (required-params . key-params)
(defun split-lambda-list (params)
  (labels ((split-loop (rest required)
             (cond
               ((null rest) (cons (reverse required) nil))
               ((is-key-marker (car rest))
                (cons (reverse required) (cdr rest)))
               (t (split-loop (cdr rest) (cons (car rest) required))))))
    (split-loop params nil)))

;; Get the parameter name from a key param spec
;; (param default) -> param, or just param -> param
(defun key-param-name (spec)
  (if (consp spec)
      (car spec)
      spec))

;; Get the default value from a key param spec
;; (param default) -> default, or just param -> nil
(defun key-param-default (spec)
  (if (consp spec)
      (cadr spec)
      nil))

;; Convert a keyword to its corresponding parameter symbol
;; :foo -> FOO (intern the name)
(defun keyword-to-param (kw)
  (intern (keyword-name kw)))

;; Convert a symbol to its corresponding keyword
;; FOO -> :FOO (intern-keyword the name)
(defun symbol-to-keyword (sym)
  (intern-keyword (symbol-name sym)))

;; Find value for a keyword in argument list
;; Args: (:foo val1 :bar val2 ...), key-kw: :foo (a keyword)
;; Returns (found . value) or nil if not found
;; Uses eq - key-kw must be from *kw-* variables (runtime-interned)
(defun find-key-arg (args key-kw)
  (cond
    ((null args) nil)
    ((null (cdr args)) nil)  ; keyword without value - error case
    ((eq (car args) key-kw)
     (cons t (cadr args)))   ; Found it
    (t (find-key-arg (cddr args) key-kw))))

;; Bind keyword arguments to environment
;; key-params: list of (name default) or just name
;; key-args: list of :key val :key val ...
;; Uses flat list format (sym val sym val ...)
(defun bind-key-args (key-params key-args env fenv)
  (if (null key-params) env
      (let* ((spec (car key-params))
             (name (key-param-name spec))
             (default (key-param-default spec))
             (key-kw (symbol-to-keyword name))
             (found (find-key-arg key-args key-kw))
             (val (if found
                      (cdr found)
                      (if default
                          (h0-eval default nil fenv)
                          nil))))
        (bind-key-args (cdr key-params)
                       key-args
                       (cons name (cons val env))
                       fenv))))

;; Count required arguments (non-keyword args)
(defun count-required-args (args)
  (cond
    ((null args) 0)
    ((keywordp (car args)) 0)  ; Reached keyword args
    (t (+ 1 (count-required-args (cdr args))))))

;; Get required arguments from arg list
(defun get-required-args (args n)
  (if (= n 0) nil
      (cons (car args) (get-required-args (cdr args) (- n 1)))))

;; Get keyword arguments from arg list (after required args)
(defun get-key-args (args n)
  (if (= n 0) args
      (get-key-args (cdr args) (- n 1))))

;; Full lambda binding: handles both required and keyword params
(defun bind-lambda-args (params args env fenv)
  (let* ((split (split-lambda-list params))
         (req-params (car split))
         (key-params (cdr split)))
    (if (null key-params)
        ;; No keyword params - use simple binding
        (bind-args req-params args env)
        ;; Has keyword params - split args and bind both
        (let* ((n-required (length req-params))
               (req-args (get-required-args args n-required))
               (key-args (get-key-args args n-required))
               (env1 (bind-args req-params req-args env)))
          (bind-key-args key-params key-args env1 fenv)))))

;;; ==========================================================
;;; Mutable Capture Boxing - transform setq on captured vars
;;; ==========================================================
;;; Variables that are both captured by a lambda AND mutated via setq
;;; must be boxed in cons cells so mutations are visible to closures.

;; Check if symbol is in list (using eq)
(defun h0-member-eq (sym lst)
  (if (null lst) nil
      (if (eq sym (car lst)) t
          (h0-member-eq sym (cdr lst)))))

;; Find all variables that are targets of setq in expr
;; bound = list of currently bound variable names
(defun h0-find-setq-targets (expr bound)
  (h0-collect-setq-targets expr bound nil))

;; Find setq targets in a list of expressions
(defun h0-find-setq-targets-list (exprs bound)
  (h0-collect-list-setq exprs bound nil))

(defun h0-collect-setq-targets (e bnd acc)
  (cond
    ((null e) acc)
    ((not (consp e)) acc)
    ((and (symbolp (car e)) (op=quote (car e))) acc)
    ((and (symbolp (car e)) (op=setq (car e)))
     (let ((var (cadr e))
           (val (caddr e)))
       (if (h0-member-eq var bnd)
           (h0-collect-setq-targets val bnd
             (if (h0-member-eq var acc) acc (cons var acc)))
           (h0-collect-setq-targets val bnd acc))))
    ((and (symbolp (car e)) (op=lambda (car e)))
     (let ((params (cadr e))
           (body-forms (cddr e)))  ;; Lambda can have multiple body forms
       (h0-collect-list-setq body-forms (h0-append params bnd) acc)))
    ((and (symbolp (car e)) (or (op=let (car e))
                                 (op=let-star (car e))))
     (let* ((bindings (cadr e))
            (body-forms (cddr e))
            (names (h0-mapcar-car bindings))
            (acc2 (h0-collect-list-setq (h0-mapcar-cadr bindings) bnd acc)))
       (h0-collect-list-setq body-forms (h0-append names bnd) acc2)))
    (t (h0-collect-list-setq e bnd acc))))

(defun h0-collect-list-setq (lst bnd acc)
  (if (null lst) acc
      (h0-collect-list-setq (cdr lst) bnd
        (h0-collect-setq-targets (car lst) bnd acc))))

(defun h0-collect-let*-setq (bindings body bnd acc)
  (if (null bindings)
      (h0-collect-setq-targets body bnd acc)
      (let* ((b (car bindings))
             (nm (car b))
             (vl (cadr b))
             (acc2 (h0-collect-setq-targets vl bnd acc)))
        (h0-collect-let*-setq (cdr bindings) body (cons nm bnd) acc2))))

;; Find all variables captured by lambdas (free in lambda bodies)
(defun h0-find-captured-vars (expr bound)
  (h0-collect-captured expr bound nil))

;; Find captured vars in a list of expressions
(defun h0-find-captured-vars-list (exprs bound)
  (h0-collect-list-captured exprs bound nil))

(defun h0-collect-captured (e bnd acc)
  (cond
    ((null e) acc)
    ((not (consp e)) acc)
    ((and (symbolp (car e)) (op=quote (car e))) acc)
    ((and (symbolp (car e)) (op=lambda (car e)))
     (let* ((params (cadr e))
            (body-forms (cddr e))  ;; Multiple body forms
            (new-bnd (h0-append params bnd))
            (acc2 (h0-collect-list-captured body-forms new-bnd acc))
            (free-vars (h0-find-free-vars-list body-forms params)))
       (h0-add-captured-vars free-vars bnd acc2)))
    ((and (symbolp (car e)) (or (op=let (car e))
                                 (op=let-star (car e))))
     (let* ((bindings (cadr e))
            (body-forms (cddr e))
            (names (h0-mapcar-car bindings))
            (acc2 (h0-collect-list-captured (h0-mapcar-cadr bindings) bnd acc)))
       (h0-collect-list-captured body-forms (h0-append names bnd) acc2)))
    (t (h0-collect-list-captured e bnd acc))))

(defun h0-collect-list-captured (lst bnd acc)
  (if (null lst) acc
      (h0-collect-list-captured (cdr lst) bnd
        (h0-collect-captured (car lst) bnd acc))))

(defun h0-collect-let*-captured (bindings body bnd acc)
  (if (null bindings)
      (h0-collect-captured body bnd acc)
      (let* ((b (car bindings))
             (nm (car b))
             (vl (cadr b))
             (acc2 (h0-collect-captured vl bnd acc)))
        (h0-collect-let*-captured (cdr bindings) body (cons nm bnd) acc2))))

(defun h0-add-captured-vars (vars bnd acc)
  (if (null vars) acc
      (let ((v (car vars)))
        (h0-add-captured-vars (cdr vars) bnd
          (if (and (h0-member-eq v bnd) (not (h0-member-eq v acc)))
              (cons v acc)
              acc)))))

;; Simple free variable finder
(defun h0-find-free-vars-simple (expr bound)
  (h0-collect-free expr bound nil))

;; Find free vars in a list of expressions
(defun h0-find-free-vars-list (exprs bound)
  (h0-collect-list-free exprs bound nil))

(defun h0-collect-free (e bnd acc)
  (cond
    ((null e) acc)
    ((symbolp e)
     (if (and (not (h0-member-eq e bnd))
              (not (h0-member-eq e acc))
              (not (op=t e))
              (not (op=nil e)))
         (cons e acc)
         acc))
    ((not (consp e)) acc)
    ((and (symbolp (car e)) (op=quote (car e))) acc)
    ((and (symbolp (car e)) (op=lambda (car e)))
     (let ((params (cadr e))
           (body-forms (cddr e)))  ;; Multiple body forms
       (h0-collect-list-free body-forms (h0-append params bnd) acc)))
    ((and (symbolp (car e)) (or (op=let (car e))
                                 (op=let-star (car e))))
     (let* ((bindings (cadr e))
            (body-forms (cddr e))
            (names (h0-mapcar-car bindings))
            (acc2 (h0-collect-list-free (h0-mapcar-cadr bindings) bnd acc)))
       (h0-collect-list-free body-forms (h0-append names bnd) acc2)))
    (t (h0-collect-list-free e bnd acc))))

(defun h0-collect-list-free (lst bnd acc)
  (if (null lst) acc
      (h0-collect-list-free (cdr lst) bnd
        (h0-collect-free (car lst) bnd acc))))

(defun h0-collect-let*-free (bindings body bnd acc)
  (if (null bindings)
      (h0-collect-free body bnd acc)
      (let* ((b (car bindings))
             (nm (car b))
             (vl (cadr b))
             (acc2 (h0-collect-free vl bnd acc)))
        (h0-collect-let*-free (cdr bindings) body (cons nm bnd) acc2))))

;; Intersection of two lists
(defun h0-intersection (lst1 lst2)
  (if (null lst1) nil
      (if (h0-member-eq (car lst1) lst2)
          (cons (car lst1) (h0-intersection (cdr lst1) lst2))
          (h0-intersection (cdr lst1) lst2))))

;; Remove elements of lst2 from lst1
(defun h0-remove-if-member (lst1 lst2)
  (if (null lst1) nil
      (if (h0-member-eq (car lst1) lst2)
          (h0-remove-if-member (cdr lst1) lst2)
          (cons (car lst1) (h0-remove-if-member (cdr lst1) lst2)))))

;; Helpers for list operations
(defun h0-append (a b)
  (if (null a) b
      (cons (car a) (h0-append (cdr a) b))))

(defun h0-mapcar-car (lst)
  (if (null lst) nil
      (cons (car (car lst)) (h0-mapcar-car (cdr lst)))))

(defun h0-mapcar-cadr (lst)
  (if (null lst) nil
      (cons (cadr (car lst)) (h0-mapcar-cadr (cdr lst)))))

;;; Box Mutable Captures - main transformation
;;; Transforms expr to box variables that are both captured and mutated:
;;; - Wraps mutable captured vars in (cons val nil) at binding site
;;; - Transforms reads of boxed vars to (car var)
;;; - Transforms (setq var val) to (setcar var val)

(defun h0-box-mutable-captures (expr)
  (h0-box-transform expr nil))

;; Helper to check if symbol has a given name (case-insensitive)
(defun h0-sym-named (sym name)
  "Check if symbol has given name. Uses eq after interning for efficiency."
  (and (symbolp sym)
       (eq sym (intern name))))

(defun h0-box-transform (e boxed)
  (cond
    ((null e) e)
    ((symbolp e)
     ;; If this var is boxed, transform to (car var) - use interned symbol
     (if (h0-member-eq e boxed)
         (list *op-car* e)
         e))
    ((not (consp e)) e)
    ((h0-sym-named (car e) "QUOTE") e)
    ((h0-sym-named (car e) "SETQ")
     (let ((var (cadr e))
           (val (caddr e)))
       (if (h0-member-eq var boxed)
           (list *op-setcar* var (h0-box-transform val boxed))
           (list *op-setq* var (h0-box-transform val boxed)))))
    ((h0-sym-named (car e) "LAMBDA")
     (let* ((params (cadr e))
            (body-forms (cddr e))
            ;; Don't transform params - they shadow boxed vars
            (new-boxed (h0-remove-if-member boxed params))
            ;; Transform each body form and return lambda with all of them
            (transformed-forms (h0-box-transform-list body-forms new-boxed)))
       (cons *op-lambda* (cons params transformed-forms))))
    ((or (h0-sym-named (car e) "LET") (h0-sym-named (car e) "LET*"))
     (if (h0-sym-named (car e) "LET")
         (h0-box-transform-let e boxed)
         (h0-box-transform-let* e boxed)))
    (t (h0-box-transform-list e boxed))))

(defun h0-box-transform-list (lst boxed)
  (if (null lst) nil
      (cons (h0-box-transform (car lst) boxed)
            (h0-box-transform-list (cdr lst) boxed))))

(defun h0-box-transform-let (e boxed)
  (let* ((bindings (cadr e))
         (body-forms (cddr e))
         (names (h0-mapcar-car bindings))
         ;; Find which new bindings need to be boxed - analyze ALL body forms
         (setq-targets (h0-find-setq-targets-list body-forms names))
         (captured (h0-find-captured-vars-list body-forms names))
         (to-box (h0-intersection setq-targets captured))
         ;; Transform binding values and box if needed
         (new-bindings (h0-box-bindings bindings boxed to-box))
         ;; Add new boxed vars to the set
         (new-boxed (h0-append to-box (h0-remove-if-member boxed names)))
         ;; Transform each body form
         (transformed-forms (h0-box-transform-list body-forms new-boxed)))
    (cons *op-let* (cons new-bindings transformed-forms))))

(defun h0-box-bindings (bindings boxed to-box)
  (if (null bindings) nil
      (let* ((b (car bindings))
             (nm (car b))
             (vl (h0-box-transform (cadr b) boxed))
             (new-val (if (h0-member-eq nm to-box)
                          (list *op-cons* vl nil)
                          vl)))
        (cons (list nm new-val)
              (h0-box-bindings (cdr bindings) boxed to-box)))))

(defun h0-box-transform-let* (e boxed)
  (let* ((bindings (cadr e))
         (body-forms (cddr e))
         (names (h0-mapcar-car bindings))
         ;; Find which new bindings need to be boxed - analyze ALL body forms
         (setq-targets (h0-find-setq-targets-list body-forms names))
         (captured (h0-find-captured-vars-list body-forms names))
         (to-box (h0-intersection setq-targets captured)))
    (h0-box-let*-bindings bindings body-forms boxed to-box)))

(defun h0-box-let*-bindings (bindings body-forms boxed to-box)
  (if (null bindings)
      (cons *op-let-star* (cons nil (h0-box-transform-list body-forms boxed)))
      (let* ((b (car bindings))
             (nm (car b))
             (vl (h0-box-transform (cadr b) boxed))
             (is-boxed (h0-member-eq nm to-box))
             (new-val (if is-boxed (list *op-cons* vl nil) vl))
             (new-binding (list nm new-val))
             (new-boxed (if is-boxed
                            (cons nm boxed)
                            (h0-remove-if-member boxed (list nm))))
             (rest (h0-box-let*-bindings (cdr bindings) body-forms new-boxed to-box)))
        (cons *op-let-star* (cons (cons new-binding (cadr rest)) (cddr rest))))))

;; Look up by symbol in environment using sym-eq
;; Flat list format: (sym1 val1 sym2 val2 ...)
;; Returns the value entry (cons sym val) or nil if not found
;; This allows distinguishing "not found" from "found with nil value"
;; Uses sym-eq for name comparison since symbols may be from different intern tables
(defun env-lookup (sym env)
  (if (null env) nil
      (if (sym-eq sym (car env))
          (cons (car env) (cadr env))  ; Return (sym . value) for compatibility
          (env-lookup sym (cddr env)))))

;; Helper for let bindings - uses flat list format (sym val sym val ...)
(defun h0-eval-let (bindings body env fenv)
  (if (null bindings)
      (h0-eval body env fenv)
      (let* ((b (car bindings))
             (var (car b))  ;; Keep as symbol for eq lookup
             (val (h0-eval (cadr b) env fenv)))
        (h0-eval-let (cdr bindings) body (cons var (cons val env)) fenv))))

;; Helper for let with implicit progn body - binds then evaluates body forms
(defun h0-eval-let-body (bindings body-forms env fenv)
  (h0-eval-let-bind bindings body-forms env fenv))

(defun h0-eval-let-bind (bindings body-forms env fenv)
  (if (null bindings)
      ;; All bindings done, evaluate body forms as progn
      (h0-eval-progn body-forms env fenv)
      (let* ((b (car bindings))
             (var (car b))
             (val (h0-eval (cadr b) env fenv)))
        (h0-eval-let-bind (cdr bindings) body-forms (cons var (cons val env)) fenv))))

;; Helper for progn - evaluates forms in sequence, returns last value
(defun h0-eval-progn (forms env fenv)
  (if (null forms)
      nil
      (if (null (cdr forms))
          (h0-eval (car forms) env fenv)
          (progn
            (h0-eval (car forms) env fenv)
            (h0-eval-progn (cdr forms) env fenv)))))

;; Global variable lookup - search *h0-globals* alist by symbol name
(defun h0-global-lookup (sym)
  (h0-global-lookup-in sym *h0-globals*))

(defun h0-global-lookup-in (sym globals)
  (if (null globals)
      nil
      (let ((entry (car globals)))
        (if (string-equal (symbol-name sym) (symbol-name (car entry)))
            entry
            (h0-global-lookup-in sym (cdr globals))))))

;; Set global variable - adds or updates entry in *h0-globals*
(defun h0-global-set (sym val)
  (let ((entry (h0-global-lookup sym)))
    (if entry
        ;; Update existing - we can't mutate, so rebuild
        (setq *h0-globals* (h0-global-update sym val *h0-globals*))
        ;; Add new
        (setq *h0-globals* (cons (cons sym val) *h0-globals*))))
  val)

(defun h0-global-update (sym val globals)
  (if (null globals)
      nil
      (let ((entry (car globals)))
        (if (string-equal (symbol-name sym) (symbol-name (car entry)))
            (cons (cons sym val) (cdr globals))
            (cons entry (h0-global-update sym val (cdr globals)))))))

;; Helper for cond - evaluates clauses until one matches
(defun h0-eval-cond (clauses env fenv)
  (if (null clauses)
      nil
      (let* ((clause (car clauses))
             (test (car clause))
             (body (cdr clause)))
        (if (h0-eval test env fenv)
            (if (null body)
                t
                (h0-eval-progn body env fenv))
            (h0-eval-cond (cdr clauses) env fenv)))))

;; Helper for while - iterative loop
;; (while test body...) - evaluates body while test is non-nil, returns nil
(defun h0-eval-while (test body env fenv)
  (if (h0-eval test env fenv)
      (progn
        (h0-eval-progn body env fenv)
        (h0-eval-while test body env fenv))
      nil))

;; Helper for labels - create recursive local functions
;; bindings is ((name (params...) body) ...)
;; Returns extended fenv where each function can call any other (including itself)
(defun h0-eval-labels-bindings (bindings fenv env)
  ;; First pass: create fenv entries with placeholder bodies
  ;; The fenv format is ((name params . body) ...)
  (let ((new-fenv (h0-eval-labels-build-fenv bindings fenv)))
    ;; All labels functions share this new-fenv (for mutual recursion)
    new-fenv))

(defun h0-eval-labels-build-fenv (bindings fenv)
  (if (null bindings)
      fenv
      (let* ((binding (car bindings))
             (name (car binding))
             (params (cadr binding))
             (body (caddr binding))
             ;; Add function to fenv: (name . (params . body))
             (entry (cons name (cons params body))))
        (h0-eval-labels-build-fenv (cdr bindings) (cons entry fenv)))))

;; Helper for flet - create non-recursive local functions
;; bindings is ((name (params...) body) ...)
;; Returns extended fenv where functions capture outer fenv (no self-recursion)
(defun h0-eval-flet-bindings (bindings fenv env)
  (if (null bindings)
      fenv
      (let* ((binding (car bindings))
             (name (car binding))
             (params (cadr binding))
             (body (caddr binding))
             ;; Add function to fenv: (name . (params . body))
             (entry (cons name (cons params body))))
        (h0-eval-flet-bindings (cdr bindings) (cons entry fenv) env))))

;; Helper for dolist - iterate over list evaluating body for each element
;; env is flat list format: (sym val sym val ...)
(defun h0-eval-dolist (var list-val body env fenv)
  (if (null list-val)
      nil
      (let ((new-env (cons var (cons (car list-val) env))))
        (h0-eval-progn body new-env fenv)
        (h0-eval-dolist var (cdr list-val) body env fenv))))

;; Helper for and - short-circuit evaluation
(defun h0-eval-and (forms env fenv)
  (if (null forms)
      t
      (let ((val (h0-eval (car forms) env fenv)))
        (if val
            (if (null (cdr forms))
                val
                (h0-eval-and (cdr forms) env fenv))
            nil))))

;; Helper for or - short-circuit evaluation
(defun h0-eval-or (forms env fenv)
  (if (null forms)
      nil
      (let ((val (h0-eval (car forms) env fenv)))
        (if val
            val
            (h0-eval-or (cdr forms) env fenv)))))

;; Helper for variadic addition
(defun h0-eval-add (args env fenv)
  (if (null args)
      #x0
      (if (null (cdr args))
          (h0-eval (car args) env fenv)
          (+ (h0-eval (car args) env fenv)
             (h0-eval-add (cdr args) env fenv)))))

;; Helper for variadic subtraction (left-associative)
(defun h0-eval-sub (args env fenv)
  (if (null args)
      #x0
      (if (null (cdr args))
          ;; Unary minus
          (- #x0 (h0-eval (car args) env fenv))
          ;; Binary and more - left-associative
          (h0-eval-sub-left (h0-eval (car args) env fenv) (cdr args) env fenv))))

(defun h0-eval-sub-left (acc args env fenv)
  (if (null args)
      acc
      (h0-eval-sub-left (- acc (h0-eval (car args) env fenv)) (cdr args) env fenv)))

;; EQL - same as EQ for symbols, uses = for numbers
;; This is the standard CL comparison used by CASE
(defun eql (a b)
  (cond
    ((eq a b) t)
    ((and (numberp a) (numberp b)) (= a b))
    (t nil)))

;; Helper for case - check if key matches a clause's keys
;; Keys can be a single value or a list of values
;; Returns t if match, nil otherwise
(defun case-key-matches (key keys)
  ;; Use sym-eq for symbols since habu0 creates new symbol objects at runtime
  ;; that are not eq to symbols from the reader
  (cond
    ((null keys) nil)
    ((consp keys)
     ;; List of keys - check each one
     (if (case-key-eq key (car keys))
         t
         (case-key-matches key (cdr keys))))
    ;; Single key
    (t (case-key-eq key keys))))

(defun case-key-eq (a b)
  ;; If both are symbols, use sym-eq (name comparison)
  ;; Otherwise use eql (works for numbers, etc.)
  (if (if (symbolp a) (symbolp b) nil)
      (sym-eq a b)
      (eql a b)))

;; Helper for case - evaluate clauses
;; Each clause is (keys body...) or (t body...) or (otherwise body...)
(defun h0-eval-case-clauses (key clauses env fenv)
  (if (null clauses)
      nil
      (let* ((clause (car clauses))
             (keys (car clause))
             (body (cdr clause)))
        (cond
          ;; t or otherwise - default case
          ((or (eq keys t)
               (if (symbolp keys)
                   (op=otherwise keys)
                   nil))
           (h0-eval-progn body env fenv))
          ;; Check if key matches
          ((case-key-matches key keys)
           (h0-eval-progn body env fenv))
          ;; Try next clause
          (t (h0-eval-case-clauses key (cdr clauses) env fenv))))))

;; Check if value is a habu keyword (tag 10)
;; Uses keywordp which compiles to tag-10 check in native habu.
;; In SBCL mode, this checks for symbols in the KEYWORD package.
;; In native mode, this checks for tag 10 in the low bits.
(defun h0-keywordp (x)
  (keywordp x))

;; Eval function with fenv for function definitions
;; Uses dispatch table with case for O(1) amortized dispatch (jump table in SBCL)
;; Dispatch IDs: 1-30 special forms, 30-39 self-eval, 101-220 primitives
;; NOTE: Order matters! Keywords must be checked before symbols since
;; SBCL keywords are also symbols (in the KEYWORD package).
(defun h0-eval (expr env fenv)
  (cond
    ;; Self-evaluating types - order matters!
    ((null expr) nil)            ; nil check first (nil is both null and symbol)
    ((h0-keywordp expr) expr)    ; Keywords before symbols (keywords ARE symbols in CL)
    ((numberp expr) expr)
    ((stringp expr) expr)
    ;; Symbols - check for T/NIL first, then variable lookup
    ((symbolp expr)
     (let ((id (op-lookup expr)))
       (cond
         ((eq id 30) t)    ; T
         ((eq id 31) nil)  ; NIL
         (t (h0-eval-symbol expr env fenv)))))
    ;; List - special form or function call
    ((consp expr)
     (let* ((op (car expr))
            (id (if (symbolp op) (op-lookup op) nil)))
       (if id
           (h0-eval-dispatch id expr env fenv)
           ;; Not in dispatch table - function call
           (h0-eval-call op expr env fenv))))
    (t (fatal-error "h0-eval: unknown expression type"))))

;; Symbol lookup helper - local env, global env, or function designator
(defun h0-eval-symbol (sym env fenv)
  (let ((entry (env-lookup sym env)))
    (if entry
        (cdr entry)
        (let ((global-entry (h0-global-lookup sym)))
          (if global-entry
              (cdr global-entry)
              (let ((fn-entry (fenv-lookup sym fenv)))
                (if fn-entry
                    sym  ; Return symbol as function designator
                    (fatal-error "h0-eval: undefined symbol"))))))))

;; Dispatch on operator ID using case
;; IDs: 1-23 special forms, 30-32 self-eval, 101-209 primitives
(defun h0-eval-dispatch (id expr env fenv)
  (case id
    ;; Special forms (1-23)
    (1 (cadr expr))  ; QUOTE
    (2 (if (h0-eval (cadr expr) env fenv)  ; IF
           (h0-eval (caddr expr) env fenv)
           (if (cadddr expr) (h0-eval (cadddr expr) env fenv) nil)))
    (3 (h0-eval-let-body (cadr expr) (cddr expr) env fenv))   ; LET
    (4 (h0-eval-let-body (cadr expr) (cddr expr) env fenv))   ; LET*
    (5 nil)  ; DEFUN - handled at load time
    (6 (let* ((var-sym (cadr expr))  ; DEFVAR
              (init-val (if (cddr expr) (h0-eval (caddr expr) env fenv) nil)))
         (if (null (h0-global-lookup var-sym))
             (h0-global-set var-sym init-val))
         var-sym))
    (7 (h0-eval-while (cadr expr) (cddr expr) env fenv))  ; WHILE
    (8 (h0-eval-progn (cdr expr) env fenv))  ; PROGN
    (9 (h0-eval-cond (cdr expr) env fenv))   ; COND
    (10 (let ((pkg-name (keyword-name (cadr expr))))  ; DEFPACKAGE
          (make-package (string-upcase pkg-name)) nil))
    (11 (let ((pkg-name (keyword-name (cadr expr))))  ; IN-PACKAGE
          (setq *current-package* (string-upcase pkg-name))
          (make-package *current-package*) nil))
    (12 (let ((key (h0-eval (cadr expr) env fenv)))  ; CASE
          (h0-eval-case-clauses key (cddr expr) env fenv)))
    (13 (if (h0-eval (cadr expr) env fenv)  ; WHEN
            (h0-eval-progn (cddr expr) env fenv) nil))
    (14 (if (h0-eval (cadr expr) env fenv) nil  ; UNLESS
            (h0-eval-progn (cddr expr) env fenv)))
    (15 nil)  ; DECLAIM - no-op
    (16 (let* ((var-sym (cadr expr))  ; SETQ
               (val (h0-eval (caddr expr) env fenv))
               (local-cell (env-lookup var-sym env)))
          (if local-cell val
              (let ((global-cell (h0-global-lookup var-sym)))
                (if global-cell (h0-global-set var-sym val)
                    (fatal-error "h0-eval: SETQ unknown variable"))))))
    (17 (fatal-error "h0-eval: error called"))  ; ERROR
    (18 (list (intern "CLOSURE-TAG") (cadr expr) (caddr expr) env))  ; LAMBDA
    (19 (let* ((binding (cadr expr))  ; DOLIST
               (var (car binding))
               (list-val (h0-eval (cadr binding) env fenv)))
          (h0-eval-dolist var list-val (cddr expr) env fenv)))
    (20 (let* ((bindings (cadr expr))  ; LABELS
               (labels-fenv (h0-eval-labels-bindings bindings fenv env)))
          (h0-eval-progn (cddr expr) env labels-fenv)))
    (21 (let* ((bindings (cadr expr))  ; FLET
               (flet-fenv (h0-eval-flet-bindings bindings fenv env)))
          (h0-eval-progn (cddr expr) env flet-fenv)))
    (22 (h0-eval-funcall expr env fenv))  ; FUNCALL
    (23 (let ((key (h0-eval (cadr expr) env fenv)))  ; ECASE
          (h0-eval-case-clauses key (cddr expr) env fenv)))
    ;; Self-evaluating symbols (30-32) - handled in h0-eval for expr case
    (30 t)         ; T
    (31 nil)       ; NIL
    (32 t)         ; OTHERWISE (true in case context)
    ;; Arithmetic (101-105)
    (101 (h0-eval-add (cdr expr) env fenv))  ; +
    (102 (h0-eval-sub (cdr expr) env fenv))  ; -
    (103 (* (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; *
    (104 (/ (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; /
    (105 (mod (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; MOD
    ;; Comparisons (111-116)
    (111 (if (= (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))   ; =
    (112 (if (< (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))   ; <
    (113 (if (> (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))   ; >
    (114 (if (<= (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))  ; <=
    (115 (if (>= (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))  ; >=
    (116 (if (= (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) nil t))   ; /=
    ;; List operations (121-135)
    (121 (cons (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; CONS
    (122 (car (h0-eval (cadr expr) env fenv)))   ; CAR
    (123 (cdr (h0-eval (cadr expr) env fenv)))   ; CDR
    (124 (cadr (h0-eval (cadr expr) env fenv)))  ; CADR
    (125 (cddr (h0-eval (cadr expr) env fenv)))  ; CDDR
    (126 (caddr (h0-eval (cadr expr) env fenv))) ; CADDR
    (127 (cadddr (h0-eval (cadr expr) env fenv))) ; CADDDR
    (128 (h0-eval-list (cdr expr) env fenv))  ; LIST
    (129 (caar (h0-eval (cadr expr) env fenv)))  ; CAAR
    (130 (cdar (h0-eval (cadr expr) env fenv)))  ; CDAR
    (131 (let ((n (h0-eval (cadr expr) env fenv))  ; NTH
               (lst (h0-eval (caddr expr) env fenv)))
           (labels ((nth-helper (i l) (if (= i 0) (car l) (nth-helper (- i 1) (cdr l)))))
             (nth-helper n lst))))
    (132 (let ((cell (h0-eval (cadr expr) env fenv))  ; SETCAR
               (val (h0-eval (caddr expr) env fenv)))
           (setcar cell val) val))
    (133 (let ((cell (h0-eval (cadr expr) env fenv))  ; SETCDR
               (val (h0-eval (caddr expr) env fenv)))
           (setcdr cell val) val))
    (134 (let ((arg (h0-eval (cadr expr) env fenv)))  ; REVERSE
           (labels ((rev-acc (lst acc) (if (null lst) acc (rev-acc (cdr lst) (cons (car lst) acc)))))
             (rev-acc arg nil))))
    (135 (h0-eval-mapcar expr env fenv))  ; MAPCAR
    ;; Type predicates (141-147)
    (141 (if (null (h0-eval (cadr expr) env fenv)) t nil))     ; NULL
    (142 (if (h0-consp (h0-eval (cadr expr) env fenv)) t nil))    ; CONSP
    (143 (if (h0-symbolp (h0-eval (cadr expr) env fenv)) t nil))  ; SYMBOLP
    (144 (if (h0-numberp (h0-eval (cadr expr) env fenv)) t nil))  ; NUMBERP
    (145 (if (h0-stringp (h0-eval (cadr expr) env fenv)) t nil))  ; STRINGP
    (146 (if (h0-keywordp (h0-eval (cadr expr) env fenv)) t nil)) ; KEYWORDP
    (147 (let ((arg (h0-eval (cadr expr) env fenv)))  ; LISTP
           (if (or (null arg) (h0-consp arg)) t nil)))
    ;; Boolean operations (151-153)
    (151 (if (h0-eval (cadr expr) env fenv) nil t))  ; NOT
    (152 (h0-eval-and (cdr expr) env fenv))  ; AND
    (153 (h0-eval-or (cdr expr) env fenv))   ; OR
    ;; String operations (161-165)
    (161 (string-length (h0-eval (cadr expr) env fenv)))  ; STRING-LENGTH
    (162 (string-ref (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; STRING-REF
    (163 (let ((str (h0-eval (cadr expr) env fenv))  ; CHAR-AT
               (idx (h0-eval (caddr expr) env fenv)))
           (if (< idx (string-length str)) (string-ref str idx) 0)))
    (164 (if (string= (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))  ; STRING=
    (165 (string-equal (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; STRING-EQUAL
    ;; Symbol operations (171-174)
    (171 (sym-eq (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; SYM-EQ
    (172 (member (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; MEMBER
    (173 (symbol-name (h0-eval (cadr expr) env fenv)))   ; SYMBOL-NAME
    (174 (keyword-name (h0-eval (cadr expr) env fenv)))  ; KEYWORD-NAME
    ;; Bitwise operations (181-184)
    (181 (logand (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; LOGAND
    (182 (logior (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; LOGIOR
    (183 (ash (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))     ; ASH
    (184 (lognot (h0-eval (cadr expr) env fenv)))  ; LOGNOT
    ;; Equality (191-192)
    (191 (if (eq (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))  ; EQ
    (192 (eql (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; EQL
    ;; Low-level operations (201-209)
    (201 (get-tag (h0-eval (cadr expr) env fenv)))  ; GET-TAG
    (202 (set-tag (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; SET-TAG
    (203 (let ((arg (h0-eval (cadr expr) env fenv)))  ; LENGTH
           (labels ((count-len (lst n) (if (null lst) n (count-len (cdr lst) (+ n 1)))))
             (count-len arg 0))))
    (204 (make-vector (h0-eval (cadr expr) env fenv)))  ; MAKE-VECTOR
    (205 (vector-length (h0-eval (cadr expr) env fenv)))  ; VECTOR-LENGTH
    (206 (let ((vec (h0-eval (cadr expr) env fenv))  ; VECTOR-SET
               (idx (h0-eval (caddr expr) env fenv))
               (val (h0-eval (cadddr expr) env fenv)))
           (vector-set vec idx val) val))
    (207 (vector-ref (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))  ; VECTOR-REF
    (208 (make-string-from-vector (h0-eval (cadr expr) env fenv)))  ; MAKE-STRING-FROM-VECTOR
    (209 (make-symbol-from-string (h0-eval (cadr expr) env fenv)))  ; MAKE-SYMBOL-FROM-STRING
    ;; Default - function call
    (t (h0-eval-call (car expr) expr env fenv))))

;; FUNCALL handler - extracted for clarity
(defun h0-eval-funcall (expr env fenv)
  (let* ((fn-expr (cadr expr))
         (args (h0-eval-list (cddr expr) env fenv)))
    (cond
      ;; Quoted symbol: (funcall 'foo ...)
      ((and (consp fn-expr) (eq (op-lookup (car fn-expr)) 1))  ; QUOTE=1
       (let* ((fn-sym (cadr fn-expr))
              (fn-entry (fenv-lookup fn-sym fenv)))
         (if fn-entry
             (if (keywordp (car fn-entry))
                 (h0-eval-builtin (cdr fn-entry) args fenv)
                 (let ((new-env (bind-lambda-args (car fn-entry) args nil fenv)))
                   (h0-eval (cdr fn-entry) new-env fenv)))
             (fatal-error "h0-eval: FUNCALL unknown function"))))
      ;; Bare symbol
      ((symbolp fn-expr)
       (let ((var-entry (env-lookup fn-expr env)))
         (if var-entry
             (let ((fn (cdr var-entry)))
               (if (and (consp fn) (eq (car fn) (intern "CLOSURE-TAG")))
                   (let ((new-env (bind-lambda-args (cadr fn) args (cadddr fn) fenv)))
                     (h0-eval (caddr fn) new-env fenv))
                   (if (symbolp fn)
                       (let ((fn-entry (fenv-lookup fn fenv)))
                         (if fn-entry
                             (if (keywordp (car fn-entry))
                                 (h0-eval-builtin (cdr fn-entry) args fenv)
                                 (let ((new-env (bind-lambda-args (car fn-entry) args nil fenv)))
                                   (h0-eval (cdr fn-entry) new-env fenv)))
                             (fatal-error "h0-eval: FUNCALL unknown function")))
                       (fatal-error "h0-eval: FUNCALL on non-closure"))))
             (let ((fn-entry (fenv-lookup fn-expr fenv)))
               (if fn-entry
                   (if (keywordp (car fn-entry))
                       (h0-eval-builtin (cdr fn-entry) args fenv)
                       (let ((new-env (bind-lambda-args (car fn-entry) args nil fenv)))
                         (h0-eval (cdr fn-entry) new-env fenv)))
                   (fatal-error "h0-eval: FUNCALL unknown function"))))))
      ;; Expression that evaluates to closure
      (t (let ((fn (h0-eval fn-expr env fenv)))
           (if (and (consp fn) (eq (car fn) (intern "CLOSURE-TAG")))
               (let ((new-env (bind-lambda-args (cadr fn) args (cadddr fn) fenv)))
                 (h0-eval (caddr fn) new-env fenv))
               (fatal-error "h0-eval: FUNCALL on non-closure")))))))

;; MAPCAR handler
(defun h0-eval-mapcar (expr env fenv)
  (let* ((fn-expr (cadr expr))
         (lst (h0-eval (caddr expr) env fenv)))
    (labels ((map-loop (items acc)
               (if (null items) (reverse acc)
                   (let ((result (h0-eval (list 'funcall fn-expr (list 'quote (car items))) env fenv)))
                     (map-loop (cdr items) (cons result acc))))))
      (map-loop lst nil))))

;; Function call handler
;; Dispatch priority:
;; 1. fenv entry with :builtin keyword -> h0-eval-builtin
;; 2. fenv entry with (params . body) -> interpret body
;; 3. SBCL: symbol has function binding -> call directly via funcall
;; 4. Error: unknown function
(defun h0-eval-call (op expr env fenv)
  (let ((fn-entry (fenv-lookup op fenv)))
    (if fn-entry
        (if (keywordp (car fn-entry))
            (let ((args (h0-eval-list (cdr expr) env fenv)))
              (h0-eval-builtin (cdr fn-entry) args fenv))
            (let* ((params (car fn-entry))
                   (body (cdr fn-entry))
                   (args (h0-eval-list (cdr expr) env fenv))
                   (new-env (bind-lambda-args params args nil fenv)))
              (h0-eval body new-env fenv)))
        ;; Not in fenv - try calling as compiled function (SBCL bootstrap only)
        #+sbcl
        (let ((sbcl-sym (find-symbol (symbol-name op) :habu)))
          (if (and sbcl-sym (fboundp sbcl-sym))
              (let ((args (h0-eval-list (cdr expr) env fenv)))
                (apply (symbol-function sbcl-sym) args))
              (fatal-error "h0-eval: unknown function")))
        #-sbcl
        (fatal-error "h0-eval: unknown function"))))

;; Builtin dispatch table - maps symbol name strings to handler functions
;; Built once at init, used for O(n) lookup (could use hash table for O(1))
(defvar *builtin-dispatch* nil)

;; Get keyword argument value from args list
;; key-kw is a keyword like :OFFSET, not a string
(defun get-kw-arg (args key-kw default)
  (let ((found (find-key-arg args key-kw)))
    (if found (cdr found) default)))

(defun init-builtin-dispatch ()
  "Build dispatch table mapping symbol to dispatch ID (simple integer)"
  ;; Use integers as dispatch keys - avoids #' function references
  (setq *builtin-dispatch*
        (list
         ;; Compiler functions - front end (IDs 1-3)
         (cons (intern "H0-COMPILE") 1)
         (cons (intern "LIFT-LAMBDAS") 2)
         (cons (intern "LAMBDAS-TO-DEFUNS") 3)
         ;; Register allocator pipeline (IDs 60-79)
         (cons (intern "CODEGEN-FN-REG-ALLOC") 60)
         (cons (intern "CODEGEN-MAIN-REG-ALLOC") 61)
         (cons (intern "IR-TO-TAC") 62)
         (cons (intern "COMPUTE-LIVENESS") 63)
         (cons (intern "COMPUTE-INTERVALS") 64)
         (cons (intern "LINEAR-SCAN") 65)
         (cons (intern "TAC-CODEGEN") 66)
         (cons (intern "MAKE-VREG-COUNTER") 67)
         (cons (intern "NEXT-VREG") 68)
         (cons (intern "TAC-DEF") 73)
         (cons (intern "TAC-USE") 74)
         ;; Backend - codegen.lisp (IDs 4-7, 70+)
         (cons (intern "DELIVER-WITH-IMPORTS-AND-HEAP") 4)
         (cons (intern "READ-ALL") 5)
         (cons (intern "NATIVE-READ-FILE") 6)
         (cons (intern "COLLECT-DEFUNS") 7)
         (cons (intern "CODEGEN-FN") 70)
         (cons (intern "RESOLVE-CALLS") 71)
         (cons (intern "FLATTEN-ALL-CALLS") 72)
         ;; ARM64 - memory (IDs 10-13)
         (cons (intern "STR") 10)
         (cons (intern "LDR") 11)
         (cons (intern "STP") 12)
         (cons (intern "LDP") 13)
         ;; ARM64 - data movement (IDs 20-21)
         (cons (intern "MOV") 20)
         (cons (intern "MOVZ") 21)
         ;; ARM64 - arithmetic (IDs 30-31)
         (cons (intern "ADD") 30)
         (cons (intern "SUB") 31)
         ;; ARM64 - compare/branch (IDs 40-48)
         (cons (intern "CMP") 40)
         (cons (intern "B") 41)
         (cons (intern "BL") 42)
         (cons (intern "B.EQ") 43)
         (cons (intern "B.NE") 44)
         (cons (intern "CBZ") 45)
         (cons (intern "CBNZ") 46)
         (cons (intern "RET") 47)
         (cons (intern "NOP") 48)
         ;; ARM64 - utility (IDs 50+)
         (cons (intern "REG") 50)
         ;; IR constructors (IDs 80-99) - needed for native mode 1024
         ;; In SBCL mode, h0-eval-call falls back to symbol-function
         ;; In native mode, these must be explicitly registered
         (cons (intern "IR-LIT") 80)
         (cons (intern "IR-VAR") 81)
         (cons (intern "IR-ADD") 82)
         (cons (intern "IR-SUB") 83)
         (cons (intern "IR-IF") 84)
         (cons (intern "IR-CONS") 85)
         (cons (intern "IR-CAR") 86)
         (cons (intern "IR-CDR") 87)
         (cons (intern "MAKE-VREG-COUNTER") 88)
         (cons (intern "NEXT-VREG") 89))))

;; Lookup dispatch ID in table - NO FALLBACK
;; Uses eq comparison only. If symbol not found, crashes with error.
;; Fallbacks mask bugs - if symbols don't match by eq, fix the root cause.
(defun find-builtin-id (name table)
  (let ((result (find-builtin-id-eq name table)))
    (if result
        result
        (fatal-error "find-builtin-id: unknown function - symbol mismatch"))))

;; eq comparison for same-intern-table symbols
(defun find-builtin-id-eq (name table)
  (if (null table)
      nil
      (if (eq name (caar table))
          (cdar table)
          (find-builtin-id-eq name (cdr table)))))

;;; ============================================================
;;; Keyword Normalization for SBCL/habu0 Boundary
;;; ============================================================
;;
;; Problem: In SBCL, keywords like :x0 are SBCL keyword symbols.
;; In habu0 native, keywords should be habu0-interned with tag 10.
;; When ARM64 functions use eq-comparison, SBCL keywords won't match
;; habu0 interned keywords.
;;
;; Solution: At the h0-eval-builtin boundary (where SBCL-evaluated
;; values cross into ARM64/habu0 code), normalize all keywords to
;; habu0-interned keywords. This allows eq comparison to work.
;;
;; Note: This only matters in mode 1024 (self-compile) where SBCL
;; evaluates code that produces keywords which then get passed to
;; ARM64 assembler functions.

(defun normalize-keyword (kw)
  "Normalize a keyword to habu0-interned form.
   If kw is an SBCL keyword (symbolp = t, keywordp = t), re-intern it.
   If kw is already a habu0 keyword (tag 10, symbolp = nil), return as-is."
  (if (keywordp kw)
      (if (symbolp kw)
          ;; SBCL keyword: symbol-name works, re-intern in habu0
          (intern-keyword (symbol-name kw))
          ;; Already habu0 keyword (tag 10, not symbolp)
          kw)
      ;; Not a keyword, return as-is
      kw))

(defun normalize-args (args)
  "Normalize all keywords in an argument list.
   Recursively handles nested lists for keyword args like :offset 8.
   Non-cons atoms (fixnums, etc.) pass through unchanged."
  (cond
    ((null args) nil)
    ((not (consp args)) args)  ; Atoms (fixnums, symbols) pass through
    (t (let ((arg (car args)))
         (cons (if (consp arg)
                   (normalize-args arg)  ; Handle nested structures
                   (normalize-keyword arg))
               (normalize-args (cdr args)))))))

;;; habu0-reg: Register lookup using eq comparison
;;; After normalization, keywords are habu0-interned, so eq works.
;;; This replaces arm64:reg's string comparison at the boundary.
(defun habu0-reg (r)
  "Convert register keyword to number using dispatch table lookup.
   Uses *register-table* populated by init-compile-ops."
  (let ((entry (assoc r *register-table* :test #'eq)))
    (if entry
        (cdr entry)
        (error "habu0-reg: unknown register keyword"))))

;;; fenv-call-arm64: Call an ARM64 function from fenv
;;; In SBCL mode 1024, ARM64 functions are in fenv as (name . fn-entry)
;;; where fn-entry = (:builtin . impl-fn) for builtins
;;; In native habu0, this function exists but ARM64 functions aren't in fenv,
;;; so fenv-lookup returns nil and we crash (no silent fallback)
;;; Note: Uses funcall with explicit arity since apply isn't a habu0 primitive
(defun fenv-call-arm64 (name nargs fenv)
  "Call ARM64 function by name from fenv. Crashes if not found."
  (let ((fn-entry (fenv-lookup name fenv)))
    (if (null fn-entry)
        (error "ARM64 function not in fenv: ~A" name)
        ;; fn-entry should be (:builtin . actual-function)
        ;; The actual-function is callable via funcall
        (let ((impl (cdr fn-entry))
              (n (length nargs)))
          (if (null impl)
              (error "ARM64 function has no impl: ~A" name)
              ;; Dispatch by arity using case
              (case n
                (0 (funcall impl))
                (1 (funcall impl (car nargs)))
                (2 (funcall impl (car nargs) (cadr nargs)))
                (3 (funcall impl (car nargs) (cadr nargs) (caddr nargs)))
                (4 (funcall impl (car nargs) (cadr nargs) (caddr nargs) (cadddr nargs)))
                (5 (funcall impl (car nargs) (cadr nargs) (caddr nargs) (cadddr nargs) (nth 4 nargs)))
                (6 (funcall impl (car nargs) (cadr nargs) (caddr nargs) (cadddr nargs) (nth 4 nargs) (nth 5 nargs)))
                (otherwise (error "fenv-call-arm64: too many args"))))))))

;; Dispatch builtin functions via ID lookup and match
;; IMPORTANT: Normalize args at this boundary to ensure keywords are
;; habu0-interned, allowing eq comparison in ARM64 functions.
(defun h0-eval-builtin (name args fenv)
  (let* ((nargs (normalize-args args))  ; Normalize keywords at boundary
         (id (find-builtin-id name *builtin-dispatch*)))
    (if (null id)
        (fatal-error "h0-eval-builtin: unknown builtin")
        (case id
          ;; Compiler functions - front end
          (1 (h0-compile (car args) (cadr args) (caddr args)))
          (2 (lift-lambdas (car args)))
          (3 (lambdas-to-defuns (car args)))
          ;; Backend - codegen.lisp
          (4 (deliver-with-imports-and-heap (car args) (cadr args) (caddr args) (cadddr args)))
          (5 (read-all (car args)))
          (6 (native-read-file (car args)))
          (7 (collect-defuns (car args) (cadr args)))
          ;; Register allocator pipeline (IDs 60-69)
          (60 (codegen-fn-reg-alloc (car args)))
          (61 (codegen-main-reg-alloc (car args)))
          (62 (ir-to-tac (car args) (cadr args)))
          (63 (compute-liveness (car args)))
          (64 (compute-intervals (car args)))
          (65 (linear-scan (car args)))
          (66 (tac-codegen (car args) (cadr args)))
          (67 (make-vreg-counter))
          (68 (next-vreg (car args)))
          ;; TAC helpers (IDs 73-74)
          (73 (tac-def (car args)))
          (74 (tac-use (car args)))
          ;; More codegen.lisp functions (IDs 70+)
          (70 (codegen-fn (car args)))
          (71 (resolve-calls (car args) (cadr args) (caddr args)))
          (72 (flatten-all-calls (car args)))
          ;; ARM64 functions - SBCL mode 1024 only
          ;; In SBCL, calls arm64:* functions directly via fenv-lookup
          ;; In native habu0, ARM64 IDs 10-48 fall through to fatal-error
          ;; (native habu0 generates ARM64 code directly, doesn't use h0-eval-builtin)
          (10 (fenv-call-arm64 'str nargs fenv))
          (11 (fenv-call-arm64 'ldr nargs fenv))
          (12 (fenv-call-arm64 'stp nargs fenv))
          (13 (fenv-call-arm64 'ldp nargs fenv))
          (20 (fenv-call-arm64 'mov nargs fenv))
          (21 (fenv-call-arm64 'movz nargs fenv))
          (30 (fenv-call-arm64 'add nargs fenv))
          (31 (fenv-call-arm64 'sub nargs fenv))
          (40 (fenv-call-arm64 'cmp nargs fenv))
          (41 (fenv-call-arm64 'b nargs fenv))
          (42 (fenv-call-arm64 'bl nargs fenv))
          (43 (fenv-call-arm64 'b.eq nargs fenv))
          (44 (fenv-call-arm64 'b.ne nargs fenv))
          (45 (fenv-call-arm64 'cbz nargs fenv))
          (46 (fenv-call-arm64 'cbnz nargs fenv))
          (47 (fenv-call-arm64 'ret nargs fenv))
          (48 (fenv-call-arm64 'nop nargs fenv))
          ;; ARM64 - utility (use nargs with habu0-reg for eq comparison)
          (50 (habu0-reg (car nargs)))
          ;; IR constructors (IDs 80-89)
          (80 (ir-lit (car args)))
          (81 (ir-var (car args)))
          (82 (ir-add (car args) (cadr args)))
          (83 (ir-sub (car args) (cadr args)))
          (84 (ir-if (car args) (cadr args) (caddr args)))
          (85 (ir-cons (car args) (cadr args)))
          (86 (ir-car (car args)))
          (87 (ir-cdr (car args)))
          (88 (make-vreg-counter))
          (89 (next-vreg (car args)))
          (otherwise (fatal-error "h0-eval-builtin: unhandled dispatch ID"))))))

;; Eval a list of expressions
(defun h0-eval-list (exprs env fenv)
  (if (null exprs) nil
      (cons (h0-eval (car exprs) env fenv)
            (h0-eval-list (cdr exprs) env fenv))))

;; Collect function definitions from forms
;; Stores (symbol . (params . body)) for eq-based lookup
(defun collect-defuns (forms fenv)
  (if (null forms) fenv
      (let ((form (car forms)))
        (if (and (consp form) (symbolp (car form)) (op=defun (car form)))
            (let* ((name (cadr form))  ;; Keep as symbol, not string
                   (params (caddr form))
                   (raw-body (cadddr form))
                   ;; Transform body to handle mutable captured variables
                   (body (h0-box-mutable-captures raw-body)))
              (collect-defuns (cdr forms) (cons (cons name (cons params body)) fenv)))
            (collect-defuns (cdr forms) fenv)))))

;; Eval forms with collected function definitions
;; Applies box-mutable-captures transformation to handle setq in closures
(defun h0-eval-forms (forms env fenv)
  (if (null forms)
      nil
      (let ((form (car forms)))
        ;; Skip defun forms during evaluation
        (if (and (consp form) (symbolp (car form)) (op=defun (car form)))
            (h0-eval-forms (cdr forms) env fenv)
            ;; Transform form to box mutable captured variables
            (let ((transformed (h0-box-mutable-captures form)))
              (if (null (cdr forms))
                  (h0-eval transformed env fenv)
                  (progn
                    (h0-eval transformed env fenv)
                    (h0-eval-forms (cdr forms) env fenv))))))))

;;; ==========================================================================
;;; IR Compiler - Source to IR transformation
;;; ==========================================================================
;;; IR format:
;;;   (lit n)           - literal number (will be tagged as fixnum)
;;;   (var offset)      - variable reference from environment
;;;   (add left right)  - addition
;;;   (sub left right)  - subtraction
;;;   (mul left right)  - multiplication
;;;   (div left right)  - division
;;;   (mod-ir left right) - modulo
;;;   (cmp-eq left right) - equality comparison
;;;   (cmp-lt left right) - less than
;;;   (cmp-gt left right) - greater than
;;;   (cmp-le left right) - less than or equal
;;;   (cmp-ge left right) - greater than or equal
;;;   (if-ir test then else) - conditional
;;;   (cons-ir car cdr) - cons cell allocation
;;;   (car-ir val)      - car of cons cell
;;;   (cdr-ir val)      - cdr of cons cell
;;;   (null-ir val)     - null check
;;;   (progn-ir forms)  - sequence

;; Symbol comparison helper for compilation
;; Uses symbol-name for string comparison since make-symbol-from-string
;; Symbol comparison using intern - ensures eq correctness
(defun sym= (sym name)
  "Check if symbol equals the interned symbol for name"
  (eq sym (intern name)))

;;; ============================================================
;;; Macro-based symbol interning for mode 1024
;;; ============================================================
;;;
;;; Problem: In mode 1024, the binary contains SBCL-compiled code.
;;; String literals like "QUOTE" become SBCL strings embedded in the binary.
;;; Habu's string primitives (string-length, string-ref) expect habu strings
;;; (tag 6), so they crash when given SBCL strings.
;;;
;;; Solution: Use macros to expand string literals at SBCL compile time
;;; into code that builds habu strings using only integer literals and
;;; habu primitives. The generated code works correctly at runtime.
;;;
;;; Example: (make-habu-string-form "HI") expands to:
;;;   (let ((vec (make-vector 2)))
;;;     (vector-set vec 0 72)  ; H
;;;     (vector-set vec 1 73)  ; I
;;;     (make-string-from-vector vec))

#+sbcl
(defmacro make-habu-string-form (str)
  "Expand a string literal to habu primitive calls at compile time.
   The generated code uses only integer literals - no SBCL strings."
  (let ((len (length str))
        (vec-sym (gensym "VEC")))
    `(let ((,vec-sym (make-vector ,len)))
       ,@(loop for i from 0 below len
               collect `(vector-set ,vec-sym ,i ,(char-code (char str i))))
       (make-string-from-vector ,vec-sym))))

#+sbcl
(defmacro make-habu-symbol-form (str)
  "Create a habu symbol and register it in the intern table.
   Expands at compile time to code using only integer literals."
  (let ((str-sym (gensym "STR"))
        (sym-sym (gensym "SYM")))
    `(let* ((,str-sym (make-habu-string-form ,str))
            (,sym-sym (make-symbol-from-string ,str-sym)))
       ;; Use intern-table-add to properly add to hash table
       (intern-table-add ,str-sym ,sym-sym)
       ,sym-sym)))

#+sbcl
(defmacro make-habu-keyword-form (str)
  "Create a habu keyword and register it in the keyword table.
   Expands at compile time to code using only integer literals."
  (let ((str-sym (gensym "STR"))
        (kw-sym (gensym "KW")))
    `(let* ((,str-sym (make-habu-string-form ,str))
            (,kw-sym (make-keyword-from-string ,str-sym)))
       ;; Use keyword-table-add to properly add to hash table
       (keyword-table-add ,str-sym ,kw-sym)
       ,kw-sym)))

;; For non-SBCL (native habu), these just use regular functions
#-sbcl
(defun make-habu-string-form (str) str)  ; Native strings are already habu strings

#-sbcl
(defun make-habu-symbol-form (str)
  (intern str))

#-sbcl
(defun make-habu-keyword-form (str)
  (intern-keyword str))

;;; Macro to define the register dispatch table
;;; Each entry maps a register keyword to its number
;;; Expands at compile time to code using only integer literals
#+sbcl
(defmacro define-registers (&rest entries)
  "Populate *register-table* with (keyword . number) pairs.
   ENTRIES are (name number) lists like (\"X0\" 0).
   Expands at compile time to code that builds habu keywords."
  `(setq *register-table*
     (list ,@(loop for entry in entries
                   collect `(cons (make-habu-keyword-form ,(car entry)) ,(cadr entry))))))

#-sbcl
(defmacro define-registers (&rest entries)
  "Native habu version - strings work directly."
  `(setq *register-table*
     (list ,@(loop for entry in entries
                   collect `(cons (intern-keyword ,(car entry)) ,(cadr entry))))))

;;; Macro to define the operator dispatch table
;;; Each entry maps a habu symbol to a dispatch ID (integer)
;;; This replaces 88 *op-* variables with a single dispatch table
#+sbcl
(defmacro define-ops (&rest entries)
  "Populate *op-dispatch-table* with (habu-symbol . dispatch-id) pairs.
   ENTRIES are (name id) lists like (\"QUOTE\" 1) (\"IF\" 2).
   Expands at compile time to code that builds habu symbols."
  `(setq *op-dispatch-table*
     (list ,@(loop for entry in entries
                   collect `(cons (make-habu-symbol-form ,(car entry)) ,(cadr entry))))))

#-sbcl
(defmacro define-ops (&rest entries)
  "Native habu version - strings work directly via intern."
  `(setq *op-dispatch-table*
     (list ,@(loop for entry in entries
                   collect `(cons (intern ,(car entry)) ,(cadr entry))))))

;;; Look up operator dispatch ID from symbol - NO FALLBACK
;;; Returns dispatch ID (integer) or nil if not a special form
;;; Uses eq comparison only. No string fallbacks - they mask bugs.
(defun op-lookup (sym)
  "Look up dispatch ID for operator symbol. Returns nil for function calls."
  (let ((entry (assoc sym *op-dispatch-table* :test #'eq)))
    (if entry
        (cdr entry)
        nil)))

;; Initialize compile ops - create habu symbols at runtime
;; Uses macros to expand string literals to integer-based construction at compile time.
;; This ensures habu-read returns the SAME symbol object, enabling eq comparison.
(defun init-compile-ops ()
  ;; Create habu symbols using macro-generated code
  ;; Each macro call expands at SBCL compile time to code that:
  ;; 1. Creates a habu string from char codes (integers)
  ;; 2. Creates a habu symbol from that string
  ;; 3. Registers it in the intern table
  ;; No SBCL strings in the generated runtime code!
  (setq *op-quote* (make-habu-symbol-form "QUOTE"))
  (setq *op-if* (make-habu-symbol-form "IF"))
  (setq *op-let* (make-habu-symbol-form "LET"))
  (setq *op-let-star* (make-habu-symbol-form "LET*"))
  (setq *op-defun* (make-habu-symbol-form "DEFUN"))
  (setq *op-defvar* (make-habu-symbol-form "DEFVAR"))
  (setq *op-while* (make-habu-symbol-form "WHILE"))
  (setq *op-progn* (make-habu-symbol-form "PROGN"))
  (setq *op-cond* (make-habu-symbol-form "COND"))
  (setq *op-t* (make-habu-symbol-form "T"))
  (setq *op-plus* (make-habu-symbol-form "+"))
  (setq *op-minus* (make-habu-symbol-form "-"))
  (setq *op-mul* (make-habu-symbol-form "*"))
  (setq *op-div* (make-habu-symbol-form "/"))
  (setq *op-mod* (make-habu-symbol-form "MOD"))
  (setq *op-eq-num* (make-habu-symbol-form "="))
  (setq *op-lt* (make-habu-symbol-form "<"))
  (setq *op-gt* (make-habu-symbol-form ">"))
  (setq *op-le* (make-habu-symbol-form "<="))
  (setq *op-ge* (make-habu-symbol-form ">="))
  (setq *op-cons* (make-habu-symbol-form "CONS"))
  (setq *op-car* (make-habu-symbol-form "CAR"))
  (setq *op-cdr* (make-habu-symbol-form "CDR"))
  (setq *op-cadr* (make-habu-symbol-form "CADR"))
  (setq *op-cddr* (make-habu-symbol-form "CDDR"))
  (setq *op-caddr* (make-habu-symbol-form "CADDR"))
  (setq *op-cadddr* (make-habu-symbol-form "CADDDR"))
  (setq *op-null* (make-habu-symbol-form "NULL"))
  (setq *op-consp* (make-habu-symbol-form "CONSP"))
  (setq *op-list* (make-habu-symbol-form "LIST"))
  (setq *op-not* (make-habu-symbol-form "NOT"))
  (setq *op-and* (make-habu-symbol-form "AND"))
  (setq *op-or* (make-habu-symbol-form "OR"))
  (setq *op-defpackage* (make-habu-symbol-form "DEFPACKAGE"))
  (setq *op-in-package* (make-habu-symbol-form "IN-PACKAGE"))
  (setq *op-case* (make-habu-symbol-form "CASE"))
  (setq *op-when* (make-habu-symbol-form "WHEN"))
  (setq *op-unless* (make-habu-symbol-form "UNLESS"))
  (setq *op-declaim* (make-habu-symbol-form "DECLAIM"))
  (setq *op-setq* (make-habu-symbol-form "SETQ"))
  (setq *op-error* (make-habu-symbol-form "ERROR"))
  ;; Additional operators
  (setq *op-symbolp* (make-habu-symbol-form "SYMBOLP"))
  (setq *op-numberp* (make-habu-symbol-form "NUMBERP"))
  (setq *op-stringp* (make-habu-symbol-form "STRINGP"))
  (setq *op-keywordp* (make-habu-symbol-form "KEYWORDP"))
  (setq *op-string-length* (make-habu-symbol-form "STRING-LENGTH"))
  (setq *op-string-ref* (make-habu-symbol-form "STRING-REF"))
  (setq *op-char-at* (make-habu-symbol-form "CHAR-AT"))
  (setq *op-string=* (make-habu-symbol-form "STRING="))
  (setq *op-string-equal* (make-habu-symbol-form "STRING-EQUAL"))
  (setq *op-sym-eq* (make-habu-symbol-form "SYM-EQ"))
  (setq *op-member* (make-habu-symbol-form "MEMBER"))
  (setq *op-symbol-name* (make-habu-symbol-form "SYMBOL-NAME"))
  (setq *op-keyword-name* (make-habu-symbol-form "KEYWORD-NAME"))
  (setq *op-logand* (make-habu-symbol-form "LOGAND"))
  (setq *op-logior* (make-habu-symbol-form "LOGIOR"))
  (setq *op-ash* (make-habu-symbol-form "ASH"))
  (setq *op-eq* (make-habu-symbol-form "EQ"))
  (setq *op-eql* (make-habu-symbol-form "EQL"))
  (setq *op-get-tag* (make-habu-symbol-form "GET-TAG"))
  (setq *op-set-tag* (make-habu-symbol-form "SET-TAG"))
  (setq *op-length* (make-habu-symbol-form "LENGTH"))
  (setq *op-make-vector* (make-habu-symbol-form "MAKE-VECTOR"))
  (setq *op-vector-length* (make-habu-symbol-form "VECTOR-LENGTH"))
  (setq *op-vector-set* (make-habu-symbol-form "VECTOR-SET"))
  (setq *op-vector-ref* (make-habu-symbol-form "VECTOR-REF"))
  (setq *op-reverse* (make-habu-symbol-form "REVERSE"))
  (setq *op-make-string-from-vector* (make-habu-symbol-form "MAKE-STRING-FROM-VECTOR"))
  (setq *op-make-symbol-from-string* (make-habu-symbol-form "MAKE-SYMBOL-FROM-STRING"))
  (setq *op-caar* (make-habu-symbol-form "CAAR"))
  (setq *op-cdar* (make-habu-symbol-form "CDAR"))
  (setq *op-nth* (make-habu-symbol-form "NTH"))
  (setq *op-lognot* (make-habu-symbol-form "LOGNOT"))
  (setq *op-neq* (make-habu-symbol-form "/="))
  (setq *op-lambda* (make-habu-symbol-form "LAMBDA"))
  (setq *op-funcall* (make-habu-symbol-form "FUNCALL"))
  (setq *op-setcar* (make-habu-symbol-form "SETCAR"))
  (setq *op-setcdr* (make-habu-symbol-form "SETCDR"))
  (setq *op-dolist* (make-habu-symbol-form "DOLIST"))
  (setq *op-flet* (make-habu-symbol-form "FLET"))
  (setq *op-labels* (make-habu-symbol-form "LABELS"))
  (setq *op-mapcar* (make-habu-symbol-form "MAPCAR"))
  (setq *op-ecase* (make-habu-symbol-form "ECASE"))
  (setq *op-listp* (make-habu-symbol-form "LISTP"))
  (setq *op-nil* (make-habu-symbol-form "NIL"))
  (setq *op-otherwise* (make-habu-symbol-form "OTHERWISE"))
  ;; Initialize special keywords for eq comparison
  (setq *kw-offset* (make-habu-keyword-form "OFFSET"))
  (setq *kw-imm* (make-habu-keyword-form "IMM"))
  ;; Initialize register dispatch table - single table instead of 40+ variables
  ;; Each entry is (keyword . register-number) for O(1) lookup in habu0-reg
  (define-registers
    ;; General purpose registers x0-x30
    ("X0" 0) ("X1" 1) ("X2" 2) ("X3" 3) ("X4" 4) ("X5" 5) ("X6" 6) ("X7" 7)
    ("X8" 8) ("X9" 9) ("X10" 10) ("X11" 11) ("X12" 12) ("X13" 13) ("X14" 14) ("X15" 15)
    ("X16" 16) ("X17" 17) ("X18" 18) ("X19" 19) ("X20" 20) ("X21" 21) ("X22" 22) ("X23" 23)
    ("X24" 24) ("X25" 25) ("X26" 26) ("X27" 27) ("X28" 28) ("X29" 29) ("X30" 30)
    ;; Special registers
    ("SP" 31) ("XZR" 31) ("LR" 30) ("FP" 29)
    ;; Habu-specific aliases
    ("ENV" 20) ("CLOSURE" 24) ("CODE-BASE" 26) ("GC" 27) ("HEAP" 28))
  ;; Initialize operator dispatch table - single table for efficient h0-eval dispatch
  ;; Dispatch IDs 1-100: special forms (quote, if, let, etc.)
  ;; Dispatch IDs 101-200: primitives (+, -, car, cdr, etc.)
  ;; nil from op-lookup means function call via fenv
  (define-ops
    ;; Special forms (1-30)
    ("QUOTE" 1) ("IF" 2) ("LET" 3) ("LET*" 4) ("DEFUN" 5) ("DEFVAR" 6)
    ("WHILE" 7) ("PROGN" 8) ("COND" 9) ("DEFPACKAGE" 10) ("IN-PACKAGE" 11)
    ("CASE" 12) ("WHEN" 13) ("UNLESS" 14) ("DECLAIM" 15) ("SETQ" 16)
    ("ERROR" 17) ("LAMBDA" 18) ("DOLIST" 19) ("LABELS" 20) ("FLET" 21)
    ("FUNCALL" 22) ("ECASE" 23)
    ;; Self-evaluating symbols (30-39)
    ("T" 30) ("NIL" 31) ("OTHERWISE" 32)
    ;; Arithmetic (101-110)
    ("+" 101) ("-" 102) ("*" 103) ("/" 104) ("MOD" 105)
    ;; Comparisons (111-120)
    ("=" 111) ("<" 112) (">" 113) ("<=" 114) (">=" 115) ("/=" 116)
    ;; List operations (121-140)
    ("CONS" 121) ("CAR" 122) ("CDR" 123) ("CADR" 124) ("CDDR" 125)
    ("CADDR" 126) ("CADDDR" 127) ("LIST" 128) ("CAAR" 129) ("CDAR" 130)
    ("NTH" 131) ("SETCAR" 132) ("SETCDR" 133) ("REVERSE" 134) ("MAPCAR" 135)
    ;; Type predicates (141-150)
    ("NULL" 141) ("CONSP" 142) ("SYMBOLP" 143) ("NUMBERP" 144) ("STRINGP" 145)
    ("KEYWORDP" 146) ("LISTP" 147)
    ;; Boolean operations (151-160)
    ("NOT" 151) ("AND" 152) ("OR" 153)
    ;; String operations (161-170)
    ("STRING-LENGTH" 161) ("STRING-REF" 162) ("CHAR-AT" 163) ("STRING=" 164) ("STRING-EQUAL" 165)
    ;; Symbol operations (171-180)
    ("SYM-EQ" 171) ("MEMBER" 172) ("SYMBOL-NAME" 173) ("KEYWORD-NAME" 174)
    ;; Bitwise operations (181-190)
    ("LOGAND" 181) ("LOGIOR" 182) ("ASH" 183) ("LOGNOT" 184)
    ;; Equality (191-200)
    ("EQ" 191) ("EQL" 192)
    ;; Low-level operations (201-220)
    ("GET-TAG" 201) ("SET-TAG" 202) ("LENGTH" 203)
    ("MAKE-VECTOR" 204) ("VECTOR-LENGTH" 205) ("VECTOR-SET" 206) ("VECTOR-REF" 207)
    ("MAKE-STRING-FROM-VECTOR" 208) ("MAKE-SYMBOL-FROM-STRING" 209))
  nil)

;; Environment lookup for compilation - returns offset or nil
;; Now uses eq since all symbols are properly interned
;; Note: Uses separate helper functions to avoid nested closure issues
(defun c-env-lookup (sym env)
  (c-env-search (symbol-name sym) env #x0))

;; Search environment for matching name - returns (cons offset nil) or nil
;; Returns cons cell so offset 0 is distinguishable from not-found (nil)
;; Env is flat list of symbols: (sym1 sym2 sym3 ...)
(defun c-env-search (sym-name env offset)
  (if (null env)
      nil
      (let ((entry-sym (car env)))
        (if (c-names-match sym-name (symbol-name entry-sym))
            (cons offset nil)  ;; Return cons to distinguish 0 from nil
            (c-env-search sym-name (cdr env) (+ offset #x1))))))

;; Check if two name strings match
;; First checks length, then compares character by character
(defun c-names-match (s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (c-chars-match s1 s2 len1 #x0)
        nil)))

;; Compare characters of two strings up to length len, starting at index i (case-insensitive)
(defun c-chars-match (s1 s2 len i)
  (if (>= i len)
      t
      (if (= (h0-char-upcase (string-ref s1 i))
             (h0-char-upcase (string-ref s2 i)))
          (c-chars-match s1 s2 len (+ i #x1))
          nil)))

;; Extend compilation environment with new bindings
;; Bindings is list of (name . value) pairs, we just need the names
(defun c-env-extend (bindings env)
  (if (null bindings)
      env
      (let ((b (car bindings)))
        (c-env-extend (cdr bindings)
                      (cons (car b) env)))))

;;; IR constructors are now provided by shared/ir.lisp via ADT
;;; Format: (:IR :VARIANT . fields) - e.g., (ir-lit 42) => (:IR :LIT 42)
;;; Use ir-*-p predicates for type checks, ir-*-value accessors for fields


;;; Free variable analysis for closures

;; Check if a symbol is in the environment (flat list of symbols)
(defun h0-in-env (sym env)
  (if (null env)
      nil
      (if (string-equal (symbol-name sym) (symbol-name (car env)))
          t
          (h0-in-env sym (cdr env)))))

;; Check if a symbol is in a list (using case-insensitive comparison on symbol names)
(defun h0-member-sym (sym lst)
  (if (null lst)
      nil
      (if (string-equal (symbol-name sym) (symbol-name (car lst)))
          t
          (h0-member-sym sym (cdr lst)))))

;; Add symbol to list if not already present
(defun h0-add-free (sym acc)
  (if (h0-member-sym sym acc)
      acc
      (cons sym acc)))

;; Collect free variables from expression
(defun h0-collect-free (expr bound env acc)
  (cond
    ((null expr) acc)
    ((symbolp expr)
     (if (and (h0-in-env expr env)
              (not (h0-member-sym expr bound)))
         (h0-add-free expr acc)
         acc))
    ((not (consp expr)) acc)
    (t
     (case (car expr)
       (QUOTE acc)
       (LAMBDA
        (let ((params (cadr expr))
              (body (caddr expr)))
          (h0-collect-free body (h0-append-lists params bound) env acc)))
       (LET
        (let* ((bindings (cadr expr))
               (body (caddr expr))
               (names (h0-binding-names bindings))
               (vals (h0-binding-vals bindings))
               (acc2 (h0-collect-free-list vals bound env acc))
               (new-bound (h0-append-lists names bound)))
          (h0-collect-free body new-bound env acc2)))
       (LET*
        (let* ((bindings (cadr expr))
               (body (caddr expr)))
          (h0-collect-free-let* bindings body bound env acc)))
       (t (h0-collect-free-list expr bound env acc))))))

(defun h0-collect-free-list (exprs bound env acc)
  (if (null exprs)
      acc
      (let ((acc2 (h0-collect-free (car exprs) bound env acc)))
        (h0-collect-free-list (cdr exprs) bound env acc2))))

(defun h0-collect-free-let* (bindings body bound env acc)
  (if (null bindings)
      (h0-collect-free body bound env acc)
      (let* ((b (car bindings))
             (name (car b))
             (val (cadr b))
             (acc2 (h0-collect-free val bound env acc))
             (new-bound (cons name bound)))
        (h0-collect-free-let* (cdr bindings) body new-bound env acc2))))

(defun h0-binding-names (bindings)
  (if (null bindings)
      nil
      (cons (car (car bindings))
            (h0-binding-names (cdr bindings)))))

(defun h0-binding-vals (bindings)
  (if (null bindings)
      nil
      (cons (cadr (car bindings))
            (h0-binding-vals (cdr bindings)))))

(defun h0-append-lists (a b)
  (if (null a)
      b
      (cons (car a) (h0-append-lists (cdr a) b))))

(defun h0-find-free-vars (expr bound env)
  (h0-collect-free expr bound env nil))

(defun h0-get-var-offset (sym env)
  (if (null env)
      nil
      (if (string-equal (symbol-name sym) (symbol-name (car env)))
          #x0
          (let ((rest-off (h0-get-var-offset sym (cdr env))))
            (if rest-off
                (+ rest-off #x1)
                nil)))))

(defun h0-get-free-offsets (free-vars env)
  (if (null free-vars)
      nil
      (cons (h0-get-var-offset (car free-vars) env)
            (h0-get-free-offsets (cdr free-vars) env))))

(defun h0-make-param-env (params free-vars)
  (h0-make-env-with-offset params #x0
    (h0-make-env-with-offset free-vars (h0-list-length params) nil)))

;; Create flat list environment: (sym1 sym2 sym3 ...)
(defun h0-make-env-with-offset (syms base rest)
  (if (null syms)
      rest
      (cons (car syms)
            (h0-make-env-with-offset (cdr syms) (+ base #x1) rest))))

(defun h0-list-length (lst)
  (if (null lst)
      #x0
      (+ #x1 (h0-list-length (cdr lst)))))

(defun h0-compile-args (args env fenv)
  (if (null args)
      nil
      (cons (h0-compile (car args) env fenv)
            (h0-compile-args (cdr args) env fenv))))

;; Compile expression to IR (using numeric tags)
;; Uses op=* predicates for runtime symbol comparison (works in native habu0)
(defun h0-compile (expr env fenv)
  (cond
    ;; Numbers compile to literals
    ((numberp expr) (ir-lit expr))
    ;; nil is 0 (both Lisp nil and NIL symbol)
    ((null expr) (ir-lit #x0))
    ((if (symbolp expr) (op=t expr) nil) (ir-lit #x1))
    ;; String literals - allocate on heap
    ((stringp expr) (ir-str-lit expr))
    ;; Keyword literals - allocate on heap (self-evaluating)
    ((keywordp expr) (ir-kw-lit expr))
    ;; Symbols - variable lookup
    ((symbolp expr)
     (let ((result (c-env-lookup expr env)))
       (if result
           (ir-var (car result))  ;; Extract offset from (cons offset nil)
           (fatal-error-ir "h0-compile: Unknown symbol"))))
    ;; Lists - special forms or function calls
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; Quote
         ((if (symbolp op) (op=quote op) nil)
          (let ((val (cadr expr)))
            (cond
              ((numberp val) (ir-lit val))
              ;; Keywords MUST be checked before symbolp (keywords are symbols)
              ((keywordp val) (ir-kw-lit val))
              ((symbolp val) (ir-quote-sym val))
              ((null val) (ir-lit #x0))
              (t (fatal-error-ir "h0-compile: Unsupported quote type")))))
         ;; If
         ((if (symbolp op) (op=if op) nil)
          (let* ((test-ir (h0-compile (cadr expr) env fenv))
                 (then-ir (h0-compile (caddr expr) env fenv))
                 (else-ir (if (cadddr expr)
                              (h0-compile (cadddr expr) env fenv)
                              (ir-lit #x0))))
            (ir-if test-ir then-ir else-ir)))
         ;; Let - pass all body forms (cddr), not just first (caddr)
         ((if (symbolp op) (op=let op) nil)
          (h0-compile-let (cadr expr) (cddr expr) env fenv))
         ;; Let* - pass all body forms (cddr), not just first (caddr)
         ((if (symbolp op) (op=let-star op) nil)
          (h0-compile-let (cadr expr) (cddr expr) env fenv))
         ;; Setq
         ((if (symbolp op) (op=setq op) nil)
          (let* ((var-sym (cadr expr))
                 (val-ir (h0-compile (caddr expr) env fenv))
                 (result (c-env-lookup var-sym env)))
            (if result
                (ir-setq (car result) val-ir)
                (fatal-error-ir "h0-compile: SETQ unknown variable"))))
         ;; Progn
         ((if (symbolp op) (op=progn op) nil)
          (h0-compile-progn (cdr expr) env fenv))
         ;; Defun returns nil during compilation
         ((if (symbolp op) (op=defun op) nil)
          (ir-lit #x0))
         ;; Defvar returns nil during compilation (global var is runtime)
         ((if (symbolp op) (op=defvar op) nil)
          (ir-lit #x0))
         ;; While - transform to labels loop
         ((if (symbolp op) (op=while op) nil)
          (h0-compile-while (cadr expr) (cddr expr) env fenv))
         ;; Arithmetic
         ((if (symbolp op) (op=plus op) nil)
          (h0-compile-add (cdr expr) env fenv))
         ((if (symbolp op) (op=minus op) nil)
          (h0-compile-sub (cdr expr) env fenv))
         ((if (symbolp op) (op=mul op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-mul l r)))
         ((if (symbolp op) (op=div op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-div l r)))
         ((if (symbolp op) (op=mod op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-mod l r)))
         ;; Comparisons
         ((if (symbolp op) (op=eq-num op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-cmp-eq l r)))
         ((if (symbolp op) (op=lt op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-cmp-lt l r)))
         ((if (symbolp op) (op=gt op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-cmp-gt l r)))
         ((if (symbolp op) (op=le op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-cmp-le l r)))
         ((if (symbolp op) (op=ge op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-cmp-ge l r)))
         ;; List operations
         ((if (symbolp op) (op=cons op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-cons l r)))
         ((if (symbolp op) (op=car op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-car v)))
         ((if (symbolp op) (op=cdr op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-cdr v)))
         ((if (symbolp op) (op=setcar op) nil)
          (let* ((cell-ir (h0-compile (cadr expr) env fenv))
                 (val-ir (h0-compile (caddr expr) env fenv)))
            (list 'setcar-ir cell-ir val-ir)))
         ((if (symbolp op) (op=setcdr op) nil)
          (let* ((cell-ir (h0-compile (cadr expr) env fenv))
                 (val-ir (h0-compile (caddr expr) env fenv)))
            (list 'setcdr-ir cell-ir val-ir)))
         ((if (symbolp op) (op=null op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-null v)))
         ;; String operations
         ((if (symbolp op) (op=string-length op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-str-len v)))
         ((if (symbolp op) (op=string-ref op) nil)
          (let* ((str (h0-compile (cadr expr) env fenv))
                 (idx (h0-compile (caddr expr) env fenv)))
            (ir-str-ref str idx)))
         ;; CHAR-AT - safe string-ref that returns 0 beyond end
         ((if (symbolp op) (op=char-at op) nil)
          (let* ((str-expr (cadr expr))
                 (pos-expr (caddr expr))
                 (str-ir (h0-compile str-expr env fenv))
                 (pos-ir (h0-compile pos-expr env fenv))
                 (len-ir (ir-str-len str-ir))
                 (test-ir (ir-cmp-lt pos-ir len-ir))
                 (then-ir (ir-str-ref str-ir pos-ir))
                 (else-ir (ir-lit #x0)))
            (ir-if test-ir then-ir else-ir)))
         ;; STRING= - string equality comparison
         ((if (symbolp op) (op=string= op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-string-eq l r)))
         ;; SYMBOL-NAME - extract name string from symbol
         ((if (symbolp op) (op=symbol-name op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-symbol-name v)))
         ;; KEYWORD-NAME - extract name string from keyword
         ((if (symbolp op) (op=keyword-name op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-keyword-name v)))
         ;; Vector operations
         ((if (symbolp op) (op=make-vector op) nil)
          (let ((size (h0-compile (cadr expr) env fenv)))
            (ir-make-vector size)))
         ((if (symbolp op) (op=vector-ref op) nil)
          (let* ((vec (h0-compile (cadr expr) env fenv))
                 (idx (h0-compile (caddr expr) env fenv)))
            (ir-vector-ref vec idx)))
         ((if (symbolp op) (op=vector-set op) nil)
          (let* ((vec (h0-compile (cadr expr) env fenv))
                 (idx (h0-compile (caddr expr) env fenv))
                 (val (h0-compile (cadddr expr) env fenv)))
            (ir-vector-set vec idx val)))
         ((if (symbolp op) (op=vector-length op) nil)
          (let ((vec (h0-compile (cadr expr) env fenv)))
            (ir-vector-length vec)))
         ;; Type predicates
         ((if (symbolp op) (op=eq op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-eq l r)))
         ((if (symbolp op) (op=consp op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-consp v)))
         ((if (symbolp op) (op=symbolp op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-symbolp v)))
         ((if (symbolp op) (op=numberp op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-numberp v)))
         ((if (symbolp op) (op=stringp op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-stringp v)))
         ((if (symbolp op) (op=keywordp op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-keywordp v)))
         ((if (symbolp op) (op=listp op) nil)
          ;; listp = (or (null x) (consp x))
          ;; Compile as: (if (null x) t (consp x))
          (let* ((arg (cadr expr))
                 (arg-ir (h0-compile arg env fenv)))
            (ir-if (ir-null arg-ir)
                   (ir-lit #x1)
                   (ir-consp arg-ir))))
         ;; Bitwise operations
         ((if (symbolp op) (op=logand op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-band l r)))
         ((if (symbolp op) (op=logior op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-bor l r)))
         ((if (symbolp op) (op=ash op) nil)
          (let* ((val (h0-compile (cadr expr) env fenv))
                 (shift (h0-compile (caddr expr) env fenv)))
            (ir-bsh val shift)))
         ;; Boolean not
         ((if (symbolp op) (op=not op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-not v)))
         ;; OR - expand to if chain
         ((if (symbolp op) (op=or op) nil)
          (h0-compile-or (cdr expr) env fenv))
         ;; AND - expand to if chain
         ((if (symbolp op) (op=and op) nil)
          (h0-compile-and (cdr expr) env fenv))
         ;; LENGTH - list length
         ((if (symbolp op) (op=length op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-length v)))
         ;; COND - expand to nested IFs
         ((if (symbolp op) (op=cond op) nil)
          (h0-compile-cond (cdr expr) env fenv))
         ;; WHEN - expand to (if test (progn body...))
         ((if (symbolp op) (op=when op) nil)
          (let* ((test-ir (h0-compile (cadr expr) env fenv))
                 (body-ir (h0-compile-progn (cddr expr) env fenv))
                 (else-ir (ir-lit #x0)))
            (ir-if test-ir body-ir else-ir)))
         ;; UNLESS - expand to (if (not test) (progn body...))
         ((if (symbolp op) (op=unless op) nil)
          (let* ((test-ir (h0-compile (cadr expr) env fenv))
                 (not-test-ir (ir-not test-ir))
                 (body-ir (h0-compile-progn (cddr expr) env fenv))
                 (else-ir (ir-lit #x0)))
            (ir-if not-test-ir body-ir else-ir)))
         ;; CASE - expand to cond with eql comparisons
         ((if (symbolp op) (op=case op) nil)
          (let* ((keyform (cadr expr))
                 (clauses (cddr expr))
                 (key-var (make-habu-symbol-form "CASE-KEY")))
            (h0-compile
             (list *op-let* (list (list key-var keyform))
                   (cons *op-cond* (h0-expand-case-clauses key-var clauses)))
             env fenv)))
         ;; ECASE - like CASE but signals error if no clause matches
         ((if (symbolp op) (op=ecase op) nil)
          (let* ((keyform (cadr expr))
                 (clauses (cddr expr))
                 (key-var (make-habu-symbol-form "CASE-KEY"))
                 (clauses-with-error
                  (h0-append clauses
                             (list (list *op-t* (list *op-error* "ecase: no matching clause"))))))
            (h0-compile
             (list *op-let* (list (list key-var keyform))
                   (cons *op-cond* (h0-expand-case-clauses key-var clauses-with-error)))
             env fenv)))
         ;; DOLIST - expand to labels loop
         ((if (symbolp op) (op=dolist op) nil)
          (let* ((binding (cadr expr))
                 (var (car binding))
                 (list-expr (cadr binding))
                 (body (cddr expr))
                 (list-var (make-habu-symbol-form "DOLIST-LIST"))
                 (loop-fn (make-habu-symbol-form "DOLIST-LOOP"))
                 (expanded
                  (list *op-let* (list (list list-var list-expr))
                        (list *op-labels*
                              (list (list loop-fn (list)
                                          (list *op-when* list-var
                                                (list *op-let* (list (list var (list *op-car* list-var)))
                                                      (cons *op-progn*
                                                            (append body
                                                                    (list (list *op-setq* list-var (list *op-cdr* list-var))
                                                                          (list loop-fn))))))))
                              (list loop-fn))
                        nil)))
            (h0-compile expanded env fenv)))
         ;; LAMBDA - create closure
         ((if (symbolp op) (op=lambda op) nil)
          (let* ((params (cadr expr))
                 (body (caddr expr))
                 (free-vars (h0-find-free-vars body params env))
                 (free-offsets (h0-get-free-offsets free-vars env))
                 (param-env (h0-make-param-env params free-vars))
                 (body-ir (h0-compile body param-env fenv)))
            (ir-lambda params body-ir free-vars free-offsets)))
         ;; FUNCALL - call function value
         ((if (symbolp op) (op=funcall op) nil)
          (let* ((fn-ir (h0-compile (cadr expr) env fenv))
                 (args (cddr expr))
                 (args-ir (h0-compile-args args env fenv)))
            (ir-funcall fn-ir args-ir)))
         ;; FLET - local function definitions (non-recursive)
         ((if (symbolp op) (op=flet op) nil)
          (h0-compile-flet (cadr expr) (cddr expr) env fenv))
         ;; LABELS - local function definitions (recursive)
         ((if (symbolp op) (op=labels op) nil)
          (h0-compile-labels (cadr expr) (cddr expr) env fenv))
         ;; MAPCAR - expand to labels loop
         ((if (symbolp op) (op=mapcar op) nil)
          (h0-compile-mapcar (cadr expr) (caddr expr) env fenv))
         ;; REVERSE - expand to labels loop
         ((if (symbolp op) (op=reverse op) nil)
          (h0-compile-reverse (cadr expr) env fenv))
         ;; LIST - expand to nested CONS
         ((if (symbolp op) (op=list op) nil)
          (h0-compile-list (cdr expr) env fenv))
         ;; CADR - (car (cdr x))
         ((if (symbolp op) (op=cadr op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-car (ir-cdr v))))
         ;; CDDR - (cdr (cdr x))
         ((if (symbolp op) (op=cddr op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-cdr (ir-cdr v))))
         ;; CADDR - (car (cdr (cdr x)))
         ((if (symbolp op) (op=caddr op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-car (ir-cdr (ir-cdr v)))))
         ;; CADDDR - (car (cdr (cdr (cdr x))))
         ((if (symbolp op) (op=cadddr op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-car (ir-cdr (ir-cdr (ir-cdr v))))))
         ;; CAAR - (car (car x))
         ((if (symbolp op) (op=caar op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-car (ir-car v))))
         ;; CDAR - (cdr (car x))
         ((if (symbolp op) (op=cdar op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-cdr (ir-car v))))
         ;; NTH - expand to nested CDRs and CAR
         ((if (symbolp op) (op=nth op) nil)
          (h0-compile-nth (cadr expr) (caddr expr) env fenv))
         ;; LOGNOT - use MVN instruction (bitwise NOT)
         ((if (symbolp op) (op=lognot op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-bnot v)))
         ;; EQL - equal for numbers and symbols
         ((if (symbolp op) (op=eql op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-eql l r)))
         ;; GET-TAG - extract tag from tagged value
         ((if (symbolp op) (op=get-tag op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-get-tag v)))
         ;; SET-TAG - change tag bits on a pointer value
         ((if (symbolp op) (op=set-tag op) nil)
          (let ((val (h0-compile (cadr expr) env fenv))
                (new-tag (h0-compile (caddr expr) env fenv)))
            (ir-set-tag val new-tag)))
         ;; MAKE-STRING-FROM-VECTOR - create string from vector of chars
         ((if (symbolp op) (op=make-string-from-vector op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-make-string-from-vector v)))
         ;; MAKE-SYMBOL-FROM-STRING - create symbol from string
         ((if (symbolp op) (op=make-symbol-from-string op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (ir-make-symbol-from-string v)))
         ;; ERROR - signal error and crash
         ((if (symbolp op) (op=error op) nil)
          (ir-error))
         ;; /= (not equal)
         ((if (symbolp op) (op=neq op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (ir-not (ir-cmp-eq l r))))
         ;; Default case - if op is a cons (e.g., lambda expression), compile as funcall
         ;; otherwise it's an unknown operator
         (t
          (if (consp op)
              (let* ((fn-ir (h0-compile op env fenv))
                     (args (cdr expr))
                     (args-ir (h0-compile-args args env fenv)))
                (ir-funcall fn-ir args-ir))
              (fatal-error-ir "h0-compile: Unknown operator"))))))
    ;; Default - unknown expression type - CRASH
    (t (fatal-error-ir "h0-compile: Unknown expression type"))))

;; Compile addition with constant folding (handles variadic args)
(defun h0-compile-add (args env fenv)
  (if (null args)
      (fatal-error-ir "h0-compile-add: Empty addition")
      (if (null (cdr args))
          ;; Single argument: just compile it
          (h0-compile (car args) env fenv)
          (if (null (cddr args))
              ;; Two arguments: normal binary add
              (let* ((left-ir (h0-compile (car args) env fenv))
                     (right-ir (h0-compile (cadr args) env fenv)))
                ;; Constant folding
                (if (and (ir-lit-p left-ir) (ir-lit-p right-ir))
                    (ir-lit (+ (ir-lit-value left-ir) (ir-lit-value right-ir)))
                    (ir-add left-ir right-ir)))
              ;; More than two: recurse - (+ a b c ...) => (+ a (+ b c ...))
              (let* ((left-ir (h0-compile (car args) env fenv))
                     (right-ir (h0-compile-add (cdr args) env fenv)))
                (if (and (ir-lit-p left-ir) (ir-lit-p right-ir))
                    (ir-lit (+ (ir-lit-value left-ir) (ir-lit-value right-ir)))
                    (ir-add left-ir right-ir)))))))

;; Compile subtraction with constant folding (handles variadic args)
(defun h0-compile-sub (args env fenv)
  (if (null args)
      (fatal-error-ir "h0-compile-sub: Empty subtraction")
      (if (null (cdr args))
          ;; Unary minus
          (let ((arg-ir (h0-compile (car args) env fenv)))
            (if (ir-lit-p arg-ir)
                (ir-lit (- #x0 (ir-lit-value arg-ir)))
                (ir-sub (ir-lit #x0) arg-ir)))
          (if (null (cddr args))
              ;; Two arguments: normal binary sub
              (let* ((left-ir (h0-compile (car args) env fenv))
                     (right-ir (h0-compile (cadr args) env fenv)))
                ;; Constant folding
                (if (and (ir-lit-p left-ir) (ir-lit-p right-ir))
                    (ir-lit (- (ir-lit-value left-ir) (ir-lit-value right-ir)))
                    (ir-sub left-ir right-ir)))
              ;; More than two: (- a b c ...) => (- (- a b) c ...)
              ;; Note: subtraction is left-associative unlike addition
              (h0-compile-sub (cons (list '- (car args) (cadr args)) (cddr args)) env fenv)))))

;; Compile let - iterate through bindings, extending environment
;; body-forms is a list of forms (implicit progn)
;; Store symbol name (string) in env for string-based lookup
(defun h0-compile-let (bindings body-forms env fenv)
  (if (null bindings)
      ;; No more bindings - compile body as implicit progn
      (if (null (cdr body-forms))
          (h0-compile (car body-forms) env fenv)
          (h0-compile-progn body-forms env fenv))
      (let* ((b (car bindings))
             (var-sym (car b))
             (val-ir (h0-compile (cadr b) env fenv))
             ;; Store symbol for flat list lookup
             (new-env (cons var-sym env))
             (body-ir (h0-compile-let (cdr bindings) body-forms new-env fenv)))
        (ir-let #x0 val-ir body-ir))))

;; Compile progn - sequence of forms
(defun h0-compile-progn (forms env fenv)
  (if (null forms)
      (ir-lit #x0)
      (if (null (cdr forms))
          (h0-compile (car forms) env fenv)
          (let* ((first-ir (h0-compile (car forms) env fenv))
                 (rest-ir (h0-compile-progn-rest (cdr forms) env fenv)))
            (ir-progn (cons first-ir rest-ir))))))

(defun h0-compile-progn-rest (forms env fenv)
  (if (null forms)
      nil
      (let* ((first-ir (h0-compile (car forms) env fenv))
             (rest-ir (h0-compile-progn-rest (cdr forms) env fenv)))
        (cons first-ir rest-ir))))

;; Expand CASE clauses to COND clauses
;; Each clause (key body...) becomes ((eql key-var 'key) body...)
;; Multiple keys ((k1 k2) body...) becomes ((or (eql key-var 'k1) (eql key-var 'k2)) body...)
;; Default clause (t body...) stays as (t body...)
(defun h0-expand-case-clauses (key-var clauses)
  (if (null clauses)
      nil
      (let* ((clause (car clauses))
             (keys (car clause))
             (body (cdr clause))
             (rest (h0-expand-case-clauses key-var (cdr clauses))))
        (cond
          ;; Default clause: t or otherwise
          ((or (eq keys 't) (eq keys 'T) (eq keys 'otherwise))
           (cons (cons 't body) rest))
          ;; Single key (not a list)
          ((not (consp keys))
           (cons (cons (list 'eql key-var (list 'quote keys)) body) rest))
          ;; Multiple keys (list)
          (t
           (cons (cons (cons 'or (h0-expand-case-keys key-var keys)) body) rest))))))

(defun h0-expand-case-keys (key-var keys)
  "Expand list of keys to (eql key-var 'k1) (eql key-var 'k2) ..."
  (if (null keys)
      nil
      (cons (list 'eql key-var (list 'quote (car keys)))
            (h0-expand-case-keys key-var (cdr keys)))))

;; Compile OR - expand to if chain: (or a b c) => (if a a (if b b c))
;; Returns the first true value or nil
(defun h0-compile-or (args env fenv)
  (if (null args)
      ;; No arguments - return nil
      (ir-lit #x0)
      (if (null (cdr args))
          ;; Single argument - just compile it
          (h0-compile (car args) env fenv)
          ;; Multiple arguments - (if a a (or b c...))
          ;; To avoid evaluating a twice, we expand to: (let ((tmp a)) (if tmp tmp (or b c...)))
          (let* ((first-ir (h0-compile (car args) env fenv))
                 (rest-ir (h0-compile-or (cdr args) env fenv))
                 ;; Create a temp variable for the first argument
                 (temp-sym (make-symbol "OR-TMP"))
                 (temp-env (cons temp-sym env))
                 (temp-ref (ir-var #x0))
                 (if-ir (ir-if temp-ref temp-ref rest-ir)))
            (ir-let #x0 first-ir if-ir)))))

;; Compile AND - expand to if chain: (and a b c) => (if a (if b c nil) nil)
;; Returns the last value if all are true, nil otherwise
(defun h0-compile-and (args env fenv)
  (if (null args)
      ;; No arguments - return t
      (ir-lit #x1)
      (if (null (cdr args))
          ;; Single argument - just compile it
          (h0-compile (car args) env fenv)
          ;; Multiple arguments - (if a (and b c...) nil)
          (let* ((first-ir (h0-compile (car args) env fenv))
                 (rest-ir (h0-compile-and (cdr args) env fenv))
                 (else-ir (ir-lit #x0)))
            (ir-if first-ir rest-ir else-ir)))))

;; Compile cond - expand to nested IFs
;; (cond (c1 e1...) (c2 e2...) (t e3...))
;; => (if c1 (progn e1...) (if c2 (progn e2...) (progn e3...)))
(defun h0-compile-cond (clauses env fenv)
  (if (null clauses)
      ;; No clauses - return nil
      (ir-lit #x0)
      (let* ((clause (car clauses))
             (test (car clause))
             (body (cdr clause)))
        (if (null (cdr clauses))
            ;; Last clause - just compile it
            (if (eq test 'T)
                ;; (t body...) - always true, just execute body
                (if (null body)
                    (ir-lit #x0)
                    (if (null (cdr body))
                        (h0-compile (car body) env fenv)
                        (h0-compile-progn body env fenv)))
                ;; Last clause with non-t test - normal if
                (let* ((test-ir (h0-compile test env fenv))
                       (body-ir (if (null body)
                                    (ir-lit #x0)
                                    (if (null (cdr body))
                                        (h0-compile (car body) env fenv)
                                        (h0-compile-progn body env fenv))))
                       (else-ir (ir-lit #x0)))
                  (ir-if test-ir body-ir else-ir)))
            ;; Multiple clauses - nested if
            (if (eq test 'T)
                ;; (t body...) in middle - execute body (subsequent clauses ignored)
                (if (null body)
                    (ir-lit #x0)
                    (if (null (cdr body))
                        (h0-compile (car body) env fenv)
                        (h0-compile-progn body env fenv)))
                ;; Normal clause - if with nested cond for else
                (let* ((test-ir (h0-compile test env fenv))
                       (then-ir (if (null body)
                                    (ir-lit #x0)
                                    (if (null (cdr body))
                                        (h0-compile (car body) env fenv)
                                        (h0-compile-progn body env fenv))))
                       (else-ir (h0-compile-cond (cdr clauses) env fenv)))
                  (ir-if test-ir then-ir else-ir)))))))

;; Compile case - expand to nested IFs with EQ comparisons
;; (case keyform (key1 e1...) (key2 e2...) (otherwise e3...))
;; => (let ((#:key keyform))
;;      (if (eq #:key 'key1) (progn e1...)
;;        (if (eq #:key 'key2) (progn e2...)
;;          (progn e3...))))
(defun h0-compile-case (keyform clauses env fenv)
  (let* ((key-ir (h0-compile keyform env fenv))
         ;; Create a temporary binding for the key
         (temp-sym (make-symbol "#:CASE-KEY"))
         (new-env (cons temp-sym env)))
    ;; Compile the case clauses with the key in environment
    (ir-let #x0 key-ir
          (h0-compile-case-clauses clauses new-env fenv))))

(defun h0-compile-case-clauses (clauses env fenv)
  (if (null clauses)
      ;; No clauses - return nil
      (ir-lit #x0)
      (let* ((clause (car clauses))
             (keys (car clause))
             (body (cdr clause)))
        (if (or (eq keys 'OTHERWISE) (eq keys 'T))
            ;; Default clause - just execute body
            (if (null body)
                (ir-lit #x0)
                (if (null (cdr body))
                    (h0-compile (car body) env fenv)
                    (h0-compile-progn body env fenv)))
            ;; Normal clause - compare key(s)
            (let* ((test-ir (h0-compile-case-test keys env fenv))
                   (then-ir (if (null body)
                                (ir-lit #x0)
                                (if (null (cdr body))
                                    (h0-compile (car body) env fenv)
                                    (h0-compile-progn body env fenv))))
                   (else-ir (h0-compile-case-clauses (cdr clauses) env fenv)))
              (ir-if test-ir then-ir else-ir))))))

(defun h0-compile-case-test (keys env fenv)
  ;; Get the temporary key variable from environment
  ;; Use sym-eq (symbol name comparison) instead of eq (pointer comparison)
  ;; because habu0 creates new symbol objects at runtime, so symbols with
  ;; the same name may not be eq.
  (let ((key-var-ir (ir-var #x0)))
    (if (consp keys)
        ;; Multiple keys - (or (sym-eq key k1) (sym-eq key k2) ...)
        (h0-compile-case-test-list keys key-var-ir env fenv)
        ;; Single key - (sym-eq key k)
        (let ((key-lit-ir (h0-compile (list 'quote keys) env fenv)))
          (ir-sym-eq key-var-ir key-lit-ir)))))

(defun h0-compile-case-test-list (keys key-var-ir env fenv)
  (if (null keys)
      ;; Should not happen, but return false
      (ir-lit #x0)
      (if (null (cdr keys))
          ;; Single key left
          (let ((key-lit-ir (h0-compile (list 'quote (car keys)) env fenv)))
            (ir-sym-eq key-var-ir key-lit-ir))
          ;; Multiple keys - (or (sym-eq key k1) (rest...))
          (let* ((key-lit-ir (h0-compile (list 'quote (car keys)) env fenv))
                 (test-ir (ir-sym-eq key-var-ir key-lit-ir))
                 (rest-ir (h0-compile-case-test-list (cdr keys) key-var-ir env fenv)))
            ;; (if test t rest) - implements OR
            (ir-if test-ir (ir-lit #x1) rest-ir)))))

;; Compile FLET - local function definitions (non-recursive)
;; Transform: (flet ((f (x) body)) form) => (let ((f (lambda (x) body))) form)
(defun h0-compile-flet (bindings body env fenv)
  (h0-compile-flet-to-let bindings body env fenv))

(defun h0-compile-flet-to-let (bindings body env fenv)
  (if (null bindings)
      (h0-compile-progn body env fenv)
      (let* ((let-bindings (h0-flet-bindings-to-let bindings)))
        (h0-compile-let let-bindings body env fenv))))

(defun h0-flet-bindings-to-let (bindings)
  (if (null bindings)
      nil
      (let* ((binding (car bindings))
             (fname (car binding))
             (params (cadr binding))
             (fbody (caddr binding))
             (lambda-expr (list *op-lambda* params fbody))
             (let-binding (list fname lambda-expr)))
        (cons let-binding (h0-flet-bindings-to-let (cdr bindings))))))

;; Compile LABELS - local function definitions (recursive)
;; Transform: (labels ((f (x) body)) form) => (let ((f nil)) (setq f (lambda (x) body)) form)
(defun h0-compile-labels (bindings body env fenv)
  (h0-compile-labels-to-let-setq bindings body env fenv))

(defun h0-compile-labels-to-let-setq (bindings body env fenv)
  (if (null bindings)
      (h0-compile-progn body env fenv)
      (let* ((let-bindings (h0-labels-nil-bindings bindings))
             (setq-forms (h0-labels-setq-forms bindings))
             (combined-body (h0-append setq-forms body)))
        ;; combined-body is already a list of forms - pass directly
        (h0-compile-let let-bindings combined-body env fenv))))

(defun h0-labels-nil-bindings (bindings)
  (if (null bindings)
      nil
      (let* ((binding (car bindings))
             (fname (car binding))
             (nil-binding (list fname nil)))
        (cons nil-binding (h0-labels-nil-bindings (cdr bindings))))))

(defun h0-labels-setq-forms (bindings)
  (if (null bindings)
      nil
      (let* ((binding (car bindings))
             (fname (car binding))
             (params (cadr binding))
             (fbody (caddr binding))
             (lambda-expr (list *op-lambda* params fbody))
             (setq-form (list *op-setq* fname lambda-expr)))
        (cons setq-form (h0-labels-setq-forms (cdr bindings))))))

(defun h0-append (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (h0-append (cdr list1) list2))))

;; Compile WHILE - generate while-ir directly for proper loop codegen
;; (while test body...) => (while-ir test-ir body-ir)
;; This generates a true iterative loop without function calls.
;; Previously expanded to labels, but that broke TCO because labels uses funcall.
(defun h0-compile-while (test body env fenv)
  (let ((test-ir (h0-compile test env fenv))
        (body-ir (if (null (cdr body))
                     (h0-compile (car body) env fenv)
                     (h0-compile (cons 'progn body) env fenv))))
    (list 'while-ir test-ir body-ir)))

;; Compile MAPCAR - generate dolist-style IR directly
;; (mapcar fn list) => iterate, cons results, reverse at end
;; Uses while-ir to avoid labels which breaks TCO
;; IMPORTANT: Use *op-* symbols (habu symbols), not (intern ...) (SBCL symbols)
(defun h0-compile-mapcar (fn-expr list-expr env fenv)
  (let* ((fn-var (make-habu-symbol-form "FN-TEMP"))
         (l-var (make-habu-symbol-form "L-TEMP"))
         (acc-var (make-habu-symbol-form "ACC-TEMP"))
         (expanded
          (list *op-let*
                (list (list fn-var fn-expr)
                      (list l-var list-expr)
                      (list acc-var nil))
                (list *op-progn*
                      (list *op-while* l-var
                            (list *op-setq* acc-var
                                  (list *op-cons*
                                        (list *op-funcall* fn-var
                                              (list *op-car* l-var))
                                        acc-var))
                            (list *op-setq* l-var
                                  (list *op-cdr* l-var)))
                      (list *op-reverse* acc-var)))))
    (h0-compile expanded env fenv)))

;; Compile REVERSE - use while-ir directly
;; (reverse list) => (let ((l list) (acc nil)) (while l (setq acc (cons (car l) acc)) (setq l (cdr l))) acc)
;; IMPORTANT: Use *op-* symbols (habu symbols), not (intern ...) (SBCL symbols)
(defun h0-compile-reverse (list-expr env fenv)
  (let* ((l-var (make-habu-symbol-form "L-TEMP"))
         (acc-var (make-habu-symbol-form "ACC-TEMP"))
         (expanded
          (list *op-let*
                (list (list l-var list-expr)
                      (list acc-var nil))
                (list *op-progn*
                      (list *op-while* l-var
                            (list *op-setq* acc-var
                                  (list *op-cons*
                                        (list *op-car* l-var)
                                        acc-var))
                            (list *op-setq* l-var
                                  (list *op-cdr* l-var)))
                      acc-var))))
    (h0-compile expanded env fenv)))

;; Compile LIST - expand to nested CONS
;; (list a b c) => (cons a (cons b (cons c nil)))
(defun h0-compile-list (args env fenv)
  (if (null args)
      (ir-lit #x0)  ;; Empty list = nil
      (let* ((first-ir (h0-compile (car args) env fenv))
             (rest-ir (h0-compile-list (cdr args) env fenv)))
        (ir-cons first-ir rest-ir))))

;; Compile NTH - expand based on index
;; (nth n list) - if n is a constant, expand to nested car/cdr
;; Otherwise expand to a labels loop
(defun h0-compile-nth (n-expr list-expr env fenv)
  (if (numberp n-expr)
      ;; Constant index - expand to nested car/cdr
      (h0-compile-nth-const n-expr list-expr env fenv)
      ;; Variable index - expand to loop
      (h0-compile-nth-var n-expr list-expr env fenv)))

(defun h0-compile-nth-const (n list-expr env fenv)
  (let ((list-ir (h0-compile list-expr env fenv)))
    (h0-nth-chain n list-ir)))

(defun h0-nth-chain (n list-ir)
  (if (= n #x0)
      (ir-car list-ir)
      (h0-nth-chain (- n #x1) (ir-cdr list-ir))))

(defun h0-compile-nth-var (n-expr list-expr env fenv)
  ;; Expand to: (let ((i n) (l list)) (while (> i 0) (setq i (- i 1)) (setq l (cdr l))) (car l))
  (let* ((i-temp (make-habu-symbol-form "I-TEMP"))
         (l-temp (make-habu-symbol-form "L-TEMP"))
         (expanded
          (list *op-let*
                (list (list i-temp n-expr)
                      (list l-temp list-expr))
                (list *op-progn*
                      (list *op-while* (list *op-gt* i-temp 0)
                            (list *op-setq* i-temp
                                  (list *op-minus* i-temp 1))
                            (list *op-setq* l-temp
                                  (list *op-cdr* l-temp)))
                      (list *op-car* l-temp)))))
    (h0-compile expanded env fenv)))

;;; ==========================================================================
;;; Mach-O Linker - Native executable generation with chained fixups
;;; ==========================================================================
;;; Generates standalone ARM64 Mach-O executables that link against libSystem.
;;; Uses chained fixups for dynamic symbol binding (modern macOS approach).

;; File I/O constants for sys-open
(defun o-wronly () #x1)
(defun o-creat () #x200)
(defun o-trunc () #x400)

;; Mach-O magic and CPU types
(defun mh-magic-64 () #xFEEDFACF)
(defun cpu-type-arm64 () #x0100000C)
(defun cpu-subtype-arm64-all () #x0)
(defun mh-execute () #x2)

;; Header flags
(defun mh-noundefs () #x1)
(defun mh-dyldlink () #x4)
(defun mh-twolevel () #x80)
(defun mh-pie () #x200000)

;; Load command types
(defun lc-segment-64 () #x19)
(defun lc-symtab () #x2)
(defun lc-dysymtab () #xB)
(defun lc-load-dylinker () #xE)
(defun lc-uuid () #x1B)
(defun lc-build-version () #x32)
(defun lc-main () #x80000028)
(defun lc-load-dylib () #xC)
(defun lc-dyld-chained-fixups () #x80000034)
(defun lc-dyld-exports-trie () #x80000033)

;; VM protection flags
(defun vm-prot-read () #x1)
(defun vm-prot-write () #x2)
(defun vm-prot-execute () #x4)

;; Section flags
(defun s-attr-pure-instructions () #x80000000)
(defun s-attr-some-instructions () #x400)
(defun s-non-lazy-symbol-pointers () #x6)
(defun s-symbol-stubs () #x8)

;; Chained fixups format
(defun dyld-chained-ptr-64-offset () #x6)

;; Page size and VM base
(defun page-size () #x4000)  ; 16KB on ARM64 macOS
(defun vm-base () #x100000000)

;; Align value up to alignment boundary
(defun align-up (val alignment)
  (let ((rem (mod val alignment)))
    (if (= rem #x0)
        val
        (+ val (- alignment rem)))))

;;; Byte buffer operations
;;; We build the executable in a list of bytes (reversed), then write it out

;; Append a single byte to buffer (returns new buffer)
(defun buf-u8 (buf val)
  (cons (logand val #xFF) buf))

;; Append u16 little-endian
(defun buf-u16-le (buf val)
  (buf-u8 (buf-u8 buf val) (ash val #x-8)))

;; Append u32 little-endian
(defun buf-u32-le (buf val)
  (buf-u16-le (buf-u16-le buf val) (ash val #x-10)))

;; Append u64 little-endian
(defun buf-u64-le (buf val)
  (buf-u32-le (buf-u32-le buf (logand val #xFFFFFFFF))
              (logand (ash val #x-20) #xFFFFFFFF)))

;; Append N zero bytes
(defun buf-zeros (buf n)
  (if (<= n #x0)
      buf
      (buf-zeros (buf-u8 buf #x0) (- n #x1))))

;; Append byte list (each byte is consed to front, so we reverse first)
(defun buf-bytes (buf bytes)
  (if (null bytes)
      buf
      (buf-bytes (buf-u8 buf (car bytes)) (cdr bytes))))

;; Append string as bytes (without null terminator) - iterative
(defun buf-string (buf str)
  (let ((len (string-length str))
        (i 0)
        (b buf))
    (while (< i len)
      (setq b (buf-u8 b (string-ref str i)))
      (setq i (+ i 1)))
    b))

;; Append string padded to length with zeros - iterative
(defun buf-string-padded (buf str len)
  (let* ((slen (string-length str))
         (copylen (if (< slen len) slen len))
         (i 0)
         (b buf))
    ;; Copy string bytes up to copylen
    (while (< i copylen)
      (setq b (buf-u8 b (string-ref str i)))
      (setq i (+ i 1)))
    ;; Pad with zeros
    (buf-zeros b (- len slen))))

;; Get current buffer length
(defun buf-length (buf)
  (length buf))

;; Convert buffer to vector (reverses the list) - iterative
(defun buf-to-vector (buf)
  (let* ((len (length buf))
         (vec (make-vector len))
         (lst (reverse buf))
         (i 0))
    (while (and lst (< i len))
      (vector-set vec i (car lst))
      (setq lst (cdr lst))
      (setq i (+ i 1)))
    vec))

;;; Mach-O structure writers

;; Mach-O header (32 bytes)
(defun buf-mach-header-64 (buf ncmds sizeofcmds flags)
  (let* ((b1 (buf-u32-le buf (mh-magic-64)))
         (b2 (buf-u32-le b1 (cpu-type-arm64)))
         (b3 (buf-u32-le b2 (cpu-subtype-arm64-all)))
         (b4 (buf-u32-le b3 (mh-execute)))
         (b5 (buf-u32-le b4 ncmds))
         (b6 (buf-u32-le b5 sizeofcmds))
         (b7 (buf-u32-le b6 flags))
         (b8 (buf-u32-le b7 #x0)))  ; reserved
    b8))

;; Segment command (72 bytes)
(defun buf-segment-command-64 (buf segname vmaddr vmsize fileoff filesize
                                maxprot initprot nsects flags)
  (let* ((b1 (buf-u32-le buf (lc-segment-64)))
         (cmdsize (+ #x48 (* nsects #x50)))  ; 72 + 80*nsects
         (b2 (buf-u32-le b1 cmdsize))
         (b3 (buf-string-padded b2 segname #x10))
         (b4 (buf-u64-le b3 vmaddr))
         (b5 (buf-u64-le b4 vmsize))
         (b6 (buf-u64-le b5 fileoff))
         (b7 (buf-u64-le b6 filesize))
         (b8 (buf-u32-le b7 maxprot))
         (b9 (buf-u32-le b8 initprot))
         (b10 (buf-u32-le b9 nsects))
         (b11 (buf-u32-le b10 flags)))
    b11))

;; Section (80 bytes)
(defun buf-section-64 (buf sectname segname addr size offset align
                        reloff nreloc flags reserved1 reserved2)
  (let* ((b1 (buf-string-padded buf sectname #x10))
         (b2 (buf-string-padded b1 segname #x10))
         (b3 (buf-u64-le b2 addr))
         (b4 (buf-u64-le b3 size))
         (b5 (buf-u32-le b4 offset))
         (b6 (buf-u32-le b5 align))
         (b7 (buf-u32-le b6 reloff))
         (b8 (buf-u32-le b7 nreloc))
         (b9 (buf-u32-le b8 flags))
         (b10 (buf-u32-le b9 reserved1))
         (b11 (buf-u32-le b10 reserved2))
         (b12 (buf-u32-le b11 #x0)))  ; reserved3
    b12))

;; LC_LOAD_DYLINKER command
(defun buf-dylinker-command (buf path)
  (let* ((path-len (+ (string-length path) #x1))
         (cmdsize (align-up (+ #xC path-len) #x8))
         (b1 (buf-u32-le buf (lc-load-dylinker)))
         (b2 (buf-u32-le b1 cmdsize))
         (b3 (buf-u32-le b2 #xC))  ; path.offset
         (b4 (buf-string-padded b3 path (- cmdsize #xC))))
    b4))

;; LC_UUID command (24 bytes)
(defun buf-uuid-command (buf)
  (let* ((b1 (buf-u32-le buf (lc-uuid)))
         (b2 (buf-u32-le b1 #x18))
         ;; Generate simple UUID
         (b3 (buf-u32-le b2 #xDEADBEEF))
         (b4 (buf-u32-le b3 #xCAFEBABE))
         (b5 (buf-u32-le b4 #x12345678))
         (b6 (buf-u32-le b5 #x87654321)))
    b6))

;; LC_BUILD_VERSION command (24 bytes)
(defun buf-build-version-command (buf)
  (let* ((b1 (buf-u32-le buf (lc-build-version)))
         (b2 (buf-u32-le b1 #x18))
         (b3 (buf-u32-le b2 #x1))       ; platform = macOS
         (b4 (buf-u32-le b3 #xE0000))   ; minos = 14.0
         (b5 (buf-u32-le b4 #xE0000))   ; sdk = 14.0
         (b6 (buf-u32-le b5 #x0)))      ; ntools
    b6))

;; LC_MAIN command (24 bytes)
(defun buf-main-command (buf entryoff)
  (let* ((b1 (buf-u32-le buf (lc-main)))
         (b2 (buf-u32-le b1 #x18))
         (b3 (buf-u64-le b2 entryoff))
         (b4 (buf-u64-le b3 #x0)))      ; stacksize = 0 (default)
    b4))

;; LC_LOAD_DYLIB command
(defun buf-load-dylib-command (buf path)
  (let* ((path-len (+ (string-length path) #x1))
         (cmdsize (align-up (+ #x18 path-len) #x8))
         (b1 (buf-u32-le buf (lc-load-dylib)))
         (b2 (buf-u32-le b1 cmdsize))
         (b3 (buf-u32-le b2 #x18))      ; name.offset
         (b4 (buf-u32-le b3 #x2))       ; timestamp
         (b5 (buf-u32-le b4 #x54C0000)) ; current_version
         (b6 (buf-u32-le b5 #x10000))   ; compatibility_version
         (b7 (buf-string-padded b6 path (- cmdsize #x18))))
    b7))

;; LC_SYMTAB command (24 bytes)
(defun buf-symtab-command (buf symoff nsyms stroff strsize)
  (let* ((b1 (buf-u32-le buf (lc-symtab)))
         (b2 (buf-u32-le b1 #x18))
         (b3 (buf-u32-le b2 symoff))
         (b4 (buf-u32-le b3 nsyms))
         (b5 (buf-u32-le b4 stroff))
         (b6 (buf-u32-le b5 strsize)))
    b6))

;; LC_DYSYMTAB command (80 bytes)
(defun buf-dysymtab-command (buf ilocalsym nlocalsym iextdefsym nextdefsym
                              iundefsym nundefsym indirectsymoff nindirectsyms)
  (let* ((b1 (buf-u32-le buf (lc-dysymtab)))
         (b2 (buf-u32-le b1 #x50))       ; cmdsize = 80
         (b3 (buf-u32-le b2 ilocalsym))
         (b4 (buf-u32-le b3 nlocalsym))
         (b5 (buf-u32-le b4 iextdefsym))
         (b6 (buf-u32-le b5 nextdefsym))
         (b7 (buf-u32-le b6 iundefsym))
         (b8 (buf-u32-le b7 nundefsym))
         (b9 (buf-u32-le b8 #x0))        ; tocoff
         (b10 (buf-u32-le b9 #x0))       ; ntoc
         (b11 (buf-u32-le b10 #x0))      ; modtaboff
         (b12 (buf-u32-le b11 #x0))      ; nmodtab
         (b13 (buf-u32-le b12 #x0))      ; extrefsymoff
         (b14 (buf-u32-le b13 #x0))      ; nextrefsyms
         (b15 (buf-u32-le b14 indirectsymoff))
         (b16 (buf-u32-le b15 nindirectsyms))
         (b17 (buf-u32-le b16 #x0))      ; extreloff
         (b18 (buf-u32-le b17 #x0))      ; nextrel
         (b19 (buf-u32-le b18 #x0))      ; locreloff
         (b20 (buf-u32-le b19 #x0)))     ; nlocrel
    b20))

;; LC_DYLD_CHAINED_FIXUPS command (16 bytes)
(defun buf-chained-fixups-command (buf dataoff datasize)
  (let* ((b1 (buf-u32-le buf (lc-dyld-chained-fixups)))
         (b2 (buf-u32-le b1 #x10))
         (b3 (buf-u32-le b2 dataoff))
         (b4 (buf-u32-le b3 datasize)))
    b4))

;; LC_DYLD_EXPORTS_TRIE command (16 bytes)
(defun buf-exports-trie-command (buf dataoff datasize)
  (let* ((b1 (buf-u32-le buf (lc-dyld-exports-trie)))
         (b2 (buf-u32-le b1 #x10))
         (b3 (buf-u32-le b2 dataoff))
         (b4 (buf-u32-le b3 datasize)))
    b4))

;; nlist_64 symbol entry (16 bytes)
(defun buf-nlist-64 (buf strx type sect desc value)
  (let* ((b1 (buf-u32-le buf strx))
         (b2 (buf-u8 b1 type))
         (b3 (buf-u8 b2 sect))
         (b4 (buf-u16-le b3 desc))
         (b5 (buf-u64-le b4 value)))
    b5))

;;; Stub code generation (ADRP + LDR + BR)

;; ADRP Xd, #page_offset
(defun macho-adrp (rd page-off)
  (let* ((immlo (logand page-off #x3))
         (immhi (logand (ash page-off #x-2) #x7FFFF))
         (inst (logior #x90000000
                       (ash immlo #x1D)
                       (ash immhi #x5)
                       rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LDR Xt, [Xn, #imm]
(defun macho-ldr (rt rn imm)
  (let* ((offset (ash imm #x-3))
         (inst (logior #xF9400000
                       (ash (logand offset #xFFF) #xA)
                       (ash rn #x5)
                       rt)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; BR Xn
(defun macho-br (rn)
  (let ((inst (logior #xD61F0000 (ash rn #x5))))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;;; Byte list utilities for linker

;; Append two byte lists
(defun bytes-append (a b)
  (if (null a) b
      (cons (car a) (bytes-append (cdr a) b))))

;; Append multiple byte lists - O(n) using accumulator
;; Strategy: reverse the input lists, accumulate in reverse, then reverse final result
(defun bytes-append-all (lists)
  (reverse (bytes-append-all-helper (reverse lists) nil)))

(defun bytes-append-all-helper (lists acc)
  (if (null lists)
      acc
      (bytes-append-all-helper (cdr lists)
                               (bytes-append-reversed (car lists) acc))))

;; Append list a to b, with a reversed during the append
(defun bytes-append-reversed (a b)
  (if (null a)
      b
      (bytes-append-reversed (cdr a) (cons (car a) b))))

;; Generate stub: ADRP x16, got_page; LDR x16, [x16, #offset]; BR x16
(defun generate-stub (got-page-diff got-slot-offset)
  (bytes-append-all
   (list (macho-adrp #x10 got-page-diff)
         (macho-ldr #x10 #x10 got-slot-offset)
         (macho-br #x10))))

;;; Chained fixups data builder

;; Build import symbol string table (NUL-separated)
(defun build-import-strings (imports)
  (if (null imports)
      (list #x0)  ; Just leading NUL
      (cons #x0 (build-import-strings-helper imports))))

;; O(n) - accumulate in reverse then reverse at end
(defun build-import-strings-helper (imports)
  (reverse (build-import-strings-acc imports nil)))

(defun build-import-strings-acc (imports acc)
  (if (null imports)
      acc
      (let* ((name (car imports))
             (bytes (string-to-bytes name))
             ;; Push NUL separator then all bytes in reverse
             (acc-with-nul (cons #x0 acc))
             (acc-with-bytes (push-bytes-reversed bytes acc-with-nul)))
        (build-import-strings-acc (cdr imports) acc-with-bytes))))

;; Push all bytes from list onto acc in reversed order - iterative
(defun push-bytes-reversed (bytes acc)
  (let ((b bytes)
        (a acc))
    (while b
      (setq a (cons (car b) a))
      (setq b (cdr b)))
    a))

;; Convert string to list of bytes - iterative
(defun string-to-bytes (str)
  (let ((len (string-length str))
        (i 0)
        (acc nil))
    (while (< i len)
      (setq acc (cons (string-ref str i) acc))
      (setq i (+ i 1)))
    (reverse acc)))

;; Build chained fixups data blob
(defun build-chained-fixups-data (num-imports num-segments got-segment-index got-vm-offset)
  "Build chained fixups data for binding external symbols.
   Returns a byte list."
  (let* (;; Calculate sizes
         (header-size #x20)              ; 32 bytes
         (starts-header-size (+ #x4 (* #x4 num-segments)))
         (seg-info-rel-offset (align-up starts-header-size #x8))
         (seg-info-size #x18)            ; 24 bytes
         (imports-entry-size #x4)
         (starts-offset header-size)
         (imports-offset (+ starts-offset seg-info-rel-offset seg-info-size))
         (symbols-offset (+ imports-offset (* num-imports imports-entry-size)))
         ;; Build the data
         (data nil))

    ;; === dyld_chained_fixups_header (32 bytes) ===
    (setq data (buf-u32-le data #x0))             ; fixups_version = 0
    (setq data (buf-u32-le data starts-offset))   ; starts_offset
    (setq data (buf-u32-le data imports-offset))  ; imports_offset
    (setq data (buf-u32-le data symbols-offset))  ; symbols_offset
    (setq data (buf-u32-le data num-imports))     ; imports_count
    (setq data (buf-u32-le data #x1))             ; imports_format = DYLD_CHAINED_IMPORT
    (setq data (buf-u32-le data #x0))             ; symbols_format = uncompressed
    (setq data (buf-u32-le data #x0))             ; padding

    ;; === dyld_chained_starts_in_image ===
    (setq data (buf-u32-le data num-segments))    ; seg_count
    ;; seg_info_offset for each segment (only GOT segment has non-zero)
    (setq data (build-seg-offsets data #x0 num-segments got-segment-index seg-info-rel-offset))

    ;; Padding to align seg_info
    (let ((current-size (buf-length data)))
      (setq data (buf-zeros data (- (+ starts-offset seg-info-rel-offset) current-size))))

    ;; === dyld_chained_starts_in_segment (24 bytes) ===
    (setq data (buf-u32-le data #x18))            ; size = 24
    (setq data (buf-u16-le data #x4000))          ; page_size
    (setq data (buf-u16-le data (dyld-chained-ptr-64-offset)))  ; pointer_format
    (setq data (buf-u64-le data got-vm-offset))   ; segment_offset
    (setq data (buf-u32-le data #x0))             ; max_valid_pointer
    (setq data (buf-u16-le data #x1))             ; page_count = 1
    (setq data (buf-u16-le data #x0))             ; page_start[0] = 0

    ;; === Import entries (DYLD_CHAINED_IMPORT format, 4 bytes each) ===
    ;; lib_ordinal (8 bits) | weak (1 bit) | name_offset (23 bits)
    (setq data (build-import-entries data num-imports #x0 #x0))

    ;; === Symbol strings ===
    ;; Note: We don't have the actual import names here, so we'll add placeholder
    ;; The caller must ensure symbols are added separately
    (setq data (buf-u8 data #x0))                 ; Leading NUL

    (reverse data)))

(defun build-seg-offsets (buf i count got-idx offset)
  (if (>= i count)
      buf
      (if (= i got-idx)
          (build-seg-offsets (buf-u32-le buf offset) (+ i #x1) count got-idx offset)
          (build-seg-offsets (buf-u32-le buf #x0) (+ i #x1) count got-idx offset))))

(defun build-import-entries (buf count name-offset i)
  (if (>= i count)
      buf
      (let* (;; lib_ordinal = 1 (first LC_LOAD_DYLIB = libSystem)
             ;; weak = 0
             ;; name_offset at bits 9-31
             (entry (logior #x1 (ash (+ name-offset #x1) #x9))))
        ;; Each import name is at offset 1 + accumulated name length
        ;; For simplicity, assume all names are "_write" (6 chars + NUL = 7 bytes)
        (build-import-entries (buf-u32-le buf entry)
                              count
                              (+ name-offset #x7)  ; Approximate name length
                              (+ i #x1)))))

;;; Wrapper stub for heap initialization

;; Wrap bytecode with heap setup stub (80 bytes = 20 instructions)
;; Stack layout (512 bytes total):
;;   sp+0:   saved x30
;;   sp+8:   saved x28
;;   sp+16:  saved x26
;;   sp+24:  saved x27
;;   sp+32:  saved x20
;;   sp+40:  (padding)
;;   sp+48:  temp slots for h0-codegen (td=0, td=1, ...)
;;   sp+64:  environment base (x20 points here)
(defun wrap-with-heap-stub (code-bytes heap-page-offset)
  "Wrap bytecode with heap initialization for executables with imports.
   heap-page-offset is the page offset from ADRP to __DATA segment."
  ;; Pre-compute all instructions to avoid function-calls-in-list crash
  (let* ((i1 (arm64:sub :sp :sp 512 :imm t))             ; sub sp, sp, #512
         (i2 (arm64:str :x30 :sp :offset 0))             ; str x30, [sp]
         (i3 (arm64:str :x28 :sp :offset 8))             ; str x28, [sp, #8]
         (i4 (arm64:str :x26 :sp :offset 16))            ; str x26, [sp, #16]
         (i5 (arm64:str :x27 :sp :offset 24)))           ; str x27, [sp, #24]
    (let* ((i6 (arm64:str :x20 :sp :offset 32))          ; str x20, [sp, #32]
           (i7 (arm64:add :x20 :sp 64 :imm t))           ; add x20, sp, #64
           (i8 (macho-adrp 28 heap-page-offset))         ; adrp x28, heap_page
           (i9 (arm64:mov :x27 :x28))                    ; mov x27, x28
           (i10 (arm64:add :x28 :x28 16 :imm t)))        ; add x28, x28, #16
      (let* ((i11 (macho-adr 26 40))                     ; adr x26, +40
             (i12 (macho-bl 9))                          ; bl +9
             (i13 (arm64:lsr :x0 :x0 4 :imm t))          ; lsr x0, x0, #4
             (i14 (arm64:ldr :x20 :sp :offset 32))       ; ldr x20, [sp, #32]
             (i15 (arm64:ldr :x27 :sp :offset 24)))      ; ldr x27, [sp, #24]
        (let* ((i16 (arm64:ldr :x26 :sp :offset 16))     ; ldr x26, [sp, #16]
               (i17 (arm64:ldr :x28 :sp :offset 8))      ; ldr x28, [sp, #8]
               (i18 (arm64:ldr :x30 :sp :offset 0))      ; ldr x30, [sp]
               (i19 (arm64:add :sp :sp 512 :imm t))      ; add sp, sp, #512
               (i20 (arm64:ret)))                        ; ret
          (let ((stub (bytes-append-all
                       (list i1 i2 i3 i4 i5 i6 i7 i8 i9 i10
                             i11 i12 i13 i14 i15 i16 i17 i18 i19 i20))))
            (bytes-append stub code-bytes)))))))

;; ADR Xd, #offset (PC-relative)
(defun macho-adr (rd offset)
  (let* ((immlo (logand offset #x3))
         (immhi (logand (ash offset #x-2) #x7FFFF))
         (inst (logior #x10000000
                       (ash immlo #x1D)
                       (ash immhi #x5)
                       rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; BL #offset (PC-relative call, offset in instructions)
(defun macho-bl (instr-offset)
  (let* ((imm26 (logand instr-offset #x3FFFFFF))
         (inst (logior #x94000000 imm26)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;;; Main linker function (split into smaller helpers to fit in 64 temp slots)

;; Calculate basic layout sizes
(defun calc-sizeofcmds ()
  (+ #x48                                         ; PAGEZERO
     (+ #x48 (* #x2 #x50))                        ; TEXT + 2 sections
     (+ #x48 #x50)                                ; DATA_CONST + 1 section
     (+ #x48 #x50)                                ; DATA + 1 section
     #x48                                         ; LINKEDIT
     #x20                                         ; LC_LOAD_DYLINKER
     #x18                                         ; LC_UUID
     #x18                                         ; LC_BUILD_VERSION
     #x18                                         ; LC_MAIN
     #x38                                         ; LC_LOAD_DYLIB
     #x10                                         ; LC_DYLD_CHAINED_FIXUPS
     #x10                                         ; LC_DYLD_EXPORTS_TRIE
     #x18                                         ; LC_SYMTAB
     #x50))                                       ; LC_DYSYMTAB

;; Calculate code offset
(defun calc-code-offset ()
  (align-up (+ #x20 (calc-sizeofcmds) #x10) #x40))

;; Calculate stubs offset based on code size
(defun calc-stubs-offset (code-size)
  (align-up (+ (calc-code-offset) code-size) #x4))

;; Calculate TEXT segment size
(defun calc-text-vmsize (code-size num-imports)
  (let* ((stubs-offset (calc-stubs-offset code-size))
         (stubs-end (+ stubs-offset (* num-imports #xC))))
    (align-up stubs-end (page-size))))

;; Calculate DATA_CONST vmaddr
(defun calc-data-const-vmaddr (code-size num-imports)
  (+ (vm-base) (calc-text-vmsize code-size num-imports)))

;; Calculate DATA vmaddr
(defun calc-data-vmaddr (code-size num-imports)
  (+ (calc-data-const-vmaddr code-size num-imports) (page-size)))

;; Calculate LINKEDIT fileoff
(defun calc-linkedit-fileoff (code-size num-imports heap-size)
  (let* ((text-vmsize (calc-text-vmsize code-size num-imports))
         (heap-vmsize (align-up heap-size (page-size))))
    (+ text-vmsize (page-size) heap-vmsize)))

;; Calculate symbol table offset
(defun calc-symtab-offset (code-size num-imports heap-size)
  (calc-linkedit-fileoff code-size num-imports heap-size))

;; Calculate string table offset
(defun calc-strtab-offset (code-size num-imports heap-size)
  (+ (calc-symtab-offset code-size num-imports heap-size)
     (* (+ #x1 num-imports) #x10)))

;; Calculate indirect symbol offset
(defun calc-indirect-offset (code-size num-imports heap-size)
  (let* ((strtab-offset (calc-strtab-offset code-size num-imports heap-size))
         (string-table-size (+ #x7 (* num-imports #x8))))
    (align-up (+ strtab-offset string-table-size) #x4)))

;; Calculate fixups offset
(defun calc-fixups-offset (code-size num-imports heap-size)
  (let* ((indirect-offset (calc-indirect-offset code-size num-imports heap-size))
         (indirect-size (* num-imports #x2 #x4)))
    (align-up (+ indirect-offset indirect-size) #x8)))

;; Calculate exports offset
(defun calc-exports-offset (code-size num-imports heap-size)
  (+ (calc-fixups-offset code-size num-imports heap-size) #x50))

;; Calculate LINKEDIT size
(defun calc-linkedit-size (num-imports)
  (let* ((nlist-size (* (+ #x1 num-imports) #x10))
         (string-table-size (+ #x7 (* num-imports #x8)))
         (indirect-size (* num-imports #x2 #x4)))
    (align-up (+ nlist-size string-table-size indirect-size #x58) (page-size))))

;; Write all load commands
(defun write-load-commands (buf code-size num-imports heap-size)
  (let* ((text-vmsize (calc-text-vmsize code-size num-imports))
         (code-offset (calc-code-offset))
         (stubs-offset (calc-stubs-offset code-size))
         (stubs-total-size (* num-imports #xC))
         (data-const-vmaddr (calc-data-const-vmaddr code-size num-imports))
         (data-vmaddr (calc-data-vmaddr code-size num-imports))
         (heap-vmsize (align-up heap-size (page-size)))
         (linkedit-fileoff (calc-linkedit-fileoff code-size num-imports heap-size))
         (linkedit-vmaddr (+ data-vmaddr heap-vmsize))
         (linkedit-size (calc-linkedit-size num-imports))
         (got-total-size (if (> num-imports #x0) (* num-imports #x8) #x8))
         (symtab-offset (calc-symtab-offset code-size num-imports heap-size))
         (strtab-offset (calc-strtab-offset code-size num-imports heap-size))
         (string-table-size (+ #x7 (* num-imports #x8)))
         (indirect-offset (calc-indirect-offset code-size num-imports heap-size))
         (fixups-offset (calc-fixups-offset code-size num-imports heap-size))
         (exports-offset (calc-exports-offset code-size num-imports heap-size))
         (b buf))
    ;; 1. __PAGEZERO
    (setq b (buf-segment-command-64 b "__PAGEZERO" #x0 (vm-base) #x0 #x0 #x0 #x0 #x0 #x0))
    ;; 2. __TEXT
    (setq b (buf-segment-command-64 b "__TEXT" (vm-base) text-vmsize #x0 text-vmsize
                                    (logior (vm-prot-read) (vm-prot-execute))
                                    (logior (vm-prot-read) (vm-prot-execute)) #x2 #x0))
    (setq b (buf-section-64 b "__text" "__TEXT" (+ (vm-base) code-offset) code-size
                            code-offset #x2 #x0 #x0
                            (logior (s-attr-pure-instructions) (s-attr-some-instructions)) #x0 #x0))
    (setq b (buf-section-64 b "__stubs" "__TEXT" (+ (vm-base) stubs-offset) stubs-total-size
                            stubs-offset #x2 #x0 #x0
                            (logior (s-symbol-stubs) (s-attr-pure-instructions)) #x0 #xC))
    ;; 3. __DATA_CONST
    (setq b (buf-segment-command-64 b "__DATA_CONST" data-const-vmaddr (page-size)
                                    text-vmsize (page-size)
                                    (logior (vm-prot-read) (vm-prot-write))
                                    (logior (vm-prot-read) (vm-prot-write)) #x1 #x0))
    (setq b (buf-section-64 b "__got" "__DATA_CONST" data-const-vmaddr got-total-size
                            text-vmsize #x3 #x0 #x0 (s-non-lazy-symbol-pointers) num-imports #x0))
    ;; 4. __DATA
    (setq b (buf-segment-command-64 b "__DATA" data-vmaddr heap-vmsize
                                    (+ text-vmsize (page-size)) heap-vmsize
                                    (logior (vm-prot-read) (vm-prot-write))
                                    (logior (vm-prot-read) (vm-prot-write)) #x1 #x0))
    (setq b (buf-section-64 b "__heap" "__DATA" data-vmaddr heap-vmsize
                            (+ text-vmsize (page-size)) #x3 #x0 #x0 #x0 #x0 #x0))
    ;; 5. __LINKEDIT
    (setq b (buf-segment-command-64 b "__LINKEDIT" linkedit-vmaddr (page-size)
                                    linkedit-fileoff linkedit-size
                                    (vm-prot-read) (vm-prot-read) #x0 #x0))
    ;; Other load commands
    (setq b (buf-dylinker-command b "/usr/lib/dyld"))
    (setq b (buf-uuid-command b))
    (setq b (buf-build-version-command b))
    (setq b (buf-main-command b code-offset))
    (setq b (buf-load-dylib-command b "/usr/lib/libSystem.B.dylib"))
    (setq b (buf-chained-fixups-command b fixups-offset #x50))
    (setq b (buf-exports-trie-command b exports-offset #x8))
    (setq b (buf-symtab-command b symtab-offset (+ #x1 num-imports) strtab-offset string-table-size))
    (setq b (buf-dysymtab-command b #x0 #x0 #x0 #x1 #x1 num-imports
                                  indirect-offset (* num-imports #x2)))
    b))

;; Write code and stubs section
(defun write-code-section (buf code-bytes num-imports)
  (let* ((code-size (length code-bytes))
         (code-offset (calc-code-offset))
         (stubs-offset (calc-stubs-offset code-size))
         (data-const-vmaddr (calc-data-const-vmaddr code-size num-imports))
         (b buf))
    ;; Pad to code
    (setq b (buf-zeros b (- code-offset (buf-length b))))
    ;; Code
    (setq b (buf-bytes b code-bytes))
    ;; Pad to stubs
    (let ((current (buf-length b)))
      (if (< current stubs-offset)
          (setq b (buf-zeros b (- stubs-offset current)))))
    ;; Stubs
    (let* ((stub-vmaddr (+ (vm-base) stubs-offset))
           (stub-page (ash stub-vmaddr #x-C))
           (got-page (ash data-const-vmaddr #x-C))
           (got-page-diff (- got-page stub-page)))
      (setq b (generate-stubs b got-page-diff num-imports #x0)))
    b))

;; Write GOT and heap sections
(defun write-data-sections (buf code-size num-imports heap-size)
  (let* ((text-vmsize (calc-text-vmsize code-size num-imports))
         (heap-vmsize (align-up heap-size (page-size)))
         (data-fileoff (+ text-vmsize (page-size)))
         (b buf))
    ;; Pad to DATA_CONST
    (let ((current (buf-length b)))
      (if (< current text-vmsize)
          (setq b (buf-zeros b (- text-vmsize current)))))
    ;; GOT entries
    (setq b (write-got-entries b num-imports #x0))
    ;; Pad to DATA
    (let ((current (buf-length b)))
      (if (< current data-fileoff)
          (setq b (buf-zeros b (- data-fileoff current)))))
    ;; Heap
    (setq b (buf-zeros b heap-vmsize))
    b))

;; Write LINKEDIT section
(defun write-linkedit-section (buf code-size num-imports heap-size imports)
  (let* ((linkedit-fileoff (calc-linkedit-fileoff code-size num-imports heap-size))
         (linkedit-size (calc-linkedit-size num-imports))
         (indirect-offset (calc-indirect-offset code-size num-imports heap-size))
         (fixups-offset (calc-fixups-offset code-size num-imports heap-size))
         (exports-offset (calc-exports-offset code-size num-imports heap-size))
         (text-vmsize (calc-text-vmsize code-size num-imports))
         (code-offset (calc-code-offset))
         (b buf))
    ;; Symbol table (_main)
    (setq b (buf-nlist-64 b #x1 #xF #x1 #x10 (+ (vm-base) code-offset)))
    (setq b (write-import-symbols b num-imports #x7 #x0))
    ;; String table
    (setq b (buf-u8 b #x0))
    (setq b (buf-string b "_main"))
    (setq b (buf-u8 b #x0))
    (setq b (write-import-strings b imports))
    ;; Pad to indirect
    (let ((current (buf-length b)))
      (if (< current indirect-offset)
          (setq b (buf-zeros b (- indirect-offset current)))))
    ;; Indirect symbols
    (setq b (write-indirect-syms b num-imports #x0))
    (setq b (write-indirect-syms b num-imports #x0))
    ;; Pad to fixups
    (let ((current (buf-length b)))
      (if (< current fixups-offset)
          (setq b (buf-zeros b (- fixups-offset current)))))
    ;; Chained fixups
    (setq b (buf-bytes b (build-chained-fixups-data num-imports #x5 #x2 text-vmsize)))
    (setq b (write-import-strings b imports))
    ;; Pad to exports
    (let ((current (buf-length b)))
      (if (< current exports-offset)
          (setq b (buf-zeros b (- exports-offset current)))))
    ;; Exports trie
    (setq b (buf-u8 b #x0))
    (setq b (buf-u8 b #x0))
    (setq b (buf-zeros b #x6))
    ;; Pad to end
    (let ((current (buf-length b))
          (target (+ linkedit-fileoff linkedit-size)))
      (if (< current target)
          (setq b (buf-zeros b (- target current)))))
    b))

;; Write vector of bytes to file
(defun write-bytes-to-file (path bytes-vec)
  "Write a vector of bytes to a file. Returns 0 on success, -1 on error."
  (let* ((flags (logior (logior (o-wronly) (o-creat)) (o-trunc)))
         (fd (sys-open path flags #x1FF)))
    (if (< fd #x0)
        #x-1  ; Failed to open
        (let* ((len (length bytes-vec))
               (str (make-string-from-vector bytes-vec))
               (written (sys-write fd str len)))
          (sys-close fd)
          (if (= written len)
              #x0   ; Success
              #x-1))))) ; Failed to write

(defun write-macho-with-imports-and-heap (output-path code-bytes imports heap-size)
  "Write a Mach-O executable with external imports and heap."
  (let* ((num-imports (length imports))
         (code-size (length code-bytes))
         (ncmds #xC)
         (sizeofcmds (calc-sizeofcmds))
         (b nil))
    ;; Build Mach-O in buffer (as reversed byte list)
    (setq b (buf-mach-header-64 b ncmds sizeofcmds (mh-dyldlink)))
    (setq b (write-load-commands b code-size num-imports heap-size))
    (setq b (write-code-section b code-bytes num-imports))
    (setq b (write-data-sections b code-size num-imports heap-size))
    (setq b (write-linkedit-section b code-size num-imports heap-size imports))
    ;; Convert buffer to vector and write to file
    (write-bytes-to-file output-path (buf-to-vector b))))

;; Helper: generate stubs for each import
(defun generate-stubs (buf got-page-diff num-imports i)
  (if (>= i num-imports)
      buf
      (let* ((got-slot-offset (* i #x8))
             (stub (generate-stub got-page-diff got-slot-offset)))
        (generate-stubs (buf-bytes buf stub) got-page-diff num-imports (+ i #x1)))))

;; Helper: write GOT entries (chained bind pointers)
(defun write-got-entries (buf num-imports i)
  (if (>= i num-imports)
      buf
      (let* ((is-last (= i (- num-imports #x1)))
             (ordinal i)
             (next (if is-last #x0 #x2))          ; stride = 2 (8 bytes / 4)
             (entry (logior #x8000000000000000    ; bind bit
                           ordinal
                           (ash next #x33))))     ; next at bits 51-62
        (write-got-entries (buf-u64-le buf entry) num-imports (+ i #x1)))))

;; Helper: write import symbols
(defun write-import-symbols (buf num-imports strx i)
  (if (>= i num-imports)
      buf
      (let ((b (buf-nlist-64 buf strx #x1 #x0 #x100 #x0)))
        (write-import-symbols b num-imports (+ strx #x7) (+ i #x1)))))

;; Helper: write import strings
(defun write-import-strings (buf imports)
  (if (null imports)
      buf
      (let* ((b1 (buf-string buf (car imports)))
             (b2 (buf-u8 b1 #x0)))
        (write-import-strings b2 (cdr imports)))))

;; Helper: write indirect symbol indices
(defun write-indirect-syms (buf num-imports i)
  (if (>= i num-imports)
      buf
      (write-indirect-syms (buf-u32-le buf (+ i #x1)) num-imports (+ i #x1))))

;;; High-level delivery function

;; Split deliver-with-imports-and-heap to avoid let* binding limit
(defun calc-heap-page-offset (code-bytes imports)
  "Calculate heap page offset for code+stubs (5 bindings)"
  (let* ((wrapper-stub-size #x50)
         (total-code-size (+ (length code-bytes) wrapper-stub-size))
         (approx-code-offset #x400)
         (stubs-offset (align-up (+ approx-code-offset total-code-size) #x4))
         (stubs-end (+ stubs-offset (* (length imports) #xC))))
    (calc-heap-page-offset-2 stubs-end)))

(defun calc-heap-page-offset-2 (stubs-end)
  "Calculate heap page offset part 2 (2 bindings)"
  (let* ((text-vmsize (align-up stubs-end (page-size)))
         (heap-page-offset (+ (/ text-vmsize (page-size)) #x1)))
    heap-page-offset))

(defun deliver-with-imports-and-heap (output-path code-bytes imports heap-size)
  "Create a standalone executable with imports and heap"
  (let* ((heap-page-offset (calc-heap-page-offset code-bytes imports))
         (wrapped-code (wrap-with-heap-stub code-bytes heap-page-offset)))
    (write-macho-with-imports-and-heap output-path wrapped-code imports heap-size)))

;;; Main entry point
;;; Mode is determined by first form in input.lisp:
;;;   #x100 - compile expression to IR and evaluate (compile-test)
;;;   #x200 - compile expression to IR and generate bytecode length (codegen-test)
;;;   #x300 - compile, codegen, and link to executable (link-test)
;;;   #x400 - self-compile mode (exposes compiler functions to user code)
;;;   anything else - interpret using h0-eval

;;; Build fenv with compiler functions for self-compilation mode
;;; Each entry is (name params . body) matching collect-defuns format
;;; For built-in functions, we use a special :builtin marker
;; Helper to create builtin fenv entry - uses habu symbol as key
;; Mode 1024: fenv keys are habu symbols, lookup uses sym-eq (O(1) eq comparison)
;; The symbol is interned at startup, habu-read returns the same symbol
#+sbcl
(defmacro make-builtin-entry (name builtin-kw)
  "Create fenv entry with habu symbol key (macro expands at compile time).
   The name is converted to a habu symbol, enabling eq comparison at runtime."
  `(cons (make-habu-symbol-form ,name) (cons ,builtin-kw (intern ,name))))

#-sbcl
(defun make-builtin-entry (name builtin-kw)
  ;; Native habu - intern to get canonical symbol
  (cons (intern name) (cons builtin-kw (intern name))))

(defun make-compiler-fenv ()
  "Build fenv with core compiler functions exposed.
   Uses setq to mutate a single variable to avoid bootstrap bugs."
  ;; Format: (name . (:builtin . impl-symbol))
  ;; Build list by mutating lst with setq - most reliable for bootstrap
  (let ((kw (intern-keyword "BUILTIN"))
        (lst nil))
    ;; ARM64 - utility (add in reverse order, H0-COMPILE last = first in result)
    (setq lst (cons (make-builtin-entry "ENCODE" kw) lst))
    (setq lst (cons (make-builtin-entry "REG" kw) lst))
    ;; ARM64 - system
    (setq lst (cons (make-builtin-entry "NOP" kw) lst))
    (setq lst (cons (make-builtin-entry "BRK" kw) lst))
    (setq lst (cons (make-builtin-entry "SVC" kw) lst))
    ;; ARM64 - compare and branch
    (setq lst (cons (make-builtin-entry "RET" kw) lst))
    (setq lst (cons (make-builtin-entry "B.GE" kw) lst))
    (setq lst (cons (make-builtin-entry "B.GT" kw) lst))
    (setq lst (cons (make-builtin-entry "B.LE" kw) lst))
    (setq lst (cons (make-builtin-entry "B.LT" kw) lst))
    (setq lst (cons (make-builtin-entry "B.NE" kw) lst))
    (setq lst (cons (make-builtin-entry "B.EQ" kw) lst))
    (setq lst (cons (make-builtin-entry "CBNZ" kw) lst))
    (setq lst (cons (make-builtin-entry "CBZ" kw) lst))
    (setq lst (cons (make-builtin-entry "BLR" kw) lst))
    (setq lst (cons (make-builtin-entry "BR" kw) lst))
    (setq lst (cons (make-builtin-entry "BL" kw) lst))
    (setq lst (cons (make-builtin-entry "B" kw) lst))
    (setq lst (cons (make-builtin-entry "CSET" kw) lst))
    (setq lst (cons (make-builtin-entry "CMP" kw) lst))
    ;; ARM64 - bitwise
    (setq lst (cons (make-builtin-entry "MVN" kw) lst))
    (setq lst (cons (make-builtin-entry "ASR" kw) lst))
    (setq lst (cons (make-builtin-entry "LSR" kw) lst))
    (setq lst (cons (make-builtin-entry "LSL" kw) lst))
    (setq lst (cons (make-builtin-entry "EOR" kw) lst))
    (setq lst (cons (make-builtin-entry "ORR" kw) lst))
    (setq lst (cons (make-builtin-entry "AND*" kw) lst))
    ;; ARM64 - arithmetic
    (setq lst (cons (make-builtin-entry "NEG" kw) lst))
    (setq lst (cons (make-builtin-entry "SDIV" kw) lst))
    (setq lst (cons (make-builtin-entry "MUL" kw) lst))
    (setq lst (cons (make-builtin-entry "SUBS" kw) lst))
    (setq lst (cons (make-builtin-entry "SUB" kw) lst))
    (setq lst (cons (make-builtin-entry "ADD" kw) lst))
    ;; ARM64 - data movement
    (setq lst (cons (make-builtin-entry "MOVN" kw) lst))
    (setq lst (cons (make-builtin-entry "MOVK" kw) lst))
    (setq lst (cons (make-builtin-entry "MOVZ" kw) lst))
    (setq lst (cons (make-builtin-entry "MOV" kw) lst))
    (setq lst (cons (make-builtin-entry "LDRB" kw) lst))
    (setq lst (cons (make-builtin-entry "STRB" kw) lst))
    (setq lst (cons (make-builtin-entry "LDP" kw) lst))
    (setq lst (cons (make-builtin-entry "STP" kw) lst))
    (setq lst (cons (make-builtin-entry "LDUR" kw) lst))
    (setq lst (cons (make-builtin-entry "STUR" kw) lst))
    (setq lst (cons (make-builtin-entry "LDR" kw) lst))
    (setq lst (cons (make-builtin-entry "STR" kw) lst))
    ;; Backend - codegen.lisp
    (setq lst (cons (make-builtin-entry "COLLECT-DEFUNS" kw) lst))
    (setq lst (cons (make-builtin-entry "NATIVE-READ-FILE" kw) lst))
    (setq lst (cons (make-builtin-entry "READ-ALL" kw) lst))
    (setq lst (cons (make-builtin-entry "FLATTEN-ALL-CALLS" kw) lst))
    (setq lst (cons (make-builtin-entry "RESOLVE-CALLS" kw) lst))
    (setq lst (cons (make-builtin-entry "CODEGEN-FN" kw) lst))
    (setq lst (cons (make-builtin-entry "DELIVER-WITH-IMPORTS-AND-HEAP" kw) lst))
    ;; Register allocator pipeline
    (setq lst (cons (make-builtin-entry "NEXT-VREG" kw) lst))
    (setq lst (cons (make-builtin-entry "MAKE-VREG-COUNTER" kw) lst))
    (setq lst (cons (make-builtin-entry "TAC-CODEGEN" kw) lst))
    (setq lst (cons (make-builtin-entry "LINEAR-SCAN" kw) lst))
    (setq lst (cons (make-builtin-entry "COMPUTE-INTERVALS" kw) lst))
    (setq lst (cons (make-builtin-entry "COMPUTE-LIVENESS" kw) lst))
    (setq lst (cons (make-builtin-entry "IR-TO-TAC" kw) lst))
    (setq lst (cons (make-builtin-entry "CODEGEN-MAIN-REG-ALLOC" kw) lst))
    (setq lst (cons (make-builtin-entry "CODEGEN-FN-REG-ALLOC" kw) lst))
    ;; TAC helpers
    (setq lst (cons (make-builtin-entry "TAC-DEF" kw) lst))
    (setq lst (cons (make-builtin-entry "TAC-USE" kw) lst))
    ;; Compiler functions - front end (added last, will be first)
    (setq lst (cons (make-builtin-entry "LAMBDAS-TO-DEFUNS" kw) lst))
    (setq lst (cons (make-builtin-entry "LIFT-LAMBDAS" kw) lst))
    (setq lst (cons (make-builtin-entry "H0-COMPILE" kw) lst))
    ;; IR constructors - in SBCL mode, h0-eval-call's fallback handles these
    ;; automatically. In native mode, explicit registration is required.
    (setq lst (cons (make-builtin-entry "IR-LIT" kw) lst))
    (setq lst (cons (make-builtin-entry "IR-VAR" kw) lst))
    (setq lst (cons (make-builtin-entry "IR-ADD" kw) lst))
    (setq lst (cons (make-builtin-entry "IR-SUB" kw) lst))
    (setq lst (cons (make-builtin-entry "IR-IF" kw) lst))
    (setq lst (cons (make-builtin-entry "IR-CONS" kw) lst))
    (setq lst (cons (make-builtin-entry "IR-CAR" kw) lst))
    (setq lst (cons (make-builtin-entry "IR-CDR" kw) lst))
    (setq lst (cons (make-builtin-entry "MAKE-VREG-COUNTER" kw) lst))
    (setq lst (cons (make-builtin-entry "NEXT-VREG" kw) lst))
    lst))

(defun main ()
  ;; Initialize dispatch tables
  (init-compile-ops)
  (init-builtin-dispatch)
  (let ((source (native-read-file "input.lisp")))
    (if (null source)
        (fatal-error "main: input.lisp not found")
        (let ((forms (read-all source)))
          (if (null forms)
              (fatal-error "main: parse error - no forms")
              (let ((first-form (car forms)))
                (cond
                  ;; Compile test mode REMOVED: h0-compile-and-eval was removed with old codegen
                  ((if (numberp first-form) (= first-form #x100) nil)
                   (fatal-error "main: mode 256 removed"))
                  ;; Codegen test mode REMOVED: use deliver-file instead
                  ;; The old h0-codegen interface (ir, dest-reg) was replaced by
                  ;; codegen-fn-reg-alloc which takes (name params body-ir param-base)
                  ((if (numberp first-form) (= first-form #x200) nil)
                   (fatal-error "main: mode 512 removed - use deliver-file"))
                  ;; Link test mode REMOVED: use deliver-file instead
                  ((if (numberp first-form) (= first-form #x300) nil)
                   (fatal-error "main: mode 768 removed - use deliver-file"))
                  ;; Self-compile mode: expose compiler functions to user code
                  ((if (numberp first-form) (= first-form #x400) nil)
                   (let* ((rest-forms (cdr forms))
                          (compiler-fenv (make-compiler-fenv))
                          (user-fenv (collect-defuns rest-forms nil))
                          ;; Merge user defuns with compiler functions
                          (fenv (h0-append user-fenv compiler-fenv)))
                     (h0-eval-forms rest-forms nil fenv)))
                  ;; Normal interpretation mode
                  (t
                   (let ((fenv (collect-defuns forms nil)))
                     (h0-eval-forms forms nil fenv))))))))))

(main)
