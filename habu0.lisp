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

;;; Runtime keyword variables - interned at startup for eq comparison
;;; SBCL-compiled keywords != habu0-reader keywords, so we intern at runtime
(defvar *kw-offset* nil)
(defvar *kw-imm* nil)

;;; Register keywords - pre-interned for eq comparison in arm64:reg
;;; These are initialized in init-compile-ops and used via habu0-reg
(defvar *kw-x0* nil) (defvar *kw-x1* nil) (defvar *kw-x2* nil) (defvar *kw-x3* nil)
(defvar *kw-x4* nil) (defvar *kw-x5* nil) (defvar *kw-x6* nil) (defvar *kw-x7* nil)
(defvar *kw-x8* nil) (defvar *kw-x9* nil) (defvar *kw-x10* nil) (defvar *kw-x11* nil)
(defvar *kw-x12* nil) (defvar *kw-x13* nil) (defvar *kw-x14* nil) (defvar *kw-x15* nil)
(defvar *kw-x16* nil) (defvar *kw-x17* nil) (defvar *kw-x18* nil) (defvar *kw-x19* nil)
(defvar *kw-x20* nil) (defvar *kw-x21* nil) (defvar *kw-x22* nil) (defvar *kw-x23* nil)
(defvar *kw-x24* nil) (defvar *kw-x25* nil) (defvar *kw-x26* nil) (defvar *kw-x27* nil)
(defvar *kw-x28* nil) (defvar *kw-x29* nil) (defvar *kw-x30* nil)
(defvar *kw-sp* nil) (defvar *kw-xzr* nil) (defvar *kw-lr* nil) (defvar *kw-fp* nil)
(defvar *kw-env* nil) (defvar *kw-closure* nil) (defvar *kw-code-base* nil)
(defvar *kw-gc* nil) (defvar *kw-heap* nil)

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

;;; Symbol interning - ensures eq works for all symbols with same name
;;; The intern table is stored at [x27+0] and accessed via primitives

;; Search intern table (alist of (name . symbol)) for name
;; Returns the symbol if found, nil otherwise
;; Uses case-insensitive comparison to match bootstrap compiler behavior
(defun find-interned (name table)
  (if (null table)
      nil
      (let ((entry (car table)))
        (if (string-equal (car entry) name)
            (cdr entry)
            (find-interned name (cdr table))))))

;; NOTE: get-intern-table and set-intern-table are compiler primitives
;; They generate code to load/store from [x27+0] (intern table at GC globals base)
;; Do NOT define them here - they're built-in primitives handled by the compiler

;; Intern a string as a symbol
;; Handles pkg:sym syntax for package-qualified symbols
;; Returns existing symbol if found, else creates new and adds it
;; This ensures all symbols with the same name are eq
(defun intern (name)
  (let* ((uname (string-upcase name))
         (parsed (parse-symbol-name uname))
         (pkg-name (car parsed))
         (sym-name (cdr parsed)))
    (if pkg-name
        ;; Explicit package prefix: intern in that package
        (intern-in-package sym-name pkg-name)
        ;; No package prefix: use current package or fall back to global table
        (if *current-package*
            (intern-in-package sym-name *current-package*)
            ;; Default: use global intern table (CL-USER equivalent)
            (let ((existing (find-interned uname (get-intern-table))))
              (if existing
                  existing
                  (let ((sym (make-symbol-from-string uname)))
                    (set-intern-table (cons (cons uname sym) (get-intern-table)))
                    sym)))))))

;;; Tag manipulation primitives
;;; In Habu, all values have a 4-bit tag in the low bits:
;;;   0 = fixnum (value << 4), 1 = cons, 2 = symbol, 3 = vector,
;;;   4 = string, 5 = closure, 6 = nil, 7 = keyword
;;;
;;; NOTE: get-tag and set-tag are COMPILER PRIMITIVES - they are recognized
;;; by both the bootstrap compiler and h0-compile as special forms that
;;; generate inline ARM64 code. Do NOT define them as functions here.
;;; The h0-eval special form handlers at lines ~1900-1912 call these
;;; primitives directly, and the bootstrap compiler compiles those calls
;;; to inline ARM64 instructions.

;;; Keyword interning - keywords use tag 7, stored in separate keyword table
;;; Keywords are self-evaluating symbols in the KEYWORD package
;;; Keywords have same memory layout as STRINGS: [length:8][chars:N]
;;; (NOT symbols - symbols have a pointer to name string at offset 0)

;; Get keyword name - keywords have STRING layout, just different tag (7 vs 4)
;; Layout: [length:8][chars:N] - same as strings, NOT symbols
;; Symbols have pointer to name at offset 0, keywords ARE the string data
(defun keyword-name (kw)
  ;; Keywords have same layout as STRINGS, not symbols
  ;; Just change tag from 7 (keyword) to 4 (string)
  (set-tag kw 4))

;; Make keyword from string - allocate like symbol but with tag 7
(defun make-keyword-from-string (name)
  ;; Use make-symbol-from-string which allocates [length:8][chars:N]
  ;; Then change tag from 2 (symbol) to 7 (keyword)
  (let ((sym (make-symbol-from-string name)))
    (set-tag sym 7)))

;; Keyword table primitives
;; NOTE: get-keyword-table and set-keyword-table are compiler primitives
;; that access the separate keyword table at [x27+128], distinct from
;; the intern (symbol) table at [x27+0]. This separation ensures that
;; keywords like :X0 remain distinct from symbols like X0.

;; Search keyword table for name
;; Uses case-insensitive comparison to match bootstrap compiler behavior
(defun find-keyword (name table)
  (if (null table)
      nil
      (let ((entry (car table)))
        (if (string-equal (car entry) name)
            (cdr entry)
            (find-keyword name (cdr table))))))

;; Intern a keyword by name (without the leading colon)
;; Keywords are like symbols but with tag 7 instead of tag 2
(defun intern-keyword (name)
  (let* ((uname (string-upcase name))
         (existing (find-keyword uname (get-keyword-table))))
    (if existing
        existing
        (let ((kw (make-keyword-from-string uname)))
          (set-keyword-table (cons (cons uname kw) (get-keyword-table)))
          kw))))

;; String upcase helper - converts lowercase to uppercase
(defun string-upcase (s)
  (let* ((len (string-length s))
         (vec (make-vector len)))
    (string-upcase-loop s vec len #x0)
    (make-string-from-vector vec)))

(defun string-upcase-loop (src dst len i)
  (if (>= i len)
      dst
      (progn
        (vector-set dst i (h0-char-upcase (string-ref src i)))
        (string-upcase-loop src dst len (+ i #x1)))))

;;; ============================================================
;;; String utilities for package system
;;; ============================================================

;; Find position of first colon in string, or nil if none
(defun find-colon (str)
  (find-colon-loop str 0 (string-length str)))

(defun find-colon-loop (str i len)
  (if (>= i len)
      nil
      (if (= (string-ref str i) #x3A)  ; colon
          i
          (find-colon-loop str (+ i 1) len))))

;; Extract substring from start to end (exclusive)
(defun substring (str start end)
  (let* ((len (- end start))
         (vec (make-vector len)))
    (substring-copy str vec start 0 len)
    (make-string-from-vector vec)))

(defun substring-copy (src dst start i len)
  (if (>= i len)
      dst
      (progn
        (vector-set dst i (string-ref src (+ start i)))
        (substring-copy src dst start (+ i 1) len))))

;; Concatenate two strings
(defun string-concat (s1 s2)
  (let* ((len1 (string-length s1))
         (len2 (string-length s2))
         (vec (make-vector (+ len1 len2))))
    (string-copy-to-vec s1 vec 0 len1)
    (string-copy-to-vec s2 vec len1 len2)
    (make-string-from-vector vec)))

(defun string-copy-to-vec (src dst start len)
  (string-copy-to-vec-loop src dst start 0 len))

(defun string-copy-to-vec-loop (src dst start i len)
  (if (>= i len)
      dst
      (progn
        (vector-set dst (+ start i) (string-ref src i))
        (string-copy-to-vec-loop src dst start (+ i 1) len))))

;; Concatenate three strings
(defun string-concat3 (s1 s2 s3)
  (string-concat (string-concat s1 s2) s3))

;; Create a list of n nil values
(defun make-list (n)
  (make-list-loop n nil))

(defun make-list-loop (n acc)
  (if (<= n 0)
      acc
      (make-list-loop (- n 1) (cons nil acc))))

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

(defun listp (x)
  (or (null x) (consp x)))

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

(defun skip-symbol (source pos)
  (let ((ch (char-at source pos)))
    (if (symbol-char? ch)
        (skip-symbol source (+ pos #x1))
        pos)))

;; String equality check
;; String comparison helper - no labels
(defun string=-loop (s1 s2 len i)
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (string=-loop s1 s2 len (+ i 1))
          nil)))

(defun string= (s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (string=-loop s1 s2 len1 0)
        nil)))

;; Case-insensitive string comparison
(defun string-equal-loop (s1 s2 len i)
  (if (>= i len)
      t
      (if (= (h0-char-upcase (string-ref s1 i))
             (h0-char-upcase (string-ref s2 i)))
          (string-equal-loop s1 s2 len (+ i 1))
          nil)))

(defun string-equal (s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (string-equal-loop s1 s2 len1 0)
        nil)))

;; Operator checks - compare symbol names using string-equal
;; Native Habu creates new symbol objects at runtime, so eq comparison fails.
;; Use string-equal on symbol-name for reliable operator matching.
;; Must handle nil arguments - nil has tag 6, not a proper symbol.
(defun sym-eq (s1 s2)
  (if (or (null s1) (null s2))
      nil  ;; nil can't match any symbol by name
      (string-equal (symbol-name s1) (symbol-name s2))))
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

;; Main reader with labels for mutual recursion
(defun habu-read (source pos)
  (labels
      ((read-list-elems (p)
         (let* ((p2 (skip-ws source p))
                (ch (char-at source p2)))
           (cond
             ((= ch #x29) (cons nil (+ p2 #x1)))
             ((= ch #x2E)
              (let* ((r (read-one (+ p2 #x1)))
                     (cdr-val (car r))
                     (p3 (cdr r))
                     (p4 (skip-ws source p3)))
                (cons cdr-val (+ p4 #x1))))
             ((= ch #x0) (cons nil p2))
             (t (let* ((er (read-one p2))
                       (el (car er))
                       (p3 (cdr er))
                       (rr (read-list-elems p3))
                       (rest-list (car rr))
                       (rest-pos (cdr rr))
                       (new-list (cons el rest-list)))
                  (cons new-list rest-pos))))))
       (read-list (p) (read-list-elems (+ p #x1)))
       (read-sharp (p)
         (let ((ch (char-at source (+ p #x1))))
           (cond
             ((or (= ch #x78) (= ch #x58)) (read-hex source (+ p #x2)))
             ((= ch #x27)
              (let* ((r (read-one (+ p #x2)))
                     (val (car r))
                     (pos (cdr r))
                     (result (list 'function val)))
                (cons result pos)))
             ;; #+ reader conditional - include if feature present
             ((= ch #x2B)
              (let* ((p2 (+ p #x2))                    ; position after #+
                     (feat-result (read-feature-name source p2))
                     (feat-name (car feat-result))     ; feature name as string
                     (p3 (cdr feat-result))            ; position after feature name
                     (form-result (read-one p3))       ; read the conditional form
                     (form (car form-result))          ; the form itself
                     (p4 (cdr form-result)))           ; position after form
                (if (has-feature? feat-name)
                    (cons form p4)                     ; feature present: return form
                    (read-one p4))))                   ; feature absent: skip form, read next
             ;; #- reader conditional - include if feature NOT present
             ((= ch #x2D)
              (let* ((p2 (+ p #x2))                    ; position after #-
                     (feat-result (read-feature-name source p2))
                     (feat-name (car feat-result))     ; feature name as string
                     (p3 (cdr feat-result))            ; position after feature name
                     (form-result (read-one p3))       ; read the conditional form
                     (form (car form-result))          ; the form itself
                     (p4 (cdr form-result)))           ; position after form
                (if (has-feature? feat-name)
                    (read-one p4)                      ; feature present: skip form, read next
                    (cons form p4))))                  ; feature absent: return form
             (t (fatal-error "read-sharp: unknown # syntax")))))
       (read-one (p)
         (let* ((p2 (skip-ws source p))
                (ch (char-at source p2)))
           (if (>= p2 (string-length source))
               (cons nil p2)
               (match ch
                 (#x28 (read-list p2))                           ; (
                 (#x27 (let* ((r (read-one (+ p2 #x1)))          ; '
                              (val (car r))
                              (pos (cdr r)))
                         (cons (list *op-quote* val) pos)))
                 (#x22 (read-str source p2))                     ; "
                 (#x23 (read-sharp p2))                          ; #
                 (#x29 (cons nil (+ p2 #x1)))                    ; )
                 (#x2D (if (digit? (char-at source (+ p2 #x1)))  ; - followed by digit
                           (read-int source p2)
                           (read-sym source p2)))
                 (#x2B (if (digit? (char-at source (+ p2 #x1)))  ; + followed by digit
                           (read-int source p2)
                           (read-sym source p2)))
                 (#x3A (read-keyword source (+ p2 #x1)))         ; : keyword
                 (_                                              ; default
                  (if (digit? ch)
                      (read-int source p2)
                      (if (symbol-char? ch)
                          (read-sym source p2)
                          (read-one (+ p2 #x1))))))))))
    (read-one pos)))

;; Reverse a list
(defun reverse-acc (lst acc)
  (if (null lst)
      acc
      (reverse-acc (cdr lst) (cons (car lst) acc))))

(defun reverse (lst)
  (reverse-acc lst nil))

;; Helper for read-all - avoids labels which has codegen issues
(defun read-all-loop (source len pos acc)
  (let ((p2 (skip-ws source pos)))
    (if (>= p2 len)
        (reverse acc)
        (let ((r (habu-read source p2)))
          (read-all-loop source len (cdr r) (cons (car r) acc))))))

(defun read-all (source)
  (let ((len (string-length source)))
    (read-all-loop source len #x0 nil)))

(defun h0-read-from-string (s)
  (car (habu-read s 0)))

;;; Simple expression evaluator with function definitions
;;; This interpreter supports defun, let, and recursion.

;; Look up function by symbol in fenv
;; Entry is (symbol . (params . body))
;; Uses sym-eq (name comparison) since symbols may be from different intern tables
(defun fenv-lookup (sym fenv)
  (if (null fenv) nil
      (let ((entry (car fenv)))
        (if (sym-eq sym (car entry))
            (cdr entry)  ;; Returns (params . body)
            (fenv-lookup sym (cdr fenv))))))

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
  (and (symbolp sym)
       (string-equal (symbol-name sym) name)))

(defun h0-box-transform (e boxed)
  (cond
    ((null e) e)
    ((symbolp e)
     ;; If this var is boxed, transform to (car var) - use interned symbol
     (if (h0-member-eq e boxed)
         (list (intern "CAR") e)
         e))
    ((not (consp e)) e)
    ((h0-sym-named (car e) "QUOTE") e)
    ((h0-sym-named (car e) "SETQ")
     (let ((var (cadr e))
           (val (caddr e)))
       (if (h0-member-eq var boxed)
           ;; Transform to (setcar var val) - use interned symbol for runtime match
           (list (intern "SETCAR") var (h0-box-transform val boxed))
           (list (intern "SETQ") var (h0-box-transform val boxed)))))
    ((h0-sym-named (car e) "LAMBDA")
     (let* ((params (cadr e))
            (body-forms (cddr e))
            ;; Don't transform params - they shadow boxed vars
            (new-boxed (h0-remove-if-member boxed params))
            ;; Transform each body form and return lambda with all of them
            (transformed-forms (h0-box-transform-list body-forms new-boxed)))
       (cons (intern "LAMBDA") (cons params transformed-forms))))
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
    (cons (intern "LET") (cons new-bindings transformed-forms))))

(defun h0-box-bindings (bindings boxed to-box)
  (if (null bindings) nil
      (let* ((b (car bindings))
             (nm (car b))
             (vl (h0-box-transform (cadr b) boxed))
             (new-val (if (h0-member-eq nm to-box)
                          ;; Box: (cons val nil) - use interned symbol
                          (list (intern "CONS") vl nil)
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
      ;; No more bindings - transform body forms and return let* with them
      (cons (intern "LET*") (cons nil (h0-box-transform-list body-forms boxed)))
      (let* ((b (car bindings))
             (nm (car b))
             (vl (h0-box-transform (cadr b) boxed))
             (is-boxed (h0-member-eq nm to-box))
             ;; Box: (cons val nil) - use interned symbol
             (new-val (if is-boxed (list (intern "CONS") vl nil) vl))
             (new-binding (list nm new-val))
             (new-boxed (if is-boxed
                            (cons nm boxed)
                            (h0-remove-if-member boxed (list nm))))
             (rest (h0-box-let*-bindings (cdr bindings) body-forms new-boxed to-box)))
        ;; Reconstruct let* with transformed bindings - use interned symbol
        (cons (intern "LET*") (cons (cons new-binding (cadr rest)) (cddr rest))))))

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
  (cond
    ((null keys) nil)
    ((consp keys)
     ;; List of keys - check each one
     (if (eql key (car keys))
         t
         (case-key-matches key (cdr keys))))
    ;; Single key
    (t (eql key keys))))

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

;; Eval function with fenv for function definitions
;; Uses cached op= functions for O(1) amortized dispatch
(defun h0-eval (expr env fenv)
  (cond
    ;; Numbers are self-evaluating
    ((numberp expr) expr)
    ;; Strings are self-evaluating
    ((stringp expr) expr)
    ;; Keywords are self-evaluating
    ((keywordp expr) expr)
    ;; nil is false (both Lisp nil and symbol NIL)
    ((null expr) nil)
    ;; Symbol NIL - return nil (catches reader-created NIL symbol)
    ((if (symbolp expr) (op=nil expr) nil) nil)
    ;; t is true
    ((if (symbolp expr) (op=t expr) nil) t)
    ;; Symbol lookup - first local env, then global env, then fenv (as function designator)
    ((symbolp expr)
     (let ((entry (env-lookup expr env)))
       (if entry
           (cdr entry)  ; Extract value from local entry
           ;; Try global env
           (let ((global-entry (h0-global-lookup expr)))
             (if global-entry
                 (cdr global-entry)  ; Extract value from global entry
                 ;; Not in var namespaces - check fenv for function
                 ;; If found, return the symbol itself as a function designator
                 ;; This allows (funcall fn ...) where fn is a function name
                 (let ((fn-entry (fenv-lookup expr fenv)))
                   (if fn-entry
                       expr  ; Return symbol as function designator
                       ;; Not found anywhere - undefined symbol
                       (fatal-error "h0-eval: undefined symbol"))))))))
    ;; List - function call or special form
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; Quote - use cached op=quote
         ((if (symbolp op) (op=quote op) nil) (cadr expr))
         ;; If - use cached op=if
         ((if (symbolp op) (op=if op) nil)
          (if (h0-eval (cadr expr) env fenv)
              (h0-eval (caddr expr) env fenv)
              (if (cadddr expr) (h0-eval (cadddr expr) env fenv) nil)))
         ;; Let - use cached op=let, delegate to helper for iteration
         ;; Let body has implicit progn: (let ((x 1)) body1 body2...)
         ((if (symbolp op) (op=let op) nil)
          (h0-eval-let-body (cadr expr) (cddr expr) env fenv))
         ;; Let* - same as let for sequential binding
         ((if (symbolp op) (op=let-star op) nil)
          (h0-eval-let-body (cadr expr) (cddr expr) env fenv))
         ;; Progn - evaluate forms in sequence
         ((if (symbolp op) (op=progn op) nil)
          (h0-eval-progn (cdr expr) env fenv))
         ;; Cond - multi-way conditional
         ((if (symbolp op) (op=cond op) nil)
          (h0-eval-cond (cdr expr) env fenv))
         ;; Defun - returns nil but defines function
         ((if (symbolp op) (op=defun op) nil) nil)
         ;; Defvar - define global variable with initial value (or nil)
         ;; (defvar name) or (defvar name value)
         ((if (symbolp op) (op=defvar op) nil)
          (let* ((var-sym (cadr expr))
                 (init-val (if (cddr expr) (h0-eval (caddr expr) env fenv) nil)))
            ;; Only initialize if not already defined
            (if (null (h0-global-lookup var-sym))
                (h0-global-set var-sym init-val))
            var-sym))
         ;; While - loop while condition is true
         ;; (while test body...) - evaluates body while test is non-nil
         ((if (symbolp op) (op=while op) nil)
          (h0-eval-while (cadr expr) (cddr expr) env fenv))
         ;; Defpackage - create package, returns nil
         ((if (symbolp op) (op=defpackage op) nil)
          (let ((pkg-name (keyword-name (cadr expr))))
            (make-package (string-upcase pkg-name))
            nil))
         ;; In-package - set current package, returns nil
         ((if (symbolp op) (op=in-package op) nil)
          (let ((pkg-name (keyword-name (cadr expr))))
            (setq *current-package* (string-upcase pkg-name))
            ;; Ensure package exists
            (make-package *current-package*)
            nil))
         ;; Case - multi-way conditional on value
         ((if (symbolp op) (op=case op) nil)
          (let ((key (h0-eval (cadr expr) env fenv)))
            (h0-eval-case-clauses key (cddr expr) env fenv)))
         ;; When - conditional execution (returns nil if false)
         ((if (symbolp op) (op=when op) nil)
          (if (h0-eval (cadr expr) env fenv)
              (h0-eval-progn (cddr expr) env fenv)
              nil))
         ;; Unless - inverse conditional (executes when false)
         ((if (symbolp op) (op=unless op) nil)
          (if (h0-eval (cadr expr) env fenv)
              nil
              (h0-eval-progn (cddr expr) env fenv)))
         ;; Declaim - declaration, no-op in interpreter
         ((if (symbolp op) (op=declaim op) nil) nil)
         ;; Setq - variable assignment
         ;; First check local env, then global env
         ((if (symbolp op) (op=setq op) nil)
          (let* ((var-sym (cadr expr))
                 (val (h0-eval (caddr expr) env fenv))
                 (local-cell (env-lookup var-sym env)))
            (if local-cell
                val  ; Return value (local mutation not supported in alist env)
                ;; Try global
                (let ((global-cell (h0-global-lookup var-sym)))
                  (if global-cell
                      (h0-global-set var-sym val)
                      (fatal-error "h0-eval: SETQ unknown variable"))))))
         ;; Error - signal error (crash with message)
         ((if (symbolp op) (op=error op) nil)
          (fatal-error "h0-eval: error called"))
         ;; Arithmetic - use cached op= functions (variadic support)
         ((if (symbolp op) (op=plus op) nil)
          (h0-eval-add (cdr expr) env fenv))
         ((if (symbolp op) (op=minus op) nil)
          (h0-eval-sub (cdr expr) env fenv))
         ((if (symbolp op) (op=mul op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (* left right)))
         ((if (symbolp op) (op=div op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (/ left right)))
         ((if (symbolp op) (op=mod op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (mod left right)))
         ;; List operations
         ((if (symbolp op) (op=cons op) nil)
          (let* ((car-val (h0-eval (cadr expr) env fenv))
                 (cdr-val (h0-eval (caddr expr) env fenv)))
            (cons car-val cdr-val)))
         ((if (symbolp op) (op=car op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (car arg)))
         ((if (symbolp op) (op=cdr op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (cdr arg)))
         ((if (symbolp op) (op=cadr op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (car (cdr arg))))
         ((if (symbolp op) (op=cddr op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (cdr (cdr arg))))
         ((if (symbolp op) (op=caddr op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (car (cdr (cdr arg)))))
         ((if (symbolp op) (op=cadddr op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (car (cdr (cdr (cdr arg))))))
         ((if (symbolp op) (op=list op) nil) (h0-eval-list (cdr expr) env fenv))
         ;; SETCAR - mutate car of cons cell (for boxed mutable captures)
         ((if (symbolp op) (op=setcar op) nil)
          (let* ((cell (h0-eval (cadr expr) env fenv))
                 (val (h0-eval (caddr expr) env fenv)))
            (setcar cell val)
            val))
         ;; Type predicates
         ((if (symbolp op) (op=null op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (if (null arg) t nil)))
         ((if (symbolp op) (op=consp op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (if (consp arg) t nil)))
         ;; Boolean operations
         ((if (symbolp op) (op=not op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (if arg nil t)))
         ((if (symbolp op) (op=and op) nil) (h0-eval-and (cdr expr) env fenv))
         ((if (symbolp op) (op=or op) nil) (h0-eval-or (cdr expr) env fenv))
         ;; Comparisons - use cached op= functions
         ((if (symbolp op) (op=eq-num op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (if (= left right) t nil)))
         ((if (symbolp op) (op=lt op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (if (< left right) t nil)))
         ((if (symbolp op) (op=gt op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (if (> left right) t nil)))
         ((if (symbolp op) (op=le op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (if (<= left right) t nil)))
         ((if (symbolp op) (op=ge op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (if (>= left right) t nil)))
         ;; Type predicates - primitives (use cached symbols, not string comparison)
         ((if (symbolp op) (op=symbolp op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (if (symbolp arg) t nil)))
         ((if (symbolp op) (op=numberp op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (if (numberp arg) t nil)))
         ((if (symbolp op) (op=stringp op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (if (stringp arg) t nil)))
         ((if (symbolp op) (op=keywordp op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (if (keywordp arg) t nil)))
         ;; String primitives (use cached symbols)
         ((if (symbolp op) (op=string-length op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (string-length arg)))
         ((if (symbolp op) (op=string-ref op) nil)
          (let* ((str (h0-eval (cadr expr) env fenv))
                 (idx (h0-eval (caddr expr) env fenv)))
            (string-ref str idx)))
         ((if (symbolp op) (op=char-at op) nil)
          (let* ((str (h0-eval (cadr expr) env fenv))
                 (idx (h0-eval (caddr expr) env fenv)))
            (if (< idx (string-length str))
                (string-ref str idx)
                0)))
         ((if (symbolp op) (op=string= op) nil)
          (let* ((s1 (h0-eval (cadr expr) env fenv))
                 (s2 (h0-eval (caddr expr) env fenv)))
            (if (string= s1 s2) t nil)))
         ;; Symbol primitives (use cached symbols)
         ((if (symbolp op) (op=symbol-name op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (symbol-name arg)))
         ;; Keyword-name - extract string name from keyword
         ((if (symbolp op) (op=keyword-name op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (keyword-name arg)))
         ;; Bitwise operations (use cached symbols)
         ((if (symbolp op) (op=logand op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (logand left right)))
         ((if (symbolp op) (op=logior op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (logior left right)))
         ((if (symbolp op) (op=ash op) nil)
          (let* ((val (h0-eval (cadr expr) env fenv))
                 (shift (h0-eval (caddr expr) env fenv)))
            (ash val shift)))
         ;; EQ comparison (use cached symbol)
         ((if (symbolp op) (op=eq op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (if (eq left right) t nil)))
         ;; EQL comparison (eq for symbols, = for numbers)
         ((if (symbolp op) (op=eql op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (eql left right)))
         ;; Get-tag - use native get-tag primitive
         ((if (symbolp op) (op=get-tag op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (get-tag arg)))
         ;; Set-tag - change tag bits on a pointer value
         ;; (set-tag value new-tag) -> value with its tag bits replaced
         ;; Both value and new-tag are tagged values
         ((if (symbolp op) (op=set-tag op) nil)
          (let ((value (h0-eval (cadr expr) env fenv))
                (new-tag (h0-eval (caddr expr) env fenv)))
            (set-tag value new-tag)))
         ;; Length - count list elements (use cached symbol)
         ((if (symbolp op) (op=length op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (labels ((count-len (lst n)
                       (if (null lst) n
                           (count-len (cdr lst) (+ n 1)))))
              (count-len arg 0))))
         ;; Vector primitives (use cached symbols)
         ((if (symbolp op) (op=make-vector op) nil)
          (let ((size (h0-eval (cadr expr) env fenv)))
            (make-vector size)))
         ((if (symbolp op) (op=vector-length op) nil)
          (let ((vec (h0-eval (cadr expr) env fenv)))
            (vector-length vec)))
         ((if (symbolp op) (op=vector-set op) nil)
          (let* ((vec (h0-eval (cadr expr) env fenv))
                 (idx (h0-eval (caddr expr) env fenv))
                 (val (h0-eval (cadddr expr) env fenv)))
            (vector-set vec idx val)
            val))
         ((if (symbolp op) (op=vector-ref op) nil)
          (let* ((vec (h0-eval (cadr expr) env fenv))
                 (idx (h0-eval (caddr expr) env fenv)))
            (vector-ref vec idx)))
         ;; Reverse list (use cached symbol)
         ((if (symbolp op) (op=reverse op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (labels ((rev-acc (lst acc)
                       (if (null lst) acc
                           (rev-acc (cdr lst) (cons (car lst) acc)))))
              (rev-acc arg nil))))
         ;; String/Symbol creation (use cached symbols)
         ((if (symbolp op) (op=make-string-from-vector op) nil)
          (let ((vec (h0-eval (cadr expr) env fenv)))
            (make-string-from-vector vec)))
         ((if (symbolp op) (op=make-symbol-from-string op) nil)
          (let ((str (h0-eval (cadr expr) env fenv)))
            (make-symbol-from-string str)))
         ;; CAAR - (car (car x))
         ((if (symbolp op) (op=caar op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (car (car arg))))
         ;; CDAR - (cdr (car x))
         ((if (symbolp op) (op=cdar op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (cdr (car arg))))
         ;; NTH - get nth element of list
         ((if (symbolp op) (op=nth op) nil)
          (let* ((n (h0-eval (cadr expr) env fenv))
                 (lst (h0-eval (caddr expr) env fenv)))
            (labels ((nth-helper (i l)
                       (if (= i #x0)
                           (car l)
                           (nth-helper (- i #x1) (cdr l)))))
              (nth-helper n lst))))
         ;; LOGNOT - bitwise NOT (two's complement)
         ((if (symbolp op) (op=lognot op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (lognot arg)))
         ;; /= - not equal comparison
         ((if (symbolp op) (op=neq op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (if (= left right) nil t)))
         ;; LAMBDA - create closure capturing current environment
         ((if (symbolp op) (op=lambda op) nil)
          (let ((params (cadr expr))
                (body (caddr expr)))
            ;; Return closure: (CLOSURE-TAG params body captured-env)
            ;; Use interned symbol for reliable eq comparison
            (list (intern "CLOSURE-TAG") params body env)))
         ;; LABELS - local recursive function definitions
         ;; (labels ((f1 (x) body1) (f2 (y) body2)) body...)
         ;; Creates closures that can call each other recursively
         ((if (symbolp op) (op=labels op) nil)
          (let* ((bindings (cadr expr))
                 (body-forms (cddr expr))
                 ;; Create closures for all functions, they share the extended fenv
                 (labels-fenv (h0-eval-labels-bindings bindings fenv env)))
            ;; Evaluate body with extended fenv
            (h0-eval-progn body-forms env labels-fenv)))
         ;; FLET - local non-recursive function definitions
         ;; (flet ((f1 (x) body1)) body...) - f1 cannot call itself
         ((if (symbolp op) (op=flet op) nil)
          (let* ((bindings (cadr expr))
                 (body-forms (cddr expr))
                 ;; For flet, closures capture outer fenv (not recursive)
                 (flet-fenv (h0-eval-flet-bindings bindings fenv env)))
            (h0-eval-progn body-forms env flet-fenv)))
         ;; FUNCALL - call a function value (closure or symbol)
         ;; Supports: (funcall fn-name args...) where fn-name is a symbol
         ;;           (funcall closure args...) where closure is a lambda
         ;;           (funcall var args...) where var holds a closure
         ((if (symbolp op) (op=funcall op) nil)
          (let* ((fn-expr (cadr expr))
                 (args (h0-eval-list (cddr expr) env fenv)))
            (cond
              ;; Quoted symbol: (funcall 'foo ...) - look up in fenv
              ((and (consp fn-expr) (op=quote (car fn-expr)))
               (let* ((fn-sym (cadr fn-expr))
                      (fn-entry (fenv-lookup fn-sym fenv)))
                 (if fn-entry
                     (if (keywordp (car fn-entry))
                         (h0-eval-builtin (cdr fn-entry) args fenv)
                         (let* ((params (car fn-entry))
                                (body (cdr fn-entry))
                                (new-env (bind-lambda-args params args nil fenv)))
                           (h0-eval body new-env fenv)))
                     (fatal-error "h0-eval: FUNCALL unknown function"))))
              ;; Bare symbol: (funcall foo ...) - try var lookup first, then fenv
              ((symbolp fn-expr)
               (let ((var-entry (env-lookup fn-expr env)))
                 (if var-entry
                     ;; Found in local env - should be a closure
                     (let ((fn (cdr var-entry)))
                       (if (and (consp fn) (eq (car fn) (intern "CLOSURE-TAG")))
                           (let* ((params (cadr fn))
                                  (body (caddr fn))
                                  (captured-env (cadddr fn))
                                  (new-env (bind-lambda-args params args captured-env fenv)))
                             (h0-eval body new-env fenv))
                           ;; Maybe it's a symbol - look up in fenv
                           (if (symbolp fn)
                               (let ((fn-entry (fenv-lookup fn fenv)))
                                 (if fn-entry
                                     (if (keywordp (car fn-entry))
                                         (h0-eval-builtin (cdr fn-entry) args fenv)
                                         (let* ((params (car fn-entry))
                                                (body (cdr fn-entry))
                                                (new-env (bind-lambda-args params args nil fenv)))
                                           (h0-eval body new-env fenv)))
                                     (fatal-error "h0-eval: FUNCALL unknown function")))
                               (fatal-error "h0-eval: FUNCALL on non-closure"))))
                     ;; Not in env - try fenv directly (Lisp-2 style)
                     (let ((fn-entry (fenv-lookup fn-expr fenv)))
                       (if fn-entry
                           (if (keywordp (car fn-entry))
                               (h0-eval-builtin (cdr fn-entry) args fenv)
                               (let* ((params (car fn-entry))
                                      (body (cdr fn-entry))
                                      (new-env (bind-lambda-args params args nil fenv)))
                                 (h0-eval body new-env fenv)))
                           (fatal-error "h0-eval: FUNCALL unknown function"))))))
              ;; Expression that evaluates to closure
              (t
               (let ((fn (h0-eval fn-expr env fenv)))
                 (if (and (consp fn) (eq (car fn) (intern "CLOSURE-TAG")))
                     (let* ((params (cadr fn))
                            (body (caddr fn))
                            (captured-env (cadddr fn))
                            (new-env (bind-lambda-args params args captured-env fenv)))
                       (h0-eval body new-env fenv))
                     (fatal-error "h0-eval: FUNCALL on non-closure")))))))
         ;; Function call - look up in fenv
         (t
          (let ((fn-entry (fenv-lookup op fenv)))
            (if fn-entry
                ;; Check for builtin marker: fn-entry = (:builtin . impl-symbol)
                ;; Use keywordp to detect - builtins have keyword as car
                ;; keywordp is a primitive recognized by bootstrap compiler
                (if (keywordp (car fn-entry))
                    ;; Builtin function - dispatch based on name
                    (let ((builtin-name (cdr fn-entry))
                          (args (h0-eval-list (cdr expr) env fenv)))
                      (h0-eval-builtin builtin-name args fenv))
                    ;; User-defined function: fn-entry = (params . body)
                    (let* ((params (car fn-entry))
                           (body (cdr fn-entry))
                           (args (h0-eval-list (cdr expr) env fenv))
                           ;; Use bind-lambda-args to support &key parameters
                           (new-env (bind-lambda-args params args nil fenv)))
                      (h0-eval body new-env fenv)))
                ;; Unknown function
                (fatal-error "h0-eval: unknown function")))))))
    ;; Unknown expression type
    (t (fatal-error "h0-eval: unknown expression type"))))

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
         ;; Compiler functions (IDs 1, 4-7)
         ;; Note: IDs 2,3 (h0-codegen, h0-linearize) REMOVED - use codegen-fn-reg-alloc
         (cons (intern "H0-COMPILE") 1)
         (cons (intern "DELIVER-WITH-IMPORTS-AND-HEAP") 4)
         (cons (intern "READ-ALL") 5)
         (cons (intern "NATIVE-READ-FILE") 6)
         (cons (intern "COLLECT-DEFUNS") 7)
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
         (cons (intern "REG") 50))))

;; Lookup dispatch ID in table
(defun find-builtin-id (name table)
  (if (null table)
      nil
      (if (eq name (caar table))
          (cdar table)
          (find-builtin-id name (cdr table)))))

;;; ============================================================
;;; Keyword Normalization for SBCL/habu0 Boundary
;;; ============================================================
;;
;; Problem: In SBCL, keywords like :x0 are SBCL keyword symbols.
;; In habu0 native, keywords should be habu0-interned with tag 7.
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
   If kw is already a habu0 keyword (tag 7, symbolp = nil), return as-is."
  (if (keywordp kw)
      (if (symbolp kw)
          ;; SBCL keyword: symbol-name works, re-intern in habu0
          (intern-keyword (symbol-name kw))
          ;; Already habu0 keyword (tag 7, not symbolp)
          kw)
      ;; Not a keyword, return as-is
      kw))

(defun normalize-args (args)
  "Normalize all keywords in an argument list.
   Recursively handles nested lists for keyword args like :offset 8."
  (if (null args)
      nil
      (let ((arg (car args)))
        (cons (if (consp arg)
                  (normalize-args arg)  ; Handle nested structures
                  (normalize-keyword arg))
              (normalize-args (cdr args))))))

;;; habu0-reg: Register lookup using eq comparison
;;; After normalization, keywords are habu0-interned, so eq works.
;;; This replaces arm64:reg's string comparison at the boundary.
(defun habu0-reg (r)
  "Convert normalized register keyword to number using eq.
   Called after normalize-keyword ensures r is habu0-interned."
  (cond
    ;; General purpose registers x0-x30
    ((eq r *kw-x0*) 0)   ((eq r *kw-x1*) 1)   ((eq r *kw-x2*) 2)   ((eq r *kw-x3*) 3)
    ((eq r *kw-x4*) 4)   ((eq r *kw-x5*) 5)   ((eq r *kw-x6*) 6)   ((eq r *kw-x7*) 7)
    ((eq r *kw-x8*) 8)   ((eq r *kw-x9*) 9)   ((eq r *kw-x10*) 10) ((eq r *kw-x11*) 11)
    ((eq r *kw-x12*) 12) ((eq r *kw-x13*) 13) ((eq r *kw-x14*) 14) ((eq r *kw-x15*) 15)
    ((eq r *kw-x16*) 16) ((eq r *kw-x17*) 17) ((eq r *kw-x18*) 18) ((eq r *kw-x19*) 19)
    ((eq r *kw-x20*) 20) ((eq r *kw-x21*) 21) ((eq r *kw-x22*) 22) ((eq r *kw-x23*) 23)
    ((eq r *kw-x24*) 24) ((eq r *kw-x25*) 25) ((eq r *kw-x26*) 26) ((eq r *kw-x27*) 27)
    ((eq r *kw-x28*) 28) ((eq r *kw-x29*) 29) ((eq r *kw-x30*) 30)
    ;; Special registers
    ((eq r *kw-sp*) 31)
    ((eq r *kw-xzr*) 31)
    ((eq r *kw-lr*) 30)
    ((eq r *kw-fp*) 29)
    ;; Habu-specific aliases
    ((eq r *kw-env*) 20)
    ((eq r *kw-closure*) 24)
    ((eq r *kw-code-base*) 26)
    ((eq r *kw-gc*) 27)
    ((eq r *kw-heap*) 28)
    ;; Unknown register - error
    (t (error "habu0-reg: unknown register keyword"))))

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
        (match id
          ;; Compiler functions (don't need normalization - no keywords)
          (1 (h0-compile (car args) (cadr args) (caddr args)))
          ;; IDs 2,3 (h0-codegen, h0-linearize) REMOVED - use codegen-fn-reg-alloc
          (4 (deliver-with-imports-and-heap (car args) (cadr args) (caddr args) (cadddr args)))
          (5 (read-all (car args)))
          (6 (native-read-file (car args)))
          (7 (collect-defuns (car args) (cadr args)))
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
          (_ (fatal-error "h0-eval-builtin: unhandled dispatch ID"))))))

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

;; Register a compile-time symbol in the runtime intern table
;; This ensures that when the reader interns the same name, it gets
;; the same symbol object (enabling eq comparison to work)
;; NOTE: Takes name STRING and symbol separately because (symbol-name sym)
;; at runtime would use SBCL's symbol-name, not habu's, which creates
;; an incompatible string object.
(defun register-symbol (name sym)
  (set-intern-table (cons (cons name sym) (get-intern-table))))

;; Initialize compile ops - register symbols in intern table for eq comparison
(defun init-compile-ops ()
  ;; Initialize all operator symbols using quoted symbols
  ;; IMPORTANT: Use 'SYMBOL not (intern "SYMBOL") because:
  ;; - Quoted symbols are created at compile-time by SBCL
  ;; - They are the SAME objects used in compiled code like '(+ 1 2)
  ;; - Runtime (intern ...) creates DIFFERENT symbol objects
  ;; - eq comparison requires object identity, not name equality
  (setq *op-quote* 'quote)
  (setq *op-if* 'if)
  (setq *op-let* 'let)
  (setq *op-let-star* 'let*)
  (setq *op-defun* 'defun)
  (setq *op-defvar* 'defvar)
  (setq *op-while* 'while)
  (setq *op-progn* 'progn)
  (setq *op-cond* 'cond)
  (setq *op-t* 't)
  (setq *op-plus* '+)
  (setq *op-minus* '-)
  (setq *op-mul* '*)
  (setq *op-div* '/)
  (setq *op-mod* 'mod)
  (setq *op-eq-num* '=)
  (setq *op-lt* '<)
  (setq *op-gt* '>)
  (setq *op-le* '<=)
  (setq *op-ge* '>=)
  (setq *op-cons* 'cons)
  (setq *op-car* 'car)
  (setq *op-cdr* 'cdr)
  (setq *op-cadr* 'cadr)
  (setq *op-cddr* 'cddr)
  (setq *op-caddr* 'caddr)
  (setq *op-cadddr* 'cadddr)
  (setq *op-null* 'null)
  (setq *op-consp* 'consp)
  (setq *op-list* 'list)
  (setq *op-not* 'not)
  (setq *op-and* 'and)
  (setq *op-or* 'or)
  (setq *op-defpackage* 'defpackage)
  (setq *op-in-package* 'in-package)
  (setq *op-case* 'case)
  (setq *op-when* 'when)
  (setq *op-unless* 'unless)
  (setq *op-declaim* 'declaim)
  (setq *op-setq* 'setq)
  (setq *op-error* 'error)
  ;; Additional operators
  (setq *op-symbolp* 'symbolp)
  (setq *op-numberp* 'numberp)
  (setq *op-stringp* 'stringp)
  (setq *op-keywordp* 'keywordp)
  (setq *op-string-length* 'string-length)
  (setq *op-string-ref* 'string-ref)
  (setq *op-char-at* 'char-at)
  (setq *op-string=* 'string=)
  (setq *op-symbol-name* 'symbol-name)
  (setq *op-keyword-name* 'keyword-name)
  (setq *op-logand* 'logand)
  (setq *op-logior* 'logior)
  (setq *op-ash* 'ash)
  (setq *op-eq* 'eq)
  (setq *op-eql* 'eql)
  (setq *op-get-tag* 'get-tag)
  (setq *op-set-tag* 'set-tag)
  (setq *op-length* 'length)
  (setq *op-make-vector* 'make-vector)
  (setq *op-vector-length* 'vector-length)
  (setq *op-vector-set* 'vector-set)
  (setq *op-vector-ref* 'vector-ref)
  (setq *op-reverse* 'reverse)
  (setq *op-make-string-from-vector* 'make-string-from-vector)
  (setq *op-make-symbol-from-string* 'make-symbol-from-string)
  (setq *op-caar* 'caar)
  (setq *op-cdar* 'cdar)
  (setq *op-nth* 'nth)
  (setq *op-lognot* 'lognot)
  (setq *op-neq* '/=)
  (setq *op-lambda* 'lambda)
  (setq *op-funcall* 'funcall)
  (setq *op-setcar* 'setcar)
  (setq *op-setcdr* 'setcdr)
  (setq *op-dolist* 'dolist)
  (setq *op-flet* 'flet)
  (setq *op-labels* 'labels)
  (setq *op-mapcar* 'mapcar)
  (setq *op-ecase* 'ecase)
  (setq *op-listp* 'listp)
  (setq *op-nil* 'nil)
  (setq *op-otherwise* 'otherwise)
  ;; Initialize runtime keywords for eq comparison
  (setq *kw-offset* (intern-keyword "OFFSET"))
  (setq *kw-imm* (intern-keyword "IMM"))
  ;; Initialize register keywords for eq comparison in habu0-reg
  (setq *kw-x0* (intern-keyword "X0"))
  (setq *kw-x1* (intern-keyword "X1"))
  (setq *kw-x2* (intern-keyword "X2"))
  (setq *kw-x3* (intern-keyword "X3"))
  (setq *kw-x4* (intern-keyword "X4"))
  (setq *kw-x5* (intern-keyword "X5"))
  (setq *kw-x6* (intern-keyword "X6"))
  (setq *kw-x7* (intern-keyword "X7"))
  (setq *kw-x8* (intern-keyword "X8"))
  (setq *kw-x9* (intern-keyword "X9"))
  (setq *kw-x10* (intern-keyword "X10"))
  (setq *kw-x11* (intern-keyword "X11"))
  (setq *kw-x12* (intern-keyword "X12"))
  (setq *kw-x13* (intern-keyword "X13"))
  (setq *kw-x14* (intern-keyword "X14"))
  (setq *kw-x15* (intern-keyword "X15"))
  (setq *kw-x16* (intern-keyword "X16"))
  (setq *kw-x17* (intern-keyword "X17"))
  (setq *kw-x18* (intern-keyword "X18"))
  (setq *kw-x19* (intern-keyword "X19"))
  (setq *kw-x20* (intern-keyword "X20"))
  (setq *kw-x21* (intern-keyword "X21"))
  (setq *kw-x22* (intern-keyword "X22"))
  (setq *kw-x23* (intern-keyword "X23"))
  (setq *kw-x24* (intern-keyword "X24"))
  (setq *kw-x25* (intern-keyword "X25"))
  (setq *kw-x26* (intern-keyword "X26"))
  (setq *kw-x27* (intern-keyword "X27"))
  (setq *kw-x28* (intern-keyword "X28"))
  (setq *kw-x29* (intern-keyword "X29"))
  (setq *kw-x30* (intern-keyword "X30"))
  (setq *kw-sp* (intern-keyword "SP"))
  (setq *kw-xzr* (intern-keyword "XZR"))
  (setq *kw-lr* (intern-keyword "LR"))
  (setq *kw-fp* (intern-keyword "FP"))
  (setq *kw-env* (intern-keyword "ENV"))
  (setq *kw-closure* (intern-keyword "CLOSURE"))
  (setq *kw-code-base* (intern-keyword "CODE-BASE"))
  (setq *kw-gc* (intern-keyword "GC"))
  (setq *kw-heap* (intern-keyword "HEAP"))
  ;; Register all operator symbols in intern table so reader returns same objects
  ;; Each call passes (name-string symbol) to avoid calling symbol-name at runtime
  (register-symbol "QUOTE" 'quote)
  (register-symbol "IF" 'if)
  (register-symbol "LET" 'let)
  (register-symbol "LET*" 'let*)
  (register-symbol "DEFUN" 'defun)
  (register-symbol "DEFVAR" 'defvar)
  (register-symbol "WHILE" 'while)
  (register-symbol "PROGN" 'progn)
  (register-symbol "COND" 'cond)
  (register-symbol "T" 't)
  (register-symbol "+" '+)
  (register-symbol "-" '-)
  (register-symbol "*" '*)
  (register-symbol "/" '/)
  (register-symbol "MOD" 'mod)
  (register-symbol "=" '=)
  (register-symbol "<" '<)
  (register-symbol ">" '>)
  (register-symbol "<=" '<=)
  (register-symbol ">=" '>=)
  (register-symbol "CONS" 'cons)
  (register-symbol "CAR" 'car)
  (register-symbol "CDR" 'cdr)
  (register-symbol "CADR" 'cadr)
  (register-symbol "CDDR" 'cddr)
  (register-symbol "CADDR" 'caddr)
  (register-symbol "CADDDR" 'cadddr)
  (register-symbol "NULL" 'null)
  (register-symbol "CONSP" 'consp)
  (register-symbol "LIST" 'list)
  (register-symbol "NOT" 'not)
  (register-symbol "AND" 'and)
  (register-symbol "OR" 'or)
  (register-symbol "DEFPACKAGE" 'defpackage)
  (register-symbol "IN-PACKAGE" 'in-package)
  (register-symbol "CASE" 'case)
  (register-symbol "WHEN" 'when)
  (register-symbol "UNLESS" 'unless)
  (register-symbol "DECLAIM" 'declaim)
  (register-symbol "SETQ" 'setq)
  (register-symbol "ERROR" 'error)
  (register-symbol "SYMBOLP" 'symbolp)
  (register-symbol "NUMBERP" 'numberp)
  (register-symbol "STRINGP" 'stringp)
  (register-symbol "KEYWORDP" 'keywordp)
  (register-symbol "STRING-LENGTH" 'string-length)
  (register-symbol "STRING-REF" 'string-ref)
  (register-symbol "CHAR-AT" 'char-at)
  (register-symbol "STRING=" 'string=)
  (register-symbol "SYMBOL-NAME" 'symbol-name)
  (register-symbol "KEYWORD-NAME" 'keyword-name)
  (register-symbol "LOGAND" 'logand)
  (register-symbol "LOGIOR" 'logior)
  (register-symbol "ASH" 'ash)
  (register-symbol "EQ" 'eq)
  (register-symbol "EQL" 'eql)
  (register-symbol "GET-TAG" 'get-tag)
  (register-symbol "SET-TAG" 'set-tag)
  (register-symbol "LENGTH" 'length)
  (register-symbol "MAKE-VECTOR" 'make-vector)
  (register-symbol "VECTOR-LENGTH" 'vector-length)
  (register-symbol "VECTOR-SET" 'vector-set)
  (register-symbol "VECTOR-REF" 'vector-ref)
  (register-symbol "REVERSE" 'reverse)
  (register-symbol "MAKE-STRING-FROM-VECTOR" 'make-string-from-vector)
  (register-symbol "MAKE-SYMBOL-FROM-STRING" 'make-symbol-from-string)
  (register-symbol "CAAR" 'caar)
  (register-symbol "CDAR" 'cdar)
  (register-symbol "NTH" 'nth)
  (register-symbol "LOGNOT" 'lognot)
  (register-symbol "/=" '/=)
  (register-symbol "LAMBDA" 'lambda)
  (register-symbol "FUNCALL" 'funcall)
  (register-symbol "SETCAR" 'setcar)
  (register-symbol "SETCDR" 'setcdr)
  (register-symbol "DOLIST" 'dolist)
  (register-symbol "FLET" 'flet)
  (register-symbol "LABELS" 'labels)
  (register-symbol "MAPCAR" 'mapcar)
  (register-symbol "ECASE" 'ecase)
  (register-symbol "LISTP" 'listp)
  ;; Note: 'nil is literally NIL (not a symbol), so don't register it
  (register-symbol "OTHERWISE" 'otherwise)
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

;; IR tag constants (using numbers to avoid symbol-name issues in native code)
(defun ir-tag-lit () #x1)
(defun ir-tag-var () #x2)
(defun ir-tag-add () #x3)
(defun ir-tag-sub () #x4)
(defun ir-tag-mul () #x5)
(defun ir-tag-div () #x6)
(defun ir-tag-mod () #x7)
(defun ir-tag-cmp-eq () #x8)
(defun ir-tag-cmp-lt () #x9)
(defun ir-tag-cmp-gt () #xA)
(defun ir-tag-cmp-le () #xB)
(defun ir-tag-cmp-ge () #xC)
(defun ir-tag-if () #xD)
(defun ir-tag-cons () #xE)
(defun ir-tag-car () #xF)
(defun ir-tag-cdr () #x10)
(defun ir-tag-null () #x11)
(defun ir-tag-let () #x12)
(defun ir-tag-progn () #x13)
;; Additional tags for self-hosting
(defun ir-tag-str-len () #x14)    ; string-length
(defun ir-tag-str-ref () #x15)    ; string-ref
(defun ir-tag-eq () #x16)         ; eq
(defun ir-tag-consp () #x17)      ; consp
(defun ir-tag-symbolp () #x18)    ; symbolp
(defun ir-tag-numberp () #x19)    ; numberp (fixnump)
(defun ir-tag-stringp () #x1A)    ; stringp
(defun ir-tag-logand () #x1B)     ; logand
(defun ir-tag-logior () #x1C)     ; logior
(defun ir-tag-ash () #x1D)        ; ash (arithmetic shift)
(defun ir-tag-not () #x1E)        ; not (boolean)
(defun ir-tag-str-lit () #x1F)    ; string literal (heap-allocated)
(defun ir-tag-kw-lit () #x20)     ; keyword literal (heap-allocated)
(defun ir-tag-keywordp () #x21)   ; keywordp predicate
(defun ir-tag-lambda () #x22)     ; lambda (closure creation)
(defun ir-tag-funcall () #x23)    ; funcall (closure invocation)
(defun ir-tag-setq () #x24)       ; setq (variable assignment)
(defun ir-tag-length () #x25)     ; length (list length)
(defun ir-tag-string-eq () #x26)  ; string= comparison
(defun ir-tag-symbol-name () #x27) ; symbol-name extraction
(defun ir-tag-make-vector () #x28) ; make-vector allocation
(defun ir-tag-vector-ref () #x29)  ; vector-ref access
(defun ir-tag-vector-set () #x2A)  ; vector-set mutation
(defun ir-tag-vector-length () #x2B) ; vector-length
(defun ir-tag-quote-sym () #x2C)  ; quoted symbol literal
(defun ir-tag-eql () #x2D)        ; eql comparison
(defun ir-tag-get-tag () #x2E)    ; get-tag primitive
(defun ir-tag-set-tag () #x32)    ; set-tag primitive (change pointer tag bits)
(defun ir-tag-make-string-from-vector () #x2F) ; make-string-from-vector
(defun ir-tag-make-symbol-from-string () #x30) ; make-symbol-from-string
(defun ir-tag-error () #x31)      ; error primitive
(defun ir-tag-lognot () #x33)     ; lognot (bitwise NOT via MVN)
(defun ir-tag-keyword-name () #x34) ; keyword-name extraction

;; Check if IR node has a specific tag (numeric comparison)
(defun h0-has-tag-n (ir tag)
  (if (consp ir)
      (= (car ir) tag)
      nil))


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
    ((numberp expr) (list (ir-tag-lit) expr))
    ;; nil is 0 (both Lisp nil and NIL symbol)
    ((null expr) (list (ir-tag-lit) #x0))
    ((if (symbolp expr) (op=t expr) nil) (list (ir-tag-lit) #x1))
    ;; String literals - allocate on heap
    ((stringp expr) (list (ir-tag-str-lit) expr))
    ;; Keyword literals - allocate on heap (self-evaluating)
    ((keywordp expr) (list (ir-tag-kw-lit) expr))
    ;; Symbols - variable lookup
    ((symbolp expr)
     (let ((result (c-env-lookup expr env)))
       (if result
           (list (ir-tag-var) (car result))  ;; Extract offset from (cons offset nil)
           (fatal-error-ir "h0-compile: Unknown symbol"))))
    ;; Lists - special forms or function calls
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; Quote
         ((if (symbolp op) (op=quote op) nil)
          (let ((val (cadr expr)))
            (cond
              ((numberp val) (list (ir-tag-lit) val))
              ;; Keywords MUST be checked before symbolp (keywords are symbols)
              ((keywordp val) (list (ir-tag-kw-lit) val))
              ((symbolp val) (list (ir-tag-quote-sym) val))
              ((null val) (list (ir-tag-lit) #x0))
              (t (fatal-error-ir "h0-compile: Unsupported quote type")))))
         ;; If
         ((if (symbolp op) (op=if op) nil)
          (let* ((test-ir (h0-compile (cadr expr) env fenv))
                 (then-ir (h0-compile (caddr expr) env fenv))
                 (else-ir (if (cadddr expr)
                              (h0-compile (cadddr expr) env fenv)
                              (list (ir-tag-lit) #x0))))
            (list (ir-tag-if) test-ir then-ir else-ir)))
         ;; Let
         ((if (symbolp op) (op=let op) nil)
          (h0-compile-let (cadr expr) (caddr expr) env fenv))
         ;; Let*
         ((if (symbolp op) (op=let-star op) nil)
          (h0-compile-let (cadr expr) (caddr expr) env fenv))
         ;; Setq
         ((if (symbolp op) (op=setq op) nil)
          (let* ((var-sym (cadr expr))
                 (val-ir (h0-compile (caddr expr) env fenv))
                 (result (c-env-lookup var-sym env)))
            (if result
                (list (ir-tag-setq) (car result) val-ir)
                (fatal-error-ir "h0-compile: SETQ unknown variable"))))
         ;; Progn
         ((if (symbolp op) (op=progn op) nil)
          (h0-compile-progn (cdr expr) env fenv))
         ;; Defun returns nil during compilation
         ((if (symbolp op) (op=defun op) nil)
          (list (ir-tag-lit) #x0))
         ;; Defvar returns nil during compilation (global var is runtime)
         ((if (symbolp op) (op=defvar op) nil)
          (list (ir-tag-lit) #x0))
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
            (list (ir-tag-mul) l r)))
         ((if (symbolp op) (op=div op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-div) l r)))
         ((if (symbolp op) (op=mod op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-mod) l r)))
         ;; Comparisons
         ((if (symbolp op) (op=eq-num op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-eq) l r)))
         ((if (symbolp op) (op=lt op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-lt) l r)))
         ((if (symbolp op) (op=gt op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-gt) l r)))
         ((if (symbolp op) (op=le op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-le) l r)))
         ((if (symbolp op) (op=ge op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-ge) l r)))
         ;; List operations
         ((if (symbolp op) (op=cons op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cons) l r)))
         ((if (symbolp op) (op=car op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-car) v)))
         ((if (symbolp op) (op=cdr op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-cdr) v)))
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
            (list (ir-tag-null) v)))
         ;; String operations
         ((if (symbolp op) (op=string-length op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-str-len) v)))
         ((if (symbolp op) (op=string-ref op) nil)
          (let* ((str (h0-compile (cadr expr) env fenv))
                 (idx (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-str-ref) str idx)))
         ;; CHAR-AT - safe string-ref that returns 0 beyond end
         ((if (symbolp op) (op=char-at op) nil)
          (let* ((str-expr (cadr expr))
                 (pos-expr (caddr expr))
                 (str-ir (h0-compile str-expr env fenv))
                 (pos-ir (h0-compile pos-expr env fenv))
                 (len-ir (list (ir-tag-str-len) str-ir))
                 (test-ir (list (ir-tag-cmp-lt) pos-ir len-ir))
                 (then-ir (list (ir-tag-str-ref) str-ir pos-ir))
                 (else-ir (list (ir-tag-lit) #x0)))
            (list (ir-tag-if) test-ir then-ir else-ir)))
         ;; STRING= - string equality comparison
         ((if (symbolp op) (op=string= op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-string-eq) l r)))
         ;; SYMBOL-NAME - extract name string from symbol
         ((if (symbolp op) (op=symbol-name op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-symbol-name) v)))
         ;; KEYWORD-NAME - extract name string from keyword
         ((if (symbolp op) (op=keyword-name op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-keyword-name) v)))
         ;; Vector operations
         ((if (symbolp op) (op=make-vector op) nil)
          (let ((size (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-make-vector) size)))
         ((if (symbolp op) (op=vector-ref op) nil)
          (let* ((vec (h0-compile (cadr expr) env fenv))
                 (idx (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-vector-ref) vec idx)))
         ((if (symbolp op) (op=vector-set op) nil)
          (let* ((vec (h0-compile (cadr expr) env fenv))
                 (idx (h0-compile (caddr expr) env fenv))
                 (val (h0-compile (cadddr expr) env fenv)))
            (list (ir-tag-vector-set) vec idx val)))
         ((if (symbolp op) (op=vector-length op) nil)
          (let ((vec (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-vector-length) vec)))
         ;; Type predicates
         ((if (symbolp op) (op=eq op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-eq) l r)))
         ((if (symbolp op) (op=consp op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-consp) v)))
         ((if (symbolp op) (op=symbolp op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-symbolp) v)))
         ((if (symbolp op) (op=numberp op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-numberp) v)))
         ((if (symbolp op) (op=stringp op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-stringp) v)))
         ((if (symbolp op) (op=keywordp op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-keywordp) v)))
         ;; Bitwise operations
         ((if (symbolp op) (op=logand op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-logand) l r)))
         ((if (symbolp op) (op=logior op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-logior) l r)))
         ((if (symbolp op) (op=ash op) nil)
          (let* ((val (h0-compile (cadr expr) env fenv))
                 (shift (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-ash) val shift)))
         ;; Boolean not
         ((if (symbolp op) (op=not op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-not) v)))
         ;; OR - expand to if chain
         ((if (symbolp op) (op=or op) nil)
          (h0-compile-or (cdr expr) env fenv))
         ;; AND - expand to if chain
         ((if (symbolp op) (op=and op) nil)
          (h0-compile-and (cdr expr) env fenv))
         ;; LENGTH - list length
         ((if (symbolp op) (op=length op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-length) v)))
         ;; COND - expand to nested IFs
         ((if (symbolp op) (op=cond op) nil)
          (h0-compile-cond (cdr expr) env fenv))
         ;; WHEN - expand to (if test (progn body...))
         ((if (symbolp op) (op=when op) nil)
          (let* ((test-ir (h0-compile (cadr expr) env fenv))
                 (body-ir (h0-compile-progn (cddr expr) env fenv))
                 (else-ir (list (ir-tag-lit) #x0)))
            (list (ir-tag-if) test-ir body-ir else-ir)))
         ;; UNLESS - expand to (if (not test) (progn body...))
         ((if (symbolp op) (op=unless op) nil)
          (let* ((test-ir (h0-compile (cadr expr) env fenv))
                 (not-test-ir (list (ir-tag-not) test-ir))
                 (body-ir (h0-compile-progn (cddr expr) env fenv))
                 (else-ir (list (ir-tag-lit) #x0)))
            (list (ir-tag-if) not-test-ir body-ir else-ir)))
         ;; CASE - expand to cond with eql comparisons
         ((if (symbolp op) (op=case op) nil)
          (let* ((keyform (cadr expr))
                 (clauses (cddr expr))
                 (key-var (intern "#:CASE-KEY")))
            (h0-compile
             (list 'let (list (list key-var keyform))
                   (cons 'cond (h0-expand-case-clauses key-var clauses)))
             env fenv)))
         ;; ECASE - like CASE but signals error if no clause matches
         ((if (symbolp op) (op=ecase op) nil)
          (let* ((keyform (cadr expr))
                 (clauses (cddr expr))
                 (key-var (intern "#:CASE-KEY"))
                 (clauses-with-error
                  (h0-append clauses
                             (list (list 't (list 'error "ecase: no matching clause"))))))
            (h0-compile
             (list 'let (list (list key-var keyform))
                   (cons 'cond (h0-expand-case-clauses key-var clauses-with-error)))
             env fenv)))
         ;; DOLIST - expand to labels loop
         ((if (symbolp op) (op=dolist op) nil)
          (let* ((binding (cadr expr))
                 (var (car binding))
                 (list-expr (cadr binding))
                 (body (cddr expr))
                 (list-var (intern "#:DOLIST-LIST"))
                 (loop-fn (intern "#:DOLIST-LOOP"))
                 (expanded
                  (list 'let (list (list list-var list-expr))
                        (list 'labels
                              (list (list loop-fn (list)
                                          (list 'when list-var
                                                (list 'let (list (list var (list 'car list-var)))
                                                      (cons 'progn
                                                            (append body
                                                                    (list (list 'setq list-var (list 'cdr list-var))
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
            (list (ir-tag-lambda) params body-ir free-vars free-offsets)))
         ;; FUNCALL - call function value
         ((if (symbolp op) (op=funcall op) nil)
          (let* ((fn-ir (h0-compile (cadr expr) env fenv))
                 (args (cddr expr))
                 (args-ir (h0-compile-args args env fenv)))
            (list (ir-tag-funcall) fn-ir args-ir)))
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
            (list (ir-tag-car) (list (ir-tag-cdr) v))))
         ;; CDDR - (cdr (cdr x))
         ((if (symbolp op) (op=cddr op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-cdr) (list (ir-tag-cdr) v))))
         ;; CADDR - (car (cdr (cdr x)))
         ((if (symbolp op) (op=caddr op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-car) (list (ir-tag-cdr) (list (ir-tag-cdr) v)))))
         ;; CADDDR - (car (cdr (cdr (cdr x))))
         ((if (symbolp op) (op=cadddr op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-car) (list (ir-tag-cdr) (list (ir-tag-cdr) (list (ir-tag-cdr) v))))))
         ;; CAAR - (car (car x))
         ((if (symbolp op) (op=caar op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-car) (list (ir-tag-car) v))))
         ;; CDAR - (cdr (car x))
         ((if (symbolp op) (op=cdar op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-cdr) (list (ir-tag-car) v))))
         ;; NTH - expand to nested CDRs and CAR
         ((if (symbolp op) (op=nth op) nil)
          (h0-compile-nth (cadr expr) (caddr expr) env fenv))
         ;; LOGNOT - use MVN instruction (bitwise NOT)
         ((if (symbolp op) (op=lognot op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-lognot) v)))
         ;; EQL - equal for numbers and symbols
         ((if (symbolp op) (op=eql op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-eql) l r)))
         ;; GET-TAG - extract tag from tagged value
         ((if (symbolp op) (op=get-tag op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-get-tag) v)))
         ;; SET-TAG - change tag bits on a pointer value
         ((if (symbolp op) (op=set-tag op) nil)
          (let ((val (h0-compile (cadr expr) env fenv))
                (new-tag (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-set-tag) val new-tag)))
         ;; MAKE-STRING-FROM-VECTOR - create string from vector of chars
         ((if (symbolp op) (op=make-string-from-vector op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-make-string-from-vector) v)))
         ;; MAKE-SYMBOL-FROM-STRING - create symbol from string
         ((if (symbolp op) (op=make-symbol-from-string op) nil)
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-make-symbol-from-string) v)))
         ;; ERROR - signal error and crash
         ((if (symbolp op) (op=error op) nil)
          (list (ir-tag-error)))
         ;; /= (not equal)
         ((if (symbolp op) (op=neq op) nil)
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-not) (list (ir-tag-cmp-eq) l r))))
         ;; Default case - if op is a cons (e.g., lambda expression), compile as funcall
         ;; otherwise it's an unknown operator
         (t
          (if (consp op)
              (let* ((fn-ir (h0-compile op env fenv))
                     (args (cdr expr))
                     (args-ir (h0-compile-args args env fenv)))
                (list (ir-tag-funcall) fn-ir args-ir))
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
                (if (and (h0-has-tag-n left-ir (ir-tag-lit)) (h0-has-tag-n right-ir (ir-tag-lit)))
                    (list (ir-tag-lit) (+ (cadr left-ir) (cadr right-ir)))
                    (list (ir-tag-add) left-ir right-ir)))
              ;; More than two: recurse - (+ a b c ...) => (+ a (+ b c ...))
              (let* ((left-ir (h0-compile (car args) env fenv))
                     (right-ir (h0-compile-add (cdr args) env fenv)))
                (if (and (h0-has-tag-n left-ir (ir-tag-lit)) (h0-has-tag-n right-ir (ir-tag-lit)))
                    (list (ir-tag-lit) (+ (cadr left-ir) (cadr right-ir)))
                    (list (ir-tag-add) left-ir right-ir)))))))

;; Compile subtraction with constant folding (handles variadic args)
(defun h0-compile-sub (args env fenv)
  (if (null args)
      (fatal-error-ir "h0-compile-sub: Empty subtraction")
      (if (null (cdr args))
          ;; Unary minus
          (let ((arg-ir (h0-compile (car args) env fenv)))
            (if (h0-has-tag-n arg-ir (ir-tag-lit))
                (list (ir-tag-lit) (- #x0 (cadr arg-ir)))
                (list (ir-tag-sub) (list (ir-tag-lit) #x0) arg-ir)))
          (if (null (cddr args))
              ;; Two arguments: normal binary sub
              (let* ((left-ir (h0-compile (car args) env fenv))
                     (right-ir (h0-compile (cadr args) env fenv)))
                ;; Constant folding
                (if (and (h0-has-tag-n left-ir (ir-tag-lit)) (h0-has-tag-n right-ir (ir-tag-lit)))
                    (list (ir-tag-lit) (- (cadr left-ir) (cadr right-ir)))
                    (list (ir-tag-sub) left-ir right-ir)))
              ;; More than two: (- a b c ...) => (- (- a b) c ...)
              ;; Note: subtraction is left-associative unlike addition
              (h0-compile-sub (cons (list '- (car args) (cadr args)) (cddr args)) env fenv)))))

;; Compile let - iterate through bindings, extending environment
;; Store symbol name (string) in env for string-based lookup
(defun h0-compile-let (bindings body env fenv)
  (if (null bindings)
      (h0-compile body env fenv)
      (let* ((b (car bindings))
             (var-sym (car b))
             (val-ir (h0-compile (cadr b) env fenv))
             ;; Store symbol for flat list lookup
             (new-env (cons var-sym env))
             (body-ir (h0-compile-let (cdr bindings) body new-env fenv)))
        (list (ir-tag-let) #x0 val-ir body-ir))))

;; Compile progn - sequence of forms
(defun h0-compile-progn (forms env fenv)
  (if (null forms)
      (list (ir-tag-lit) #x0)
      (if (null (cdr forms))
          (h0-compile (car forms) env fenv)
          (let* ((first-ir (h0-compile (car forms) env fenv))
                 (rest-ir (h0-compile-progn-rest (cdr forms) env fenv)))
            (list (ir-tag-progn) (cons first-ir rest-ir))))))

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
      (list (ir-tag-lit) #x0)
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
                 (temp-ref (list (ir-tag-var) #x0))
                 (if-ir (list (ir-tag-if) temp-ref temp-ref rest-ir)))
            (list (ir-tag-let) #x0 first-ir if-ir)))))

;; Compile AND - expand to if chain: (and a b c) => (if a (if b c nil) nil)
;; Returns the last value if all are true, nil otherwise
(defun h0-compile-and (args env fenv)
  (if (null args)
      ;; No arguments - return t
      (list (ir-tag-lit) #x1)
      (if (null (cdr args))
          ;; Single argument - just compile it
          (h0-compile (car args) env fenv)
          ;; Multiple arguments - (if a (and b c...) nil)
          (let* ((first-ir (h0-compile (car args) env fenv))
                 (rest-ir (h0-compile-and (cdr args) env fenv))
                 (else-ir (list (ir-tag-lit) #x0)))
            (list (ir-tag-if) first-ir rest-ir else-ir)))))

;; Compile cond - expand to nested IFs
;; (cond (c1 e1...) (c2 e2...) (t e3...))
;; => (if c1 (progn e1...) (if c2 (progn e2...) (progn e3...)))
(defun h0-compile-cond (clauses env fenv)
  (if (null clauses)
      ;; No clauses - return nil
      (list (ir-tag-lit) #x0)
      (let* ((clause (car clauses))
             (test (car clause))
             (body (cdr clause)))
        (if (null (cdr clauses))
            ;; Last clause - just compile it
            (if (eq test 'T)
                ;; (t body...) - always true, just execute body
                (if (null body)
                    (list (ir-tag-lit) #x0)
                    (if (null (cdr body))
                        (h0-compile (car body) env fenv)
                        (h0-compile-progn body env fenv)))
                ;; Last clause with non-t test - normal if
                (let* ((test-ir (h0-compile test env fenv))
                       (body-ir (if (null body)
                                    (list (ir-tag-lit) #x0)
                                    (if (null (cdr body))
                                        (h0-compile (car body) env fenv)
                                        (h0-compile-progn body env fenv))))
                       (else-ir (list (ir-tag-lit) #x0)))
                  (list (ir-tag-if) test-ir body-ir else-ir)))
            ;; Multiple clauses - nested if
            (if (eq test 'T)
                ;; (t body...) in middle - execute body (subsequent clauses ignored)
                (if (null body)
                    (list (ir-tag-lit) #x0)
                    (if (null (cdr body))
                        (h0-compile (car body) env fenv)
                        (h0-compile-progn body env fenv)))
                ;; Normal clause - if with nested cond for else
                (let* ((test-ir (h0-compile test env fenv))
                       (then-ir (if (null body)
                                    (list (ir-tag-lit) #x0)
                                    (if (null (cdr body))
                                        (h0-compile (car body) env fenv)
                                        (h0-compile-progn body env fenv))))
                       (else-ir (h0-compile-cond (cdr clauses) env fenv)))
                  (list (ir-tag-if) test-ir then-ir else-ir)))))))

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
    (list (ir-tag-let) #x0 key-ir
          (h0-compile-case-clauses clauses new-env fenv))))

(defun h0-compile-case-clauses (clauses env fenv)
  (if (null clauses)
      ;; No clauses - return nil
      (list (ir-tag-lit) #x0)
      (let* ((clause (car clauses))
             (keys (car clause))
             (body (cdr clause)))
        (if (or (eq keys 'OTHERWISE) (eq keys 'T))
            ;; Default clause - just execute body
            (if (null body)
                (list (ir-tag-lit) #x0)
                (if (null (cdr body))
                    (h0-compile (car body) env fenv)
                    (h0-compile-progn body env fenv)))
            ;; Normal clause - compare key(s)
            (let* ((test-ir (h0-compile-case-test keys env fenv))
                   (then-ir (if (null body)
                                (list (ir-tag-lit) #x0)
                                (if (null (cdr body))
                                    (h0-compile (car body) env fenv)
                                    (h0-compile-progn body env fenv))))
                   (else-ir (h0-compile-case-clauses (cdr clauses) env fenv)))
              (list (ir-tag-if) test-ir then-ir else-ir))))))

(defun h0-compile-case-test (keys env fenv)
  ;; Get the temporary key variable from environment
  (let ((key-var-ir (list (ir-tag-var) #x0)))
    (if (consp keys)
        ;; Multiple keys - (or (eq key k1) (eq key k2) ...)
        (h0-compile-case-test-list keys key-var-ir env fenv)
        ;; Single key - (eq key k)
        (let ((key-lit-ir (h0-compile (list 'quote keys) env fenv)))
          (list (ir-tag-eq) key-var-ir key-lit-ir)))))

(defun h0-compile-case-test-list (keys key-var-ir env fenv)
  (if (null keys)
      ;; Should not happen, but return false
      (list (ir-tag-lit) #x0)
      (if (null (cdr keys))
          ;; Single key left
          (let ((key-lit-ir (h0-compile (list 'quote (car keys)) env fenv)))
            (list (ir-tag-eq) key-var-ir key-lit-ir))
          ;; Multiple keys - (or (eq key k1) (rest...))
          (let* ((key-lit-ir (h0-compile (list 'quote (car keys)) env fenv))
                 (test-ir (list (ir-tag-eq) key-var-ir key-lit-ir))
                 (rest-ir (h0-compile-case-test-list (cdr keys) key-var-ir env fenv)))
            ;; (if test t rest) - implements OR
            (list (ir-tag-if) test-ir (list (ir-tag-lit) #x1) rest-ir)))))

;; Compile FLET - local function definitions (non-recursive)
;; Transform: (flet ((f (x) body)) form) => (let ((f (lambda (x) body))) form)
(defun h0-compile-flet (bindings body env fenv)
  (h0-compile-flet-to-let bindings body env fenv))

(defun h0-compile-flet-to-let (bindings body env fenv)
  (if (null bindings)
      (h0-compile-progn body env fenv)
      (let* ((let-bindings (h0-flet-bindings-to-let bindings)))
        (h0-compile-let let-bindings (car body) env fenv))))

(defun h0-flet-bindings-to-let (bindings)
  (if (null bindings)
      nil
      (let* ((binding (car bindings))
             (fname (car binding))
             (params (cadr binding))
             (fbody (caddr binding))
             (lambda-expr (list (intern "LAMBDA") params fbody))
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
        (h0-compile-let let-bindings (cons (intern "PROGN") combined-body) env fenv))))

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
             (lambda-expr (list (intern "LAMBDA") params fbody))
             (setq-form (list (intern "SETQ") fname lambda-expr)))
        (cons setq-form (h0-labels-setq-forms (cdr bindings))))))

(defun h0-append (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (h0-append (cdr list1) list2))))

;; Compile WHILE - transform to labels loop
;; (while test body...) =>
;; (labels ((loop ()
;;            (when test
;;              body...
;;              (loop))))
;;   (loop))
(defun h0-compile-while (test body env fenv)
  (let* ((loop-sym (intern "LOOP"))
         (when-sym (intern "WHEN"))
         (labels-sym (intern "LABELS"))
         ;; Build body + recursive call: (when test body... (loop))
         (loop-body (h0-append body (list (list loop-sym))))
         (when-form (cons when-sym (cons test loop-body)))
         ;; Build the expanded form
         (expanded
          (list labels-sym
                ;; ((loop () (when test body... (loop))))
                (list (list loop-sym (list) when-form))
                ;; (loop)
                (list loop-sym))))
    (h0-compile expanded env fenv)))

;; Compile MAPCAR - expand to labels loop with reverse at end
;; (mapcar fn list) =>
;; (let ((fn-temp fn))
;;   (labels ((loop (l acc)
;;              (if (null l)
;;                  (reverse acc)
;;                  (loop (cdr l) (cons (funcall fn-temp (car l)) acc)))))
;;     (loop list nil)))
(defun h0-compile-mapcar (fn-expr list-expr env fenv)
  (let* ((fn-sym (intern "FN-TEMP"))
         (l-sym (intern "L"))
         (acc-sym (intern "ACC"))
         (loop-sym (intern "MAPCAR-LOOP"))
         ;; Build the expanded form
         (expanded
          (list (intern "LET")
                (list (list fn-sym fn-expr))
                (list (intern "LABELS")
                      (list (list loop-sym (list l-sym acc-sym)
                                  (list (intern "IF") (list (intern "NULL") l-sym)
                                        (list (intern "REVERSE") acc-sym)
                                        (list loop-sym
                                              (list (intern "CDR") l-sym)
                                              (list (intern "CONS")
                                                    (list (intern "FUNCALL") fn-sym (list (intern "CAR") l-sym))
                                                    acc-sym)))))
                      (list loop-sym list-expr nil)))))
    (h0-compile expanded env fenv)))

;; Compile REVERSE - expand to labels loop
;; (reverse list) =>
;; (labels ((loop (l acc)
;;            (if (null l)
;;                acc
;;                (loop (cdr l) (cons (car l) acc)))))
;;   (loop list nil))
(defun h0-compile-reverse (list-expr env fenv)
  (let* ((l-sym (intern "L"))
         (acc-sym (intern "ACC"))
         (loop-sym (intern "REV-LOOP"))
         ;; Build the expanded form
         (expanded
          (list (intern "LABELS")
                (list (list loop-sym (list l-sym acc-sym)
                            (list (intern "IF") (list (intern "NULL") l-sym)
                                  acc-sym
                                  (list loop-sym
                                        (list (intern "CDR") l-sym)
                                        (list (intern "CONS")
                                              (list (intern "CAR") l-sym)
                                              acc-sym)))))
                (list loop-sym list-expr nil))))
    (h0-compile expanded env fenv)))

;; Compile LIST - expand to nested CONS
;; (list a b c) => (cons a (cons b (cons c nil)))
(defun h0-compile-list (args env fenv)
  (if (null args)
      (list (ir-tag-lit) #x0)  ;; Empty list = nil
      (let* ((first-ir (h0-compile (car args) env fenv))
             (rest-ir (h0-compile-list (cdr args) env fenv)))
        (list (ir-tag-cons) first-ir rest-ir))))

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
      (list (ir-tag-car) list-ir)
      (h0-nth-chain (- n #x1) (list (ir-tag-cdr) list-ir))))

(defun h0-compile-nth-var (n-expr list-expr env fenv)
  ;; Expand to: (labels ((loop (i l) (if (= i 0) (car l) (loop (- i 1) (cdr l))))) (loop n list))
  (let* ((i-sym (intern "I"))
         (l-sym (intern "L"))
         (loop-sym (intern "NTH-LOOP"))
         (expanded
          (list (intern "LABELS")
                (list (list loop-sym (list i-sym l-sym)
                            (list (intern "IF") (list (intern "=") i-sym #x0)
                                  (list (intern "CAR") l-sym)
                                  (list loop-sym
                                        (list (intern "-") i-sym #x1)
                                        (list (intern "CDR") l-sym)))))
                (list loop-sym n-expr list-expr))))
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

;; Append string as bytes (without null terminator)
(defun buf-string (buf str)
  (buf-string-helper buf str #x0 (string-length str)))

(defun buf-string-helper (buf str i len)
  (if (>= i len)
      buf
      (buf-string-helper (buf-u8 buf (string-ref str i)) str (+ i #x1) len)))

;; Append string padded to length with zeros
(defun buf-string-padded (buf str len)
  (let* ((slen (string-length str))
         (buf2 (buf-string-helper buf str #x0 (if (< slen len) slen len))))
    (buf-zeros buf2 (- len slen))))

;; Get current buffer length
(defun buf-length (buf)
  (length buf))

;; Convert buffer to vector (reverses the list)
(defun buf-to-vector (buf)
  (let* ((len (length buf))
         (vec (make-vector len)))
    (buf-to-vector-helper (reverse buf) vec #x0)))

(defun buf-to-vector-helper (lst vec i)
  (if (null lst)
      vec
      (progn
        (vector-set vec i (car lst))
        (buf-to-vector-helper (cdr lst) vec (+ i #x1)))))

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

;; Append multiple byte lists
(defun bytes-append-all (lists)
  (if (null lists)
      nil
      (bytes-append (car lists) (bytes-append-all (cdr lists)))))

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

(defun build-import-strings-helper (imports)
  (if (null imports)
      nil
      (let ((name (car imports)))
        (bytes-append (string-to-bytes name)
                      (cons #x0 (build-import-strings-helper (cdr imports)))))))

(defun string-to-bytes (str)
  (string-to-bytes-helper str #x0 (string-length str) nil))

(defun string-to-bytes-helper (str i len acc)
  (if (>= i len)
      (reverse acc)
      (string-to-bytes-helper str (+ i #x1) len (cons (string-ref str i) acc))))

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
;; Helper to create builtin fenv entry - interns name once
(defun make-builtin-entry (name builtin-kw)
  (let ((sym (intern name)))
    (cons sym (cons builtin-kw sym))))

(defun make-compiler-fenv ()
  "Build fenv with core compiler functions exposed"
  ;; Format: (name . (:builtin . impl-symbol))
  (let ((kw (intern-keyword "BUILTIN")))
    (list
     ;; Compiler functions
     (make-builtin-entry "H0-COMPILE" kw)
     (make-builtin-entry "H0-CODEGEN" kw)
     (make-builtin-entry "H0-LINEARIZE" kw)
     (make-builtin-entry "DELIVER-WITH-IMPORTS-AND-HEAP" kw)
     (make-builtin-entry "READ-ALL" kw)
     (make-builtin-entry "NATIVE-READ-FILE" kw)
     (make-builtin-entry "COLLECT-DEFUNS" kw)
     ;; ARM64 - data movement
     (make-builtin-entry "STR" kw)
     (make-builtin-entry "LDR" kw)
     (make-builtin-entry "STUR" kw)
     (make-builtin-entry "LDUR" kw)
     (make-builtin-entry "STP" kw)
     (make-builtin-entry "LDP" kw)
     (make-builtin-entry "STRB" kw)
     (make-builtin-entry "LDRB" kw)
     (make-builtin-entry "MOV" kw)
     (make-builtin-entry "MOVZ" kw)
     (make-builtin-entry "MOVK" kw)
     (make-builtin-entry "MOVN" kw)
     ;; ARM64 - arithmetic
     (make-builtin-entry "ADD" kw)
     (make-builtin-entry "SUB" kw)
     (make-builtin-entry "SUBS" kw)
     (make-builtin-entry "MUL" kw)
     (make-builtin-entry "SDIV" kw)
     (make-builtin-entry "NEG" kw)
     ;; ARM64 - bitwise
     (make-builtin-entry "AND*" kw)
     (make-builtin-entry "ORR" kw)
     (make-builtin-entry "EOR" kw)
     (make-builtin-entry "LSL" kw)
     (make-builtin-entry "LSR" kw)
     (make-builtin-entry "ASR" kw)
     (make-builtin-entry "MVN" kw)
     ;; ARM64 - compare and branch
     (make-builtin-entry "CMP" kw)
     (make-builtin-entry "CSET" kw)
     (make-builtin-entry "B" kw)
     (make-builtin-entry "BL" kw)
     (make-builtin-entry "BR" kw)
     (make-builtin-entry "BLR" kw)
     (make-builtin-entry "CBZ" kw)
     (make-builtin-entry "CBNZ" kw)
     (make-builtin-entry "B.EQ" kw)
     (make-builtin-entry "B.NE" kw)
     (make-builtin-entry "B.LT" kw)
     (make-builtin-entry "B.LE" kw)
     (make-builtin-entry "B.GT" kw)
     (make-builtin-entry "B.GE" kw)
     (make-builtin-entry "RET" kw)
     ;; ARM64 - system
     (make-builtin-entry "SVC" kw)
     (make-builtin-entry "BRK" kw)
     (make-builtin-entry "NOP" kw)
     ;; ARM64 - utility
     (make-builtin-entry "REG" kw)
     (make-builtin-entry "ENCODE" kw))))

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
