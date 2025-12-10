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

;;; Keyword interning - keywords use tag 7, stored in separate keyword table
;;; Keywords are self-evaluating symbols in the KEYWORD package
;;; Keywords have same memory layout as symbols: [length:8][chars:N]

;; Get keyword name - same layout as symbols, just different tag
;; Untag by masking off tag bits, then read string data
(defun keyword-name (kw)
  ;; Keywords have same layout as symbols, just different tag (7 vs 2)
  ;; Use set-tag to change tag from 7 to 2, then call symbol-name
  (symbol-name (set-tag kw 2)))

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

;; Operator checks - pure eq comparison
;; All symbols are properly interned, so eq is sufficient
;; If eq fails, that indicates a symbol interning bug - fail fast
(defun op=quote (sym) (eq sym *op-quote*))
(defun op=if (sym) (eq sym *op-if*))
(defun op=let (sym) (eq sym *op-let*))
(defun op=defun (sym) (eq sym *op-defun*))
(defun op=defvar (sym) (eq sym *op-defvar*))
(defun op=while (sym) (eq sym *op-while*))
(defun op=t (sym) (eq sym *op-t*))
(defun op=plus (sym) (eq sym *op-plus*))
(defun op=minus (sym) (eq sym *op-minus*))
(defun op=mul (sym) (eq sym *op-mul*))
(defun op=div (sym) (eq sym *op-div*))
(defun op=eq-num (sym) (eq sym *op-eq-num*))
(defun op=lt (sym) (eq sym *op-lt*))
(defun op=gt (sym) (eq sym *op-gt*))
(defun op=le (sym) (eq sym *op-le*))
(defun op=ge (sym) (eq sym *op-ge*))
(defun op=let-star (sym) (eq sym *op-let-star*))
(defun op=progn (sym) (eq sym *op-progn*))
(defun op=cond (sym) (eq sym *op-cond*))
(defun op=mod (sym) (eq sym *op-mod*))
(defun op=cons (sym) (eq sym *op-cons*))
(defun op=car (sym) (eq sym *op-car*))
(defun op=cdr (sym) (eq sym *op-cdr*))
(defun op=cadr (sym) (eq sym *op-cadr*))
(defun op=cddr (sym) (eq sym *op-cddr*))
(defun op=caddr (sym) (eq sym *op-caddr*))
(defun op=cadddr (sym) (eq sym *op-cadddr*))
(defun op=null (sym) (eq sym *op-null*))
(defun op=consp (sym) (eq sym *op-consp*))
(defun op=list (sym) (eq sym *op-list*))
(defun op=not (sym) (eq sym *op-not*))
(defun op=and (sym) (eq sym *op-and*))
(defun op=or (sym) (eq sym *op-or*))
(defun op=defpackage (sym) (eq sym *op-defpackage*))
(defun op=in-package (sym) (eq sym *op-in-package*))
(defun op=case (sym) (eq sym *op-case*))
(defun op=when (sym) (eq sym *op-when*))
(defun op=unless (sym) (eq sym *op-unless*))
(defun op=declaim (sym) (eq sym *op-declaim*))
(defun op=setq (sym) (eq sym *op-setq*))
(defun op=error (sym) (eq sym *op-error*))
;; Additional operators - previously used string comparison
(defun op=symbolp (sym) (eq sym *op-symbolp*))
(defun op=numberp (sym) (eq sym *op-numberp*))
(defun op=stringp (sym) (eq sym *op-stringp*))
(defun op=keywordp (sym) (eq sym *op-keywordp*))
(defun op=string-length (sym) (eq sym *op-string-length*))
(defun op=string-ref (sym) (eq sym *op-string-ref*))
(defun op=char-at (sym) (eq sym *op-char-at*))
(defun op=string= (sym) (eq sym *op-string=*))
(defun op=symbol-name (sym) (eq sym *op-symbol-name*))
(defun op=keyword-name (sym) (eq sym *op-keyword-name*))
(defun op=logand (sym) (eq sym *op-logand*))
(defun op=logior (sym) (eq sym *op-logior*))
(defun op=ash (sym) (eq sym *op-ash*))
(defun op=eq (sym) (eq sym *op-eq*))
(defun op=eql (sym) (eq sym *op-eql*))
(defun op=get-tag (sym) (eq sym *op-get-tag*))
(defun op=set-tag (sym) (eq sym *op-set-tag*))
(defun op=length (sym) (eq sym *op-length*))
(defun op=make-vector (sym) (eq sym *op-make-vector*))
(defun op=vector-length (sym) (eq sym *op-vector-length*))
(defun op=vector-set (sym) (eq sym *op-vector-set*))
(defun op=vector-ref (sym) (eq sym *op-vector-ref*))
(defun op=reverse (sym) (eq sym *op-reverse*))
(defun op=make-string-from-vector (sym) (eq sym *op-make-string-from-vector*))
(defun op=make-symbol-from-string (sym) (eq sym *op-make-symbol-from-string*))
(defun op=caar (sym) (eq sym *op-caar*))
(defun op=cdar (sym) (eq sym *op-cdar*))
(defun op=nth (sym) (eq sym *op-nth*))
(defun op=lognot (sym) (eq sym *op-lognot*))
(defun op=neq (sym) (eq sym *op-neq*))
(defun op=lambda (sym) (eq sym *op-lambda*))
(defun op=funcall (sym) (eq sym *op-funcall*))
(defun op=setcar (sym) (eq sym *op-setcar*))
(defun op=setcdr (sym) (eq sym *op-setcdr*))
(defun op=dolist (sym) (eq sym *op-dolist*))
(defun op=flet (sym) (eq sym *op-flet*))
(defun op=labels (sym) (eq sym *op-labels*))
(defun op=mapcar (sym) (eq sym *op-mapcar*))
(defun op=ecase (sym) (eq sym *op-ecase*))
(defun op=listp (sym) (eq sym *op-listp*))
(defun op=nil (sym) (eq sym *op-nil*))
(defun op=otherwise (sym) (eq sym *op-otherwise*))

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
;; Uses eq since all symbols are properly interned
(defun fenv-lookup (sym fenv)
  (if (null fenv) nil
      (let ((entry (car fenv)))
        (if (eq sym (car entry))
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

;; Look up by symbol in environment using eq
;; Flat list format: (sym1 val1 sym2 val2 ...)
;; Returns the value entry (cons sym val) or nil if not found
;; This allows distinguishing "not found" from "found with nil value"
(defun env-lookup (sym env)
  (if (null env) nil
      (if (eq sym (car env))
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
    ;; Symbol lookup - first local env, then global env
    ((symbolp expr)
     (let ((entry (env-lookup expr env)))
       (if entry
           (cdr entry)  ; Extract value from local entry
           ;; Try global env
           (let ((global-entry (h0-global-lookup expr)))
             (if global-entry
                 (cdr global-entry)  ; Extract value from global entry
                 ;; Not found - undefined symbol
                 (fatal-error "h0-eval: undefined symbol"))))))
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
         ;; FUNCALL - call a function value (closure)
         ((if (symbolp op) (op=funcall op) nil)
          (let ((fn (h0-eval (cadr expr) env fenv))
                (args (h0-eval-list (cddr expr) env fenv)))
            (if (and (consp fn) (eq (car fn) (intern "CLOSURE-TAG")))
                ;; Closure: (CLOSURE-TAG params body captured-env)
                (let* ((params (cadr fn))
                       (body (caddr fn))
                       (captured-env (cadddr fn))
                       (new-env (bind-lambda-args params args captured-env fenv)))
                  (h0-eval body new-env fenv))
                (fatal-error "h0-eval: FUNCALL on non-closure"))))
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
         ;; Compiler functions (IDs 1-7)
         (cons (intern "H0-COMPILE") 1)
         (cons (intern "H0-CODEGEN") 2)
         (cons (intern "H0-LINEARIZE") 3)
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

;; Dispatch builtin functions via ID lookup and match
(defun h0-eval-builtin (name args fenv)
  (let ((id (find-builtin-id name *builtin-dispatch*)))
    (if (null id)
        (fatal-error "h0-eval-builtin: unknown builtin")
        (match id
          ;; Compiler functions
          (1 (h0-compile (car args) (cadr args) (caddr args)))
          (2 (h0-codegen (car args) (cadr args)))
          (3 (h0-linearize (car args)))
          (4 (deliver-with-imports-and-heap (car args) (cadr args) (caddr args) (cadddr args)))
          (5 (read-all (car args)))
          (6 (native-read-file (car args)))
          (7 (collect-defuns (car args) (cadr args)))
          ;; ARM64 - memory
          (10 (str (car args) (cadr args) :offset (get-kw-arg args *kw-offset* 0)))
          (11 (ldr (car args) (cadr args) :offset (get-kw-arg args *kw-offset* 0)))
          (12 (stp (car args) (cadr args) (caddr args) :offset (get-kw-arg args *kw-offset* 0)))
          (13 (ldp (car args) (cadr args) (caddr args) :offset (get-kw-arg args *kw-offset* 0)))
          ;; ARM64 - data movement
          (20 (mov (car args) (cadr args)))
          (21 (movz (car args) (cadr args)))
          ;; ARM64 - arithmetic
          (30 (if (cddr args)
                  (add (car args) (cadr args) (caddr args) :imm (get-kw-arg args *kw-imm* nil))
                  (add (car args) (cadr args) 0)))
          (31 (if (cddr args)
                  (sub (car args) (cadr args) (caddr args) :imm (get-kw-arg args *kw-imm* nil))
                  (sub (car args) (cadr args) 0)))
          ;; ARM64 - compare/branch
          (40 (if (get-kw-arg args *kw-imm* nil)
                  (cmp (car args) (cadr args) :imm t)
                  (cmp (car args) (cadr args))))
          (41 (b (car args)))
          (42 (bl (car args)))
          (43 (b.eq (car args)))
          (44 (b.ne (car args)))
          (45 (cbz (car args) (cadr args)))
          (46 (cbnz (car args) (cadr args)))
          (47 (ret))
          (48 (nop))
          ;; ARM64 - utility
          (50 (reg (car args)))
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

;; Initialize compile ops - now a no-op since we use string comparison
(defun init-compile-ops ()
  ;; Initialize all operator symbols via intern
  ;; This ensures eq comparison works in op= functions
  (setq *op-quote* (intern "QUOTE"))
  (setq *op-if* (intern "IF"))
  (setq *op-let* (intern "LET"))
  (setq *op-let-star* (intern "LET*"))
  (setq *op-defun* (intern "DEFUN"))
  (setq *op-defvar* (intern "DEFVAR"))
  (setq *op-while* (intern "WHILE"))
  (setq *op-progn* (intern "PROGN"))
  (setq *op-cond* (intern "COND"))
  (setq *op-t* (intern "T"))
  (setq *op-plus* (intern "+"))
  (setq *op-minus* (intern "-"))
  (setq *op-mul* (intern "*"))
  (setq *op-div* (intern "/"))
  (setq *op-mod* (intern "MOD"))
  (setq *op-eq-num* (intern "="))
  (setq *op-lt* (intern "<"))
  (setq *op-gt* (intern ">"))
  (setq *op-le* (intern "<="))
  (setq *op-ge* (intern ">="))
  (setq *op-cons* (intern "CONS"))
  (setq *op-car* (intern "CAR"))
  (setq *op-cdr* (intern "CDR"))
  (setq *op-cadr* (intern "CADR"))
  (setq *op-cddr* (intern "CDDR"))
  (setq *op-caddr* (intern "CADDR"))
  (setq *op-cadddr* (intern "CADDDR"))
  (setq *op-null* (intern "NULL"))
  (setq *op-consp* (intern "CONSP"))
  (setq *op-list* (intern "LIST"))
  (setq *op-not* (intern "NOT"))
  (setq *op-and* (intern "AND"))
  (setq *op-or* (intern "OR"))
  (setq *op-defpackage* (intern "DEFPACKAGE"))
  (setq *op-in-package* (intern "IN-PACKAGE"))
  (setq *op-case* (intern "CASE"))
  (setq *op-when* (intern "WHEN"))
  (setq *op-unless* (intern "UNLESS"))
  (setq *op-declaim* (intern "DECLAIM"))
  (setq *op-setq* (intern "SETQ"))
  (setq *op-error* (intern "ERROR"))
  ;; Additional operators - previously used string comparison
  (setq *op-symbolp* (intern "SYMBOLP"))
  (setq *op-numberp* (intern "NUMBERP"))
  (setq *op-stringp* (intern "STRINGP"))
  (setq *op-keywordp* (intern "KEYWORDP"))
  (setq *op-string-length* (intern "STRING-LENGTH"))
  (setq *op-string-ref* (intern "STRING-REF"))
  (setq *op-char-at* (intern "CHAR-AT"))
  (setq *op-string=* (intern "STRING="))
  (setq *op-symbol-name* (intern "SYMBOL-NAME"))
  (setq *op-keyword-name* (intern "KEYWORD-NAME"))
  (setq *op-logand* (intern "LOGAND"))
  (setq *op-logior* (intern "LOGIOR"))
  (setq *op-ash* (intern "ASH"))
  (setq *op-eq* (intern "EQ"))
  (setq *op-eql* (intern "EQL"))
  (setq *op-get-tag* (intern "GET-TAG"))
  (setq *op-set-tag* (intern "SET-TAG"))
  (setq *op-length* (intern "LENGTH"))
  (setq *op-make-vector* (intern "MAKE-VECTOR"))
  (setq *op-vector-length* (intern "VECTOR-LENGTH"))
  (setq *op-vector-set* (intern "VECTOR-SET"))
  (setq *op-vector-ref* (intern "VECTOR-REF"))
  (setq *op-reverse* (intern "REVERSE"))
  (setq *op-make-string-from-vector* (intern "MAKE-STRING-FROM-VECTOR"))
  (setq *op-make-symbol-from-string* (intern "MAKE-SYMBOL-FROM-STRING"))
  (setq *op-caar* (intern "CAAR"))
  (setq *op-cdar* (intern "CDAR"))
  (setq *op-nth* (intern "NTH"))
  (setq *op-lognot* (intern "LOGNOT"))
  (setq *op-neq* (intern "/="))
  (setq *op-lambda* (intern "LAMBDA"))
  (setq *op-funcall* (intern "FUNCALL"))
  (setq *op-setcar* (intern "SETCAR"))
  (setq *op-setcdr* (intern "SETCDR"))
  (setq *op-dolist* (intern "DOLIST"))
  (setq *op-flet* (intern "FLET"))
  (setq *op-labels* (intern "LABELS"))
  (setq *op-mapcar* (intern "MAPCAR"))
  (setq *op-ecase* (intern "ECASE"))
  (setq *op-listp* (intern "LISTP"))
  (setq *op-nil* (intern "NIL"))
  (setq *op-otherwise* (intern "OTHERWISE"))
  ;; Initialize runtime keywords for eq comparison
  (setq *kw-offset* (intern-keyword "OFFSET"))
  (setq *kw-imm* (intern-keyword "IMM"))
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
;;; Linearization - Convert tree IR to linear IR
;;; ==========================================================================
;;; Converts nested IR expressions to a flat sequence of instructions with
;;; explicit temporaries and control flow labels.
;;;
;;; State management:
;;; Linear state is a cons cell: ((temp-counter . label-counter) . output)
;;; - temp-counter: next available temp slot number
;;; - label-counter: next available label number
;;; - output: list of instructions (in reverse order, reversed at end)

;; Helper: Convert number to string using recursive digit extraction
(defun h0-number-to-string (n)
  (if (< n 0)
      (cons #x2D (h0-number-to-string-helper (- 0 n)))  ; prepend '-' for negative
      (h0-number-to-string-helper n)))

(defun h0-number-to-string-helper (n)
  (if (< n 10)
      (list (+ #x30 n))  ; '0' is 48 (0x30)
      (h0-append (h0-number-to-string-helper (/ n 10))
                 (list (+ #x30 (mod n 10))))))

;; Helper: Create label symbol from number (L0, L1, etc.)
(defun h0-make-label-symbol (n)
  (let* ((l-char (list #x4C))  ; 'L' = 76 (0x4C)
         (digits (h0-number-to-string n))
         (name-chars (h0-append l-char digits))
         (name-str (make-string-from-vector (make-vector name-chars))))
    (make-symbol-from-string name-str)))

;; Create initial linear state
(defun h0-make-linear-state ()
  (cons (cons 0 0) nil))

;; Allocate a fresh temp slot, returns temp number
(defun h0-fresh-temp (state)
  (let* ((counter-cell (car state))
         (n (car counter-cell)))
    (setcar counter-cell (+ n 1))
    n))

;; Allocate a fresh label, returns label symbol
(defun h0-fresh-label (state)
  (let* ((counter-cell (car state))
         (n (cdr counter-cell)))
    (setcdr counter-cell (+ n 1))
    (h0-make-label-symbol n)))

;; Emit a linear IR instruction
(defun h0-emit-linear (state instr)
  (setcdr state (cons instr (cdr state))))

;; Get final output (in correct order)
(defun h0-get-linear-output (state)
  (reverse (cdr state)))

;; Check if IR is a leaf node (no sub-expressions to linearize)
(defun h0-linear-leaf-p (ir)
  (if (consp ir)
      (let ((tag (car ir)))
        (if (= tag (intern "LIT")) t
            (if (= tag (intern "NIL-IR")) t
                (if (= tag (intern "VAR")) t
                    (if (= tag (intern "SYM-LIT")) t
                        (if (= tag (intern "STR-LIT")) t
                            (if (= tag (intern "LAMBDA-REF")) t
                                (if (= tag (intern "GET-GLOBAL-VARS-IR")) t
                                    (if (= tag (intern "GET-CMDLINE-ARGS-IR")) t
                                        nil)))))))))
      t))  ; non-cons is a leaf (e.g., bare number)

;; Linearize a leaf IR node, returns temp holding result
(defun h0-linearize-leaf (ir state)
  (let ((dst (h0-fresh-temp state)))
    (if (consp ir)
        (let ((tag (car ir)))
          (if (= tag (intern "LIT"))
              (h0-emit-linear state (list (intern "LOAD-LIT") dst (car (cdr ir))))
              (if (= tag (intern "NIL-IR"))
                  (h0-emit-linear state (list (intern "LOAD-NIL") dst))
                  (if (= tag (intern "VAR"))
                      (h0-emit-linear state (list (intern "LOAD-VAR") dst (car (cdr ir))))
                      (if (= tag (intern "SYM-LIT"))
                          (h0-emit-linear state (list (intern "LOAD-SYM") dst (car (cdr ir))))
                          (if (= tag (intern "STR-LIT"))
                              (h0-emit-linear state (list (intern "LOAD-STR") dst (car (cdr ir))))
                              (if (= tag (intern "LAMBDA-REF"))
                                  (h0-emit-linear state (list (intern "LOAD-LAMBDA") dst (car (cdr ir)) (car (cdr (cdr ir)))))
                                  (if (= tag (intern "GET-GLOBAL-VARS-IR"))
                                      (h0-emit-linear state (list (intern "GET-GLOBAL-VARS") dst))
                                      (if (= tag (intern "GET-CMDLINE-ARGS-IR"))
                                          (h0-emit-linear state (list (intern "GET-CMDLINE-ARGS") dst))
                                          (error "linearize-leaf: unknown leaf type"))))))))))
        ;; bare number
        (h0-emit-linear state (list (intern "LOAD-LIT") dst ir)))
    dst))

;; Linearize a binary operation, returns temp holding result
(defun h0-linearize-binary (tag ir state)
  (let* ((left-temp (h0-linearize-expr (car (cdr ir)) state))
         (right-temp (h0-linearize-expr (car (cdr (cdr ir))) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list tag dst left-temp right-temp))
    dst))

;; Linearize a unary operation, returns temp holding result
(defun h0-linearize-unary (tag ir state)
  (let* ((arg-temp (h0-linearize-expr (car (cdr ir)) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list tag dst arg-temp))
    dst))

;; Linearize if expression with explicit jumps
(defun h0-linearize-if (ir state)
  (let* ((else-label (h0-fresh-label state))
         (end-label (h0-fresh-label state))
         (test-temp (h0-linearize-expr (car (cdr ir)) state))
         (dst (h0-fresh-temp state)))
    ;; Jump to else if test is nil
    (h0-emit-linear state (list (intern "JUMP-IF-NIL") test-temp else-label))
    ;; Then branch
    (let ((then-temp (h0-linearize-expr (car (cdr (cdr ir))) state)))
      (h0-emit-linear state (list (intern "MOVE") dst then-temp)))
    (h0-emit-linear state (list (intern "JUMP") end-label))
    ;; Else branch
    (h0-emit-linear state (list (intern "LABEL") else-label))
    (let ((else-ir (car (cdr (cdr (cdr ir))))))
      (if else-ir
          (let ((else-temp (h0-linearize-expr else-ir state)))
            (h0-emit-linear state (list (intern "MOVE") dst else-temp)))
          (let ((nil-dst (h0-fresh-temp state)))
            (h0-emit-linear state (list (intern "LOAD-NIL") nil-dst))
            (h0-emit-linear state (list (intern "MOVE") dst nil-dst)))))
    (h0-emit-linear state (list (intern "LABEL") end-label))
    dst))

;; Linearize progn, returns temp of last expression
(defun h0-linearize-progn (ir state)
  (let ((forms (car (cdr ir))))
    (h0-linearize-progn-forms forms state)))

(defun h0-linearize-progn-forms (forms state)
  (if (null forms)
      (let ((nil-dst (h0-fresh-temp state)))
        (h0-emit-linear state (list (intern "LOAD-NIL") nil-dst))
        nil-dst)
      (let ((temp (h0-linearize-expr (car forms) state)))
        (if (null (cdr forms))
            temp
            (h0-linearize-progn-forms (cdr forms) state)))))

;; Linearize let binding
(defun h0-linearize-let (ir state)
  (let* ((vals (car (cdr ir)))
         (body (car (cdr (cdr ir))))
         (count (car (cdr (cdr (cdr ir)))))
         (offs-raw (car (cdr (cdr (cdr (cdr ir))))))
         ;; Handle both formats: (0 1 2) or just 0
         (offs (if (consp offs-raw) offs-raw (list offs-raw))))
    ;; Emit bind instruction
    (h0-emit-linear state (list (intern "BIND") count offs))
    ;; Linearize each binding value and store using actual offsets
    (h0-linearize-let-bindings vals offs 0 state)
    ;; Linearize body
    (let ((body-temp (h0-linearize-expr body state)))
      ;; Emit unbind
      (h0-emit-linear state (list (intern "UNBIND") count))
      body-temp)))

(defun h0-linearize-let-bindings (vals offsets idx state)
  (if vals
      (let* ((val-temp (h0-linearize-expr (car vals) state))
             (offset (if offsets (car offsets) idx)))
        (h0-emit-linear state (list (intern "STORE-BINDING") offset val-temp))
        (h0-linearize-let-bindings (cdr vals)
                                    (if offsets (cdr offsets) nil)
                                    (+ idx 1)
                                    state))
      nil))

;; Linearize function call
(defun h0-linearize-call (ir state)
  (let* ((name (car (cdr ir)))
         (args (car (cdr (cdr ir))))
         (arg-temps (h0-linearize-call-args args state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (cons (intern "CALL") (cons dst (cons name arg-temps))))
    dst))

(defun h0-linearize-call-args (args state)
  (if (null args)
      nil
      (cons (h0-linearize-expr (car args) state)
            (h0-linearize-call-args (cdr args) state))))

;; Linearize funcall (indirect call)
(defun h0-linearize-funcall (ir state)
  (let* ((fn-ir (car (cdr ir)))
         (args (car (cdr (cdr ir))))
         (fn-temp (h0-linearize-expr fn-ir state))
         (arg-temps (h0-linearize-call-args args state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (cons (intern "FUNCALL") (cons dst (cons fn-temp arg-temps))))
    dst))

;; Linearize variable assignment
(defun h0-linearize-setq (ir state)
  (let* ((off (car (cdr ir)))
         (val-ir (car (cdr (cdr ir))))
         (val-temp (h0-linearize-expr val-ir state)))
    (h0-emit-linear state (list (intern "SETQ") off val-temp))
    val-temp))  ; setq returns the value

;; Linearize cons cell creation
(defun h0-linearize-cons (ir state)
  (let* ((car-temp (h0-linearize-expr (car (cdr ir)) state))
         (cdr-temp (h0-linearize-expr (car (cdr (cdr ir))) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "CONS") dst car-temp cdr-temp))
    dst))

;; Linearize while loop
(defun h0-linearize-while (ir state)
  (let* ((loop-label (h0-fresh-label state))
         (end-label (h0-fresh-label state))
         (dst (h0-fresh-temp state)))
    ;; Result starts as nil
    (h0-emit-linear state (list (intern "LOAD-NIL") dst))
    (h0-emit-linear state (list (intern "LABEL") loop-label))
    ;; Test
    (let ((test-temp (h0-linearize-expr (car (cdr ir)) state)))
      (h0-emit-linear state (list (intern "JUMP-IF-NIL") test-temp end-label)))
    ;; Body
    (let ((body-temp (h0-linearize-expr (car (cdr (cdr ir))) state)))
      (h0-emit-linear state (list (intern "MOVE") dst body-temp)))
    (h0-emit-linear state (list (intern "JUMP") loop-label))
    (h0-emit-linear state (list (intern "LABEL") end-label))
    dst))

;; Forward declaration for mutual recursion
(defun h0-linearize-expr (ir state)
  (h0-linearize-expr-impl ir state))

;; Linearize any IR expression, returns temp holding result
(defun h0-linearize-expr-impl (ir state)
  (if (h0-linear-leaf-p ir)
      (h0-linearize-leaf ir state)
      (let ((tag (car ir)))
        (case tag
          ;; Binary arithmetic (with and without -IR suffix)
          ((ADD ADD-IR) (h0-linearize-binary (intern "ADD") ir state))
          ((SUB SUB-IR) (h0-linearize-binary (intern "SUB") ir state))
          ((MUL MUL-IR) (h0-linearize-binary (intern "MUL") ir state))
          ((DIV DIV-IR) (h0-linearize-binary (intern "DIV") ir state))
          ((MOD MOD-IR) (h0-linearize-binary (intern "MOD") ir state))
          ;; Comparisons
          (CMP-EQ (h0-linearize-binary (intern "CMP-EQ") ir state))
          (CMP-LT (h0-linearize-binary (intern "CMP-LT") ir state))
          (CMP-GT (h0-linearize-binary (intern "CMP-GT") ir state))
          (CMP-LE (h0-linearize-binary (intern "CMP-LE") ir state))
          (CMP-GE (h0-linearize-binary (intern "CMP-GE") ir state))
          ;; Bitwise binary
          (BAND (h0-linearize-binary (intern "BAND") ir state))
          (BOR (h0-linearize-binary (intern "BOR") ir state))
          (BXOR (h0-linearize-binary (intern "BXOR") ir state))
          (BSH (h0-linearize-binary (intern "BSH") ir state))
          ;; List mutations
          (SETCAR-IR (h0-linearize-binary (intern "SETCAR") ir state))
          (SETCDR-IR (h0-linearize-binary (intern "SETCDR") ir state))
          ;; String binary
          (STRING-REF-IR (h0-linearize-binary (intern "STRING-REF") ir state))
          (STRING-CONCAT-IR (h0-linearize-binary (intern "STRING-CONCAT") ir state))
          (STRING-EQUAL-IR (h0-linearize-binary (intern "STRING-EQUAL") ir state))
          ;; Vector binary
          (VECTOR-REF-IR (h0-linearize-binary (intern "VECTOR-REF") ir state))
          ;; Buffer binary
          (BUFFER-BYTE-REF-IR (h0-linearize-binary (intern "BUFFER-BYTE-REF") ir state))
          ;; Unary operations
          (BNOT (h0-linearize-unary (intern "BNOT") ir state))
          (CAR-IR (h0-linearize-unary (intern "CAR") ir state))
          (CDR-IR (h0-linearize-unary (intern "CDR") ir state))
          (STRING-LENGTH-IR (h0-linearize-unary (intern "STRING-LENGTH") ir state))
          (MAKE-STRING-FROM-VECTOR-IR (h0-linearize-unary (intern "MAKE-STRING-FROM-VECTOR") ir state))
          (SYMBOL-NAME-IR (h0-linearize-unary (intern "SYMBOL-NAME") ir state))
          (MAKE-SYMBOL-IR (h0-linearize-unary (intern "MAKE-SYMBOL") ir state))
          (MAKE-SYMBOL-FROM-STRING-IR (h0-linearize-unary (intern "MAKE-SYMBOL") ir state))
          (MAKE-VECTOR-IR (h0-linearize-unary (intern "MAKE-VECTOR") ir state))
          (VECTOR-LENGTH-IR (h0-linearize-unary (intern "VECTOR-LENGTH") ir state))
          (GET-TAG (h0-linearize-unary (intern "GET-TAG") ir state))
          ;; List cons
          (CONS-IR (h0-linearize-cons ir state))
          ;; Vector/buffer set (ternary)
          (VECTOR-SET-IR (h0-linearize-vector-set ir state))
          (BUFFER-BYTE-SET-IR (h0-linearize-buffer-byte-set ir state))
          ;; Control flow
          (IF-IR (h0-linearize-if ir state))
          (WHILE-IR (h0-linearize-while ir state))
          (PROGN-IR (h0-linearize-progn ir state))
          ;; Bindings
          (LET-IR (h0-linearize-let ir state))
          (LET*-IR (h0-linearize-let ir state))
          (SETQ-IR (h0-linearize-setq ir state))
          ;; Function calls
          (CALL-FN (h0-linearize-call ir state))
          (FUNCALL-IR (h0-linearize-funcall ir state))
          ;; System calls
          (SYS-EXIT-IR (h0-linearize-sys-exit ir state))
          (SYS-OPEN-IR (h0-linearize-sys-open ir state))
          (SYS-READ-IR (h0-linearize-sys-read ir state))
          (SYS-WRITE-IR (h0-linearize-sys-write ir state))
          (SYS-CLOSE-IR (h0-linearize-sys-close ir state))
          ;; Global vars
          (SET-GLOBAL-VARS-IR (h0-linearize-set-global-vars ir state))
          ;; Memory operations
          (MEM-SET-BYTE-IR (h0-linearize-mem-set-byte ir state))
          (MEM-LOAD-64-IR (h0-linearize-mem-load-64 ir state))
          ;; Block/return-from
          (BLOCK-IR (h0-linearize-block ir state))
          (RETURN-FROM-IR (h0-linearize-return-from ir state))
          ;; Loop/continue
          (LOOP-IR (h0-linearize-loop ir state))
          (CONTINUE-IR (h0-linearize-continue ir state))
          ;; Buffer to string
          (BUFFER-TO-STRING-IR (h0-linearize-buffer-to-string ir state))
          ;; Symbol table
          (GET-SYMTAB-OFFSET-IR (h0-linearize-get-symtab-offset ir state))
          ;; Unknown IR type
          (t (error "linearize-expr: unknown IR type"))))))

;; Helper functions for complex operations

(defun h0-linearize-vector-set (ir state)
  (let* ((vec-temp (h0-linearize-expr (car (cdr ir)) state))
         (idx-temp (h0-linearize-expr (car (cdr (cdr ir))) state))
         (val-temp (h0-linearize-expr (car (cdr (cdr (cdr ir)))) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "VECTOR-SET") dst vec-temp idx-temp val-temp))
    dst))

(defun h0-linearize-buffer-byte-set (ir state)
  (let* ((buf-temp (h0-linearize-expr (car (cdr ir)) state))
         (idx-temp (h0-linearize-expr (car (cdr (cdr ir))) state))
         (val-temp (h0-linearize-expr (car (cdr (cdr (cdr ir)))) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "BUFFER-BYTE-SET") dst buf-temp idx-temp val-temp))
    dst))

(defun h0-linearize-sys-exit (ir state)
  (let ((arg-temp (h0-linearize-expr (car (cdr ir)) state)))
    (h0-emit-linear state (list (intern "SYS-EXIT") arg-temp))
    arg-temp))

(defun h0-linearize-sys-open (ir state)
  (let* ((path-temp (h0-linearize-expr (car (cdr ir)) state))
         (flags-temp (h0-linearize-expr (car (cdr (cdr ir))) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "SYS-OPEN") dst path-temp flags-temp))
    dst))

(defun h0-linearize-sys-read (ir state)
  (let* ((fd-temp (h0-linearize-expr (car (cdr ir)) state))
         (buf-temp (h0-linearize-expr (car (cdr (cdr ir))) state))
         (len-temp (h0-linearize-expr (car (cdr (cdr (cdr ir)))) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "SYS-READ") dst fd-temp buf-temp len-temp))
    dst))

(defun h0-linearize-sys-write (ir state)
  (let* ((fd-temp (h0-linearize-expr (car (cdr ir)) state))
         (buf-temp (h0-linearize-expr (car (cdr (cdr ir))) state))
         (len-temp (h0-linearize-expr (car (cdr (cdr (cdr ir)))) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "SYS-WRITE") dst fd-temp buf-temp len-temp))
    dst))

(defun h0-linearize-sys-close (ir state)
  (let* ((fd-temp (h0-linearize-expr (car (cdr ir)) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "SYS-CLOSE") dst fd-temp))
    dst))

(defun h0-linearize-set-global-vars (ir state)
  (let ((val-temp (h0-linearize-expr (car (cdr ir)) state)))
    (h0-emit-linear state (list (intern "SET-GLOBAL-VARS") val-temp))
    val-temp))

(defun h0-linearize-mem-set-byte (ir state)
  (let* ((ptr-temp (h0-linearize-expr (car (cdr ir)) state))
         (off-temp (h0-linearize-expr (car (cdr (cdr ir))) state))
         (val-temp (h0-linearize-expr (car (cdr (cdr (cdr ir)))) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "MEM-SET-BYTE") dst ptr-temp off-temp val-temp))
    dst))

(defun h0-linearize-mem-load-64 (ir state)
  (let* ((ptr-temp (h0-linearize-expr (car (cdr ir)) state))
         (off-temp (h0-linearize-expr (car (cdr (cdr ir))) state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "MEM-LOAD-64") dst ptr-temp off-temp))
    dst))

(defun h0-linearize-block (ir state)
  (let* ((block-id (car (cdr ir)))
         (body-ir (car (cdr (cdr ir))))
         (end-label (h0-fresh-label state))
         (dst (h0-fresh-temp state)))
    ;; Emit block start marker, body, then end marker
    (h0-emit-linear state (list (intern "BLOCK-START") block-id end-label dst))
    (let ((body-temp (h0-linearize-expr body-ir state)))
      ;; Move body result to dst
      (h0-emit-linear state (list (intern "MOVE") dst body-temp)))
    (h0-emit-linear state (list (intern "LABEL") end-label))
    dst))

(defun h0-linearize-return-from (ir state)
  (let* ((block-id (car (cdr ir)))
         (value-ir (car (cdr (cdr ir))))
         (val-temp (h0-linearize-expr value-ir state)))
    (h0-emit-linear state (list (intern "RETURN-FROM") block-id val-temp))
    ;; Return nil since we're jumping away
    (let ((dst (h0-fresh-temp state)))
      (h0-emit-linear state (list (intern "LOAD-NIL") dst))
      dst)))

(defun h0-linearize-loop (ir state)
  (let* ((body-ir (car (cdr ir)))
         (loop-label (h0-fresh-label state))
         (dst (h0-fresh-temp state)))
    ;; Emit loop start marker
    (h0-emit-linear state (list (intern "LOOP-START") loop-label))
    (h0-emit-linear state (list (intern "LABEL") loop-label))
    ;; Linearize body
    (let ((body-temp (h0-linearize-expr body-ir state)))
      (h0-emit-linear state (list (intern "MOVE") dst body-temp)))
    dst))

(defun h0-linearize-continue (ir state)
  (let* ((args (car (cdr ir)))
         (arg-temps (h0-linearize-call-args args state))
         (dst (h0-fresh-temp state)))
    ;; Emit stores for each arg to param slots
    (h0-linearize-continue-stores arg-temps 0 state)
    ;; Emit continue marker
    (h0-emit-linear state (list (intern "CONTINUE")))
    dst))

(defun h0-linearize-continue-stores (arg-temps idx state)
  (if arg-temps
      (let ((arg-temp (car arg-temps)))
        (h0-emit-linear state (list (intern "STORE-PARAM") arg-temp idx))
        (h0-linearize-continue-stores (cdr arg-temps) (+ idx 1) state))
      nil))

(defun h0-linearize-buffer-to-string (ir state)
  (let* ((buf-ir (car (cdr ir)))
         (len-ir (car (cdr (cdr ir))))
         (buf-temp (h0-linearize-expr buf-ir state))
         (len-temp (h0-linearize-expr len-ir state))
         (dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "BUFFER-TO-STRING") dst buf-temp len-temp))
    dst))

(defun h0-linearize-get-symtab-offset (ir state)
  (let ((dst (h0-fresh-temp state)))
    (h0-emit-linear state (list (intern "GET-SYMTAB-OFFSET") dst))
    dst))

;; Entry point: Convert tree IR to linear IR
;; Returns a list of linear instructions in execution order
(defun h0-linearize (ir)
  (let* ((state (h0-make-linear-state))
         (result-temp (h0-linearize-expr ir state)))
    ;; Add final instruction to mark result
    (h0-emit-linear state (list (intern "RESULT") result-temp))
    ;; Return in execution order
    (h0-get-linear-output state)))

;;; ==========================================================================
;;; ARM64 Code Generation - IR to machine code
;;; ==========================================================================
;;; Generates ARM64 machine code bytes from IR
;;; Uses tagged fixnum representation: value << 4
;;; Registers:
;;;   x0-x4   - arguments and return value
;;;   x20     - environment base (stack frame)
;;;   x28     - heap bump pointer

;; ARM64 instructions are called directly via arm64:* with keyword registers


;; Append byte lists
(defun bytes-append (a b)
  (if (null a) b
      (cons (car a) (bytes-append (cdr a) b))))

(defun bytes-append-all (lists)
  (if (null lists)
      nil
      (bytes-append (car lists) (bytes-append-all (cdr lists)))))

;; Temp slot offset calculation:
;; Temp slots start at sp+48 (#x30) to avoid overlap with saved registers (sp+0..sp+40)
;; Formula: 48 + td*8 = #x30 + (* td #x8)
;; Note: Inlined everywhere because function calls have overhead in native code

;; Generate STRB instructions for string characters
;; MUST be defined before h0-codegen-str-lit which calls it
(defun h0-gen-str-bytes (str idx)
  (if (>= idx (string-length str))
      nil
      (let* ((char (string-ref str idx))
             ;; MOVZ x0, #char
             (mov-char (movz :x0 char))
             ;; STRB w0, [x1, #idx]
             (store-byte (strb :x0 :x1 idx)))
        (bytes-append mov-char
                      (bytes-append store-byte
                                    (h0-gen-str-bytes str (+ idx 1)))))))

;; Codegen helper for string literals
;; Allocates string on heap: [length:8][chars:N][padding]
;; Returns tagged pointer with tag 4
;; MUST be defined before h0-codegen which calls it
(defun h0-codegen-str-lit (str len total-size)
  (let* (;; Store length at x28
         (mov-len-lo (movz :x0 (logand len #xFFFF)))
         (str-len (str :x0 :x28 :offset 0))
         ;; Get string base address (x28 + 8)
         (add-base (add :x1 :x28 8 :imm t))
         ;; Generate STRB instructions for each character
         (char-stores (h0-gen-str-bytes str 0))
         ;; Save tagged pointer to x0: x28 | 4
         (mov-ptr (mov :x0 :x28))
         (tag-ptr (add :x0 :x0 4 :imm t))
         ;; Bump heap pointer
         (bump-heap (add :x28 :x28 total-size :imm t)))
    (bytes-append-all (list mov-len-lo str-len add-base
                            char-stores mov-ptr tag-ptr bump-heap))))

;; Codegen helper for keyword literals
;; Same layout as strings but with tag 7 instead of 4
;; MUST be defined before h0-codegen which calls it
(defun h0-codegen-kw-lit (str len total-size)
  (let* (;; Store length at x28
         (mov-len-lo (movz :x0 (logand len #xFFFF)))
         (str-len (str :x0 :x28 :offset 0))
         ;; Get string base address (x28 + 8)
         (add-base (add :x1 :x28 8 :imm t))
         ;; Generate STRB instructions for each character
         (char-stores (h0-gen-str-bytes str 0))
         ;; Save tagged pointer to x0: x28 | 7 (keyword tag)
         (mov-ptr (mov :x0 :x28))
         (tag-ptr (add :x0 :x0 7 :imm t))
         ;; Bump heap pointer
         (bump-heap (add :x28 :x28 total-size :imm t)))
    (bytes-append-all (list mov-len-lo str-len add-base
                            char-stores mov-ptr tag-ptr bump-heap))))

;; Codegen helper for binary operations
;; Inline temp slot calculation: 48 + td*8
(defun h0-codegen-binop (left-ir right-ir op-instrs td)
  (let* ((slot-off (+ 48 (* td 8)))
         (left-code (h0-codegen left-ir td))
         (save-left (str :x0 :sp :offset slot-off))
         (right-code (h0-codegen right-ir (+ td 1)))
         (move-right (mov :x1 :x0))
         (load-left (ldr :x0 :sp :offset slot-off)))
    (bytes-append-all
     (list left-code save-left right-code move-right load-left op-instrs))))

;; Codegen helper for comparisons
;; Inline temp slot calculation: 48 + td*8
(defun h0-codegen-cmp (left-ir right-ir cond td)
  (let* ((slot-off (+ 48 (* td 8)))
         (left-code (h0-codegen left-ir td))
         (save-left (str :x0 :sp :offset slot-off))
         (right-code (h0-codegen right-ir (+ td 1)))
         (move-right (mov :x1 :x0))
         (load-left (ldr :x0 :sp :offset slot-off)))
    (let* ((cmp-code (cmp :x0 :x1))
           (cset-code (cset :x0 cond))
           (tag-code (lsl :x0 :x0 4 :imm t)))
      (bytes-append-all
       (list left-code save-left right-code move-right load-left
             cmp-code cset-code tag-code)))))

;; Codegen helper for progn (list of IR forms)
(defun h0-codegen-progn (forms td)
  (if (null forms)
      (movz :x0 0)
      (if (null (cdr forms))
          (h0-codegen (car forms) td)
          (bytes-append (h0-codegen (car forms) td)
                        (h0-codegen-progn (cdr forms) td)))))

;; Helper: Generate code for FUNCALL (0-2 args)
;; Closure structure (assumed): [env_ptr:8][code_ptr:8] with tag 5
;; ARM64 calling convention: x0-x7 for args, x24 for closure env
(defun h0-codegen-funcall (fn-ir args-ir num-args td)
  (let* (;; Evaluate function expression to x0 (closure pointer)
         (fn-code (h0-codegen fn-ir td))
         ;; Save closure to temp slot 0
         (fn-slot (+ 48 (* td 8)))
         (save-fn (str :x0 :sp :offset fn-slot))
         ;; Generate code to evaluate and save arguments
         (arg-code-list (h0-codegen-funcall-args args-ir (+ td 1) 0 nil))
         ;; arg-code-list is now a list of code sequences
         (arg-code (if arg-code-list
                       (bytes-append-all arg-code-list)
                       nil)))
    ;; Now load arguments back to x0, x1, etc. and call
    (let* (;; Load closure from temp slot 0
           (load-fn (ldr :x16 :sp :offset fn-slot))
           ;; Untag closure (subtract 5)
           (untag-fn (sub :x16 :x16 5 :imm t))
           ;; Load env pointer to x24 from [x16+0]
           (load-env (ldr :x24 :x16 :offset 0))
           ;; Load code pointer from [x16+8] directly to x16
           (load-code (ldr :x16 :x16 :offset 8))
           ;; Load args back to registers
           (load-args (h0-codegen-funcall-load-args num-args (+ td 1)))
           ;; Call via BLR x16
           (call (blr :x16)))
      (if arg-code
          (bytes-append-all (list fn-code save-fn arg-code load-fn untag-fn
                                  load-env load-code load-args call))
          (bytes-append-all (list fn-code save-fn load-fn untag-fn
                                  load-env load-code load-args call))))))

;; Helper: Generate code to evaluate and save arguments
;; Returns list of code sequences
(defun h0-codegen-funcall-args (args-ir td idx acc)
  (if (null args-ir)
      (reverse acc)
      (let* ((arg-code (h0-codegen (car args-ir) td))
             (slot-off (+ 48 (* td 8)))
             (save-code (str :x0 :sp :offset slot-off))
             (combined (bytes-append arg-code save-code)))
        (h0-codegen-funcall-args (cdr args-ir) (+ td 1) (+ idx 1)
                                 (cons combined acc)))))

;; Helper: Generate code to load arguments back to x0, x1, etc.
;; Supports up to 8 arguments (x0-x7) for ARM64 calling convention
(defun h0-codegen-funcall-load-args (num-args td)
  (h0-codegen-funcall-load-args-loop num-args td 0 nil))

;; Helper: Loop to generate LDR instructions for each argument
(defun h0-codegen-funcall-load-args-loop (num-args td idx acc)
  (if (>= idx num-args)
      (if acc
          (bytes-append-all (reverse acc))
          nil)
      (let* ((slot-off (+ 48 (* (+ td idx) 8)))
             (reg (cond ((= idx 0) :x0)
                        ((= idx 1) :x1)
                        ((= idx 2) :x2)
                        ((= idx 3) :x3)
                        ((= idx 4) :x4)
                        ((= idx 5) :x5)
                        ((= idx 6) :x6)
                        ((= idx 7) :x7)
                        (t (fatal-error "h0-codegen-funcall-load-args: too many args"))))
             (load-code (ldr reg :sp :offset slot-off)))
        (h0-codegen-funcall-load-args-loop num-args td (+ idx 1) (cons load-code acc)))))

;;; ============================================================
;;; Linear IR Codegen (iterative, no recursion)
;;; ============================================================
;;; Generates ARM64 code from linear IR by simple iteration.
;;; Each temp slot maps to a stack location.
;;; This replaces the recursive tree-walking codegen for self-hosting.

;; Helper: Calculate stack offset for linear temp slot
;; Uses temp area 0x40-0x3840 = 1792 slots (frame is 16KB)
(defun h0-linear-temp-slot (temp)
  (if (>= temp 1792)
      (fatal-error "h0-linear-temp-slot: temp exceeds 1792 slot limit")
      (+ #x40 (* temp 8))))

;; Helper: Load temp slot into register
(defun h0-linear-load-temp (rd temp)
  (ldr rd :sp :offset (h0-linear-temp-slot temp)))

;; Helper: Save x0 to temp slot
(defun h0-linear-save-temp (temp)
  (str :x0 :sp :offset (h0-linear-temp-slot temp)))

;; Helper: Generate code to load tagged fixnum literal into x0
(defun h0-linear-load-lit (val)
  (let ((tagged (ash val 4)))
    (cond
      ;; Small positive: single movz
      ((and (>= tagged 0) (< tagged #x10000))
       (movz :x0 tagged))
      ;; Small negative: use movn (move wide with NOT)
      ((and (< tagged 0) (>= tagged (- #x10000)))
       (movn :x0 (logand (lognot tagged) #xFFFF)))
      ;; Large positive: movz + movk
      ((>= tagged 0)
       (bytes-append (movz :x0 (logand tagged #xFFFF))
                     (movk :x0 (logand (ash tagged -16) #xFFFF) :lsl 16)))
      ;; Large negative: movn + movk for upper bits
      (t
       (let ((inv (lognot tagged)))
         (bytes-append-all
          (list (movn :x0 (logand inv #xFFFF))
                (movk :x0 (logand (ash tagged -16) #xFFFF) :lsl 16)
                (movk :x0 (logand (ash tagged -32) #xFFFF) :lsl 32)
                (movk :x0 (logand (ash tagged -48) #xFFFF) :lsl 48))))))))

;; Helper: Convert boolean 0/1 in x0 to tagged nil(6)/t(16)
;; Uses: x0 = 1 (true) or 0 (false)
;; Result: x0 = 16 (t) or 6 (nil)
(defun h0-gen-bool-to-tagged ()
  (bytes-append-all
   (list (neg :x0 :x0)
         (movz :x1 10)
         (and* :x0 :x0 :x1)
         (add :x0 :x0 6 :imm t))))

;; NOTE: Stub helper functions - these are referenced by bootstrap codegen
;; but not yet implemented in habu0. For now, return nil or simple values.
(defun h0-gen-symbol-lit (name-str len total-size)
  ;; TODO: Implement symbol literal generation
  nil)

(defun h0-gen-string-lit (str len total-size)
  ;; TODO: Use h0-codegen-str-lit or implement inline
  nil)

(defun h0-gc-trigger-code ()
  ;; TODO: Implement GC trigger check
  nil)

(defun h0-load-addr-8 (rd addr)
  ;; Load address into register (8 bytes = 2 instructions)
  (bytes-append (movz rd (logand addr #xFFFF))
                (movk rd (ash addr -16) :lsl 16)))

(defun h0-lookup-string (name fnoffs)
  ;; Look up function name in fnoffs alist
  ;; Returns (name . offset) or nil
  (h0-lookup-string-helper name fnoffs))

(defun h0-lookup-string-helper (name lst)
  (if (null lst)
      nil
      (let ((entry (car lst)))
        (if (string-equal (symbol-name name) (symbol-name (car entry)))
            entry
            (h0-lookup-string-helper name (cdr lst))))))

;; Main instruction codegen for linear IR
;; This is a massive case statement handling all instruction types
(defun h0-codegen-linear-instr (instr rtaddrs fnoffs)
  (let ((op (car instr)))
    (cond
      ;; Load literal integer
      ((eq op 'load-lit)
       (let ((dst (cadr instr))
             (val (caddr instr)))
         (bytes-append (h0-linear-load-lit val)
                       (h0-linear-save-temp dst))))

      ;; Load nil constant
      ((eq op 'load-nil)
       (let ((dst (cadr instr)))
         (bytes-append (movz :x0 6)
                       (h0-linear-save-temp dst))))

      ;; Load variable from environment
      ((eq op 'load-var)
       (let ((dst (cadr instr))
             (offset (caddr instr)))
         (bytes-append-all
          (list (sub :x1 :x20 (* offset 8) :imm t)
                (ldr :x0 :x1 :offset 0)
                (h0-linear-save-temp dst)))))

      ;; Load symbol (allocate on heap)
      ((eq op 'load-sym)
       (let* ((dst (cadr instr))
              (name (caddr instr))
              (name-str (symbol-name name))
              (len (string-length name-str))
              (total-size (logand (+ len 8 15) (lognot 15))))
         (bytes-append (h0-gen-symbol-lit name-str len total-size)
                       (h0-linear-save-temp dst))))

      ;; Load string literal
      ((eq op 'load-str)
       (let* ((dst (cadr instr))
              (str (caddr instr))
              (len (string-length str))
              (total-size (logand (+ len 8 15) (lognot 15))))
         (bytes-append (h0-gen-string-lit str len total-size)
                       (h0-linear-save-temp dst))))

      ;; Binary arithmetic operations
      ((eq op 'add)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (add :x0 :x0 :x1)
                (h0-linear-save-temp dst)))))

      ((eq op 'sub)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (sub :x0 :x0 :x1)
                (h0-linear-save-temp dst)))))

      ((eq op 'mul)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (lsr :x1 :x1 4 :imm t)
                (mul :x0 :x0 :x1)
                (h0-linear-save-temp dst)))))

      ((eq op 'div)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (sdiv :x0 :x0 :x1)
                (lsl :x0 :x0 4 :imm t)
                (h0-linear-save-temp dst)))))

      ((eq op 'mod)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (lsr :x0 :x0 4 :imm t)
                (lsr :x1 :x1 4 :imm t)
                (sdiv :x2 :x0 :x1)
                (mul :x2 :x2 :x1)
                (sub :x0 :x0 :x2)
                (lsl :x0 :x0 4 :imm t)
                (h0-linear-save-temp dst)))))

      ;; Comparison operations
      ((eq op 'cmp-eq)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (cmp :x0 :x1)
                (cset :x0 0)
                (h0-gen-bool-to-tagged)
                (h0-linear-save-temp dst)))))

      ((eq op 'cmp-lt)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (cmp :x0 :x1)
                (cset :x0 11)
                (h0-gen-bool-to-tagged)
                (h0-linear-save-temp dst)))))

      ((eq op 'cmp-gt)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (cmp :x0 :x1)
                (cset :x0 12)
                (h0-gen-bool-to-tagged)
                (h0-linear-save-temp dst)))))

      ((eq op 'cmp-le)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (cmp :x0 :x1)
                (cset :x0 13)
                (h0-gen-bool-to-tagged)
                (h0-linear-save-temp dst)))))

      ((eq op 'cmp-ge)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (cmp :x0 :x1)
                (cset :x0 10)
                (h0-gen-bool-to-tagged)
                (h0-linear-save-temp dst)))))

      ;; Bitwise operations
      ((eq op 'band)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (and* :x0 :x0 :x1)
                (h0-linear-save-temp dst)))))

      ((eq op 'bor)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (orr :x0 :x0 :x1)
                (h0-linear-save-temp dst)))))

      ((eq op 'bxor)
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src1)
                (h0-linear-load-temp :x1 src2)
                (eor :x0 :x0 :x1)
                (h0-linear-save-temp dst)))))

      ((eq op 'bnot)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src)
                (mvn :x0 :x0)
                (and* :x0 :x0 #xFFFFFFFFFFFFFFF0 :imm t)
                (h0-linear-save-temp dst)))))

      ;; List operations
      ((eq op 'cons)
       (let ((dst (cadr instr))
             (car-src (caddr instr))
             (cdr-src (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 car-src)
                (h0-linear-load-temp :x1 cdr-src)
                (str :x0 :x28 :offset 0)
                (str :x1 :x28 :offset 8)
                (mov :x0 :x28)
                (add :x0 :x0 1 :imm t)
                (add :x28 :x28 16 :imm t)
                (h0-linear-save-temp dst)))))

      ((eq op 'car)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src)
                (sub :x0 :x0 1 :imm t)
                (ldr :x0 :x0 :offset 0)
                (h0-linear-save-temp dst)))))

      ((eq op 'cdr)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src)
                (sub :x0 :x0 1 :imm t)
                (ldr :x0 :x0 :offset 8)
                (h0-linear-save-temp dst)))))

      ;; Cons cell mutation
      ((eq op 'setcar)
       (let ((dst (cadr instr))
             (cell (caddr instr))
             (val (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 cell)
                (h0-linear-load-temp :x1 val)
                (sub :x0 :x0 1 :imm t)           ; untag cons ptr
                (str :x1 :x0 :offset 0)          ; store to car slot
                (mov :x0 :x1)                     ; return value
                (h0-linear-save-temp dst)))))

      ((eq op 'setcdr)
       (let ((dst (cadr instr))
             (cell (caddr instr))
             (val (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 cell)
                (h0-linear-load-temp :x1 val)
                (sub :x0 :x0 1 :imm t)           ; untag cons ptr
                (str :x1 :x0 :offset 8)          ; store to cdr slot
                (mov :x0 :x1)                     ; return value
                (h0-linear-save-temp dst)))))

      ;; Get tag bits
      ((eq op 'get-tag)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src)
                (and* :x0 :x0 #xF :imm t)
                (lsl :x0 :x0 4 :imm t)
                (h0-linear-save-temp dst)))))

      ;; Control flow markers (handled in main codegen loop)
      ((eq op 'label) nil)
      ((eq op 'jump) (b 0))
      ((eq op 'jump-if-nil)
       (let ((src (cadr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src)
                (cmp :x0 6 :imm t)
                (b.eq 0)))))
      ((eq op 'move)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src)
                (h0-linear-save-temp dst)))))

      ;; Block/return-from (handled specially in main loop)
      ((eq op 'block-start) nil)
      ((eq op 'return-from) nil)
      ((eq op 'loop-start) nil)
      ((eq op 'continue) (list (list :continue)))
      ((eq op 'bind) nil)
      ((eq op 'unbind) nil)

      ;; Store operations for TCO
      ((eq op 'store-param)
       (let ((src-temp (cadr instr))
             (param-idx (caddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x16 src-temp)
                (sub :x10 :x20 (* param-idx 8) :imm t)
                (str :x16 :x10 :offset 0)))))

      ((eq op 'store-binding)
       (let ((offset (cadr instr))
             (src (caddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src)
                (sub :x1 :x20 (* offset 8) :imm t)
                (str :x0 :x1 :offset 0)))))

      ((eq op 'setq)
       (let ((offset (cadr instr))
             (src (caddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src)
                (sub :x1 :x20 (* offset 8) :imm t)
                (str :x0 :x1 :offset 0)))))

      ;; Function result marker
      ((eq op 'result)
       (let ((src (cadr instr)))
         (h0-linear-load-temp :x0 src)))

      ;; System calls
      ((eq op 'sys-exit)
       (let ((src (cadr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 src)
                (asr :x0 :x0 4 :imm t)    ; untag exit code
                (movz :x16 1)             ; syscall number for exit
                (svc 0)))))               ; supervisor call

      ((eq op 'sys-open)
       (let ((dst (cadr instr))
             (path-temp (caddr instr))
             (flags-temp (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 path-temp)
                (sub :x0 :x0 4 :imm t)    ; untag string
                (add :x0 :x0 8 :imm t)    ; skip length
                (h0-linear-load-temp :x1 flags-temp)
                (lsr :x1 :x1 4 :imm t)    ; untag flags
                (movz :x2 0)              ; mode = 0
                (list :extern-call "_open")
                (lsl :x0 :x0 4 :imm t)    ; tag result
                (h0-linear-save-temp dst)))))

      ((eq op 'sys-read)
       (let ((dst (cadr instr))
             (fd-temp (caddr instr))
             (buf-temp (cadddr instr))
             (len-temp (car (cddddr instr))))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 fd-temp)
                (lsr :x0 :x0 4 :imm t)
                (h0-linear-load-temp :x1 buf-temp)
                (sub :x1 :x1 3 :imm t)    ; untag vector
                (add :x1 :x1 8 :imm t)    ; skip length
                (h0-linear-load-temp :x2 len-temp)
                (lsr :x2 :x2 4 :imm t)
                (list :extern-call "_read")
                (lsl :x0 :x0 4 :imm t)
                (h0-linear-save-temp dst)))))

      ((eq op 'sys-write)
       (let ((dst (cadr instr))
             (fd-temp (caddr instr))
             (buf-temp (cadddr instr))
             (len-temp (car (cddddr instr))))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 fd-temp)
                (lsr :x0 :x0 4 :imm t)
                (h0-linear-load-temp :x1 buf-temp)
                (sub :x1 :x1 3 :imm t)    ; untag vector
                (add :x1 :x1 8 :imm t)    ; skip length
                (h0-linear-load-temp :x2 len-temp)
                (lsr :x2 :x2 4 :imm t)
                (list :extern-call "_write")
                (lsl :x0 :x0 4 :imm t)
                (h0-linear-save-temp dst)))))

      ((eq op 'sys-close)
       (let ((dst (cadr instr))
             (fd-temp (caddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 fd-temp)
                (lsr :x0 :x0 4 :imm t)
                (list :extern-call "_close")
                (lsl :x0 :x0 4 :imm t)
                (h0-linear-save-temp dst)))))

      ;; Vector operations
      ((eq op 'vector-set)
       (let ((dst (cadr instr))
             (vec-temp (caddr instr))
             (idx-temp (cadddr instr))
             (val-temp (car (cddddr instr))))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 vec-temp)
                (sub :x0 :x0 3 :imm t)    ; untag vector
                (h0-linear-load-temp :x1 idx-temp)
                (lsr :x1 :x1 4 :imm t)    ; untag index
                (add :x1 :x1 1 :imm t)    ; skip length slot
                (lsl :x1 :x1 3 :imm t)    ; *8 for offset
                (add :x0 :x0 :x1)         ; x0 = address
                (h0-linear-load-temp :x2 val-temp)
                (str :x2 :x0 :offset 0)
                (mov :x0 :x2)             ; return value
                (h0-linear-save-temp dst)))))

      ;; Buffer operations
      ((eq op 'buffer-byte-set)
       (let ((dst (cadr instr))
             (buf-temp (caddr instr))
             (idx-temp (cadddr instr))
             (val-temp (car (cddddr instr))))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 buf-temp)
                (sub :x0 :x0 3 :imm t)    ; untag vector
                (add :x0 :x0 8 :imm t)    ; skip length
                (h0-linear-load-temp :x1 idx-temp)
                (lsr :x1 :x1 4 :imm t)
                (add :x0 :x0 :x1)         ; x0 = address
                (h0-linear-load-temp :x2 val-temp)
                (lsr :x2 :x2 4 :imm t)
                (strb :x2 :x0 :offset 0)
                (h0-linear-save-temp dst)))))

      ;; Memory operations
      ((eq op 'mem-set-byte)
       (let ((dst (cadr instr))
             (ptr-temp (caddr instr))
             (off-temp (cadddr instr))
             (val-temp (car (cddddr instr))))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 ptr-temp)
                (lsr :x0 :x0 4 :imm t)
                (h0-linear-load-temp :x1 off-temp)
                (lsr :x1 :x1 4 :imm t)
                (add :x0 :x0 :x1)
                (h0-linear-load-temp :x2 val-temp)
                (lsr :x2 :x2 4 :imm t)
                (strb :x2 :x0 :offset 0)
                (movz :x0 6)              ; return nil
                (h0-linear-save-temp dst)))))

      ((eq op 'mem-load-64)
       (let ((dst (cadr instr))
             (ptr-temp (caddr instr))
             (off-temp (cadddr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 ptr-temp)
                (lsr :x0 :x0 4 :imm t)
                (h0-linear-load-temp :x1 off-temp)
                (lsr :x1 :x1 4 :imm t)
                (add :x0 :x0 :x1)
                (ldr :x0 :x0 :offset 0)
                (lsl :x0 :x0 4 :imm t)    ; tag as fixnum
                (h0-linear-save-temp dst)))))

      ;; Global variables
      ((eq op 'set-global-vars)
       (let ((val-temp (cadr instr)))
         (bytes-append-all
          (list (h0-linear-load-temp :x0 val-temp)
                (str :x0 :x27 :offset 104)))))

      ((eq op 'get-global-vars)
       (let ((dst (cadr instr)))
         (bytes-append-all
          (list (ldr :x0 :x27 :offset 104)
                (h0-linear-save-temp dst)))))

      ;; Command line args
      ((eq op 'get-cmdline-args)
       (let ((dst (cadr instr)))
         ;; Return nil for now - full implementation would build list from argv
         (bytes-append (movz :x0 6)       ; nil
                       (h0-linear-save-temp dst))))

      ;; Lambda operations
      ((eq op 'load-lambda)
       (let* ((dst (cadr instr))
              (name (caddr instr))
              (free-offsets (cadddr instr))
              ;; Look up function offset
              (fn-entry (h0-lookup-string name fnoffs)))
         (if (null fn-entry)
             (fatal-error "Function not found in fnoffs")
             (let ((fn-offset (cdr fn-entry)))
               (if (null free-offsets)
                   ;; No captures - simple closure
                   (bytes-append-all
                    (list (h0-load-addr-8 :x0 (ash fn-offset 4))
                          (str :x0 :x28 :offset 0)
                          (movz :x0 0)        ; nil for empty env
                          (str :x0 :x28 :offset 8)
                          (mov :x0 :x28)
                          (add :x0 :x0 5 :imm t)  ; closure tag
                          (add :x28 :x28 16 :imm t)
                          (h0-linear-save-temp dst)))
                   ;; Has captures - build cons chain (simplified for now)
                   (fatal-error "load-lambda with captures not yet implemented in habu0"))))))

      ;; String conversion
      ((eq op 'buffer-to-string)
       (let ((dst (cadr instr))
             (buf-temp (caddr instr))
             (len-temp (cadddr instr)))
         (bytes-append-all
          (list ;; Load buf to x1, len to x5 (untagged)
                (h0-linear-load-temp :x1 buf-temp)
                (sub :x1 :x1 3 :imm t)      ; untag vector (tag 3)
                (add :x1 :x1 8 :imm t)      ; skip length header, x1 = data ptr
                (h0-linear-load-temp :x5 len-temp)
                (lsr :x5 :x5 4 :imm t)      ; untag length
                ;; Allocate string: store length at [x28]
                (str :x5 :x28 :offset 0)
                ;; x4 = alloc size = (8 + len + 15) & ~15
                (add :x4 :x5 23 :imm t)     ; x4 = len + 23
                (and* :x4 :x4 #xFFFFFFFFFFFFFFF0 :imm t) ; x4 = (len + 23) & ~15
                ;; x0 = string base, bump heap
                (mov :x0 :x28)
                (add :x28 :x28 :x4)
                ;; x2 = string data base = x0 + 8
                (add :x2 :x0 8 :imm t)
                ;; x3 = loop counter = 0
                (movz :x3 0)
                ;; Loop: copy bytes from buf to string
                (cmp :x3 :x5)               ; compare counter with length
                (b.ge 6)                    ; +6 instrs to exit
                (add :x4 :x1 :x3)           ; x4 = buf_data + x3
                (ldrb :x4 :x4 0)            ; x4 = byte at [x4]
                (strb :x4 :x2 :x3 :reg t)   ; [x2 + x3] = x4 (register offset)
                (add :x3 :x3 1 :imm t)      ; x3++
                (b -6)                      ; back to cmp (-6 instrs)
                ;; Tag result with string tag (4)
                (movz :x4 4)                ; x4 = string tag
                (orr :x0 :x0 :x4)           ; x0 |= tag
                (h0-linear-save-temp dst)))))

      ;; Symbol table operations
      ((eq op 'get-symtab-offset)
       ;; Load symtab offset from [x27+112]
       ;; Note: value is ALREADY tagged (pre-shifted << 4) in wrapper storage
       (let ((dst (cadr instr)))
         (bytes-append-all
          (list (ldr :x0 :x27 :offset 112)
                (h0-linear-save-temp dst)))))

      ;; Stub implementations for operations not yet fully ported
      (t
       (fatal-error "h0-codegen-linear-instr: unknown instruction")))))

;; Main codegen loop for linear IR
;; Processes instructions iteratively, tracking labels and fixing up branches
;; NOTE: This is a simplified version without full block/loop support yet
(defun h0-codegen-linear (linear-ir rtaddrs fnoffs)
  ;; For now, just generate code for each instruction without fixups
  ;; TODO: Add proper label tracking and branch fixup support
  (h0-codegen-linear-loop linear-ir rtaddrs fnoffs nil))

;; Helper for recursive iteration over linear IR
(defun h0-codegen-linear-loop (ir rtaddrs fnoffs acc)
  (if (null ir)
      (if acc
          (bytes-append-all (reverse acc))
          nil)
      (let* ((instr (car ir))
             (code (h0-codegen-linear-instr instr rtaddrs fnoffs)))
        (if code
            (h0-codegen-linear-loop (cdr ir) rtaddrs fnoffs (cons code acc))
            (h0-codegen-linear-loop (cdr ir) rtaddrs fnoffs acc)))))

;; Generate code for IR (using numeric tags)
;; td = temp slot depth (for nested expressions)
(defun h0-codegen (ir td)
  (cond
    ;; Literal - MOVZ x0, #(val << 4)
    ((h0-has-tag-n ir (ir-tag-lit))
     (let* ((val (cadr ir))
            (tagged (ash val 4)))
       (if (< tagged #x10000)
           (movz :x0 tagged)
           ;; Larger values need MOVZ + MOVK
           (let ((movz-code (movz :x0 (logand tagged #xFFFF)))
                 (movk-code (movk :x0 (logand (ash tagged -16) #xFFFF) :lsl 16)))
             (bytes-append movz-code movk-code)))))

    ;; String literal - allocate on heap
    ;; Layout: [length:8][chars:N][padding to 16]
    ;; Returns tagged pointer with tag 4
    ((h0-has-tag-n ir (ir-tag-str-lit))
     (let* ((str (cadr ir))
            (len (string-length str))
            ;; Round up (len + 8) to 16-byte boundary
            (total-size (logand (+ len 8 15) (lognot 15))))
       (h0-codegen-str-lit str len total-size)))

    ;; Keyword literal - allocate on heap
    ;; Layout: same as string [length:8][chars:N][padding to 16]
    ;; Returns tagged pointer with tag 7
    ((h0-has-tag-n ir (ir-tag-kw-lit))
     (let* ((kw (cadr ir))
            (str (keyword-name kw))
            (len (string-length str))
            ;; Round up (len + 8) to 16-byte boundary
            (total-size (logand (+ len 8 15) (lognot 15))))
       (h0-codegen-kw-lit str len total-size)))

    ;; Variable - load from stack frame at x20
    ((h0-has-tag-n ir (ir-tag-var))
     (let* ((off (cadr ir))
            (byte-off (* off 8))
            (sub-code (sub :x1 :x20 byte-off :imm t))
            (ldr-code (ldr :x0 :x1 :offset 0)))
       (bytes-append sub-code ldr-code)))

    ;; Addition
    ((h0-has-tag-n ir (ir-tag-add))
     (h0-codegen-binop (cadr ir) (caddr ir)
                       (add :x0 :x0 :x1)
                       td))

    ;; Subtraction
    ((h0-has-tag-n ir (ir-tag-sub))
     (h0-codegen-binop (cadr ir) (caddr ir)
                       (sub :x0 :x0 :x1)
                       td))

    ;; Multiplication (need to untag one operand)
    ((h0-has-tag-n ir (ir-tag-mul))
     (let* ((slot-off (+ 48 (* td 8)))
            (left-code (h0-codegen (cadr ir) td))
            (save-code (str :x0 :sp :offset slot-off))
            (right-code (h0-codegen (caddr ir) (+ td 1)))
            (untag-code (lsr :x1 :x0 4 :imm t))
            (load-code (ldr :x0 :sp :offset slot-off)))
       (let ((mul-code (mul :x0 :x0 :x1)))
         (bytes-append-all
          (list left-code save-code right-code untag-code load-code mul-code)))))

    ;; Division
    ((h0-has-tag-n ir (ir-tag-div))
     (let* ((slot-off (+ 48 (* td 8)))
            (left-code (h0-codegen (cadr ir) td))
            (save-left (str :x0 :sp :offset slot-off))
            (right-code (h0-codegen (caddr ir) (+ td 1)))
            (untag-right (lsr :x1 :x0 4 :imm t))
            (load-left (ldr :x0 :sp :offset slot-off))
            (untag-left (lsr :x0 :x0 4 :imm t))
            (divide (sdiv :x0 :x0 :x1))
            (retag (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all
        (list left-code save-left right-code untag-right load-left
              untag-left divide retag))))           ; retag result

    ;; Modulo (a mod b = a - (a/b)*b)
    ((h0-has-tag-n ir (ir-tag-mod))
     (let* ((slot-off (+ 48 (* td 8)))
            (slot-off2 (+ 48 (* (+ td 1) 8)))
            (left-code (h0-codegen (cadr ir) td))
            (save-left (str :x0 :sp :offset slot-off))
            (right-code (h0-codegen (caddr ir) (+ td 1)))
            (save-right (str :x0 :sp :offset slot-off2)))
       (let* ((untag-right (lsr :x1 :x0 4 :imm t))
              (load-left (ldr :x0 :sp :offset slot-off))
              (untag-left (lsr :x0 :x0 4 :imm t))
              (divide (sdiv :x2 :x0 :x1))
              (msub-code (msub :x0 :x2 :x1 :x0))
              (retag (lsl :x0 :x0 4 :imm t)))
         (bytes-append-all
          (list left-code save-left right-code save-right untag-right
                load-left untag-left divide msub-code retag)))))                ; retag

    ;; Comparisons - condition codes: eq=0, lt=11, gt=12, le=13, ge=10
    ((h0-has-tag-n ir (ir-tag-cmp-eq))
     (h0-codegen-cmp (cadr ir) (caddr ir) 0 td))
    ((h0-has-tag-n ir (ir-tag-cmp-lt))
     (h0-codegen-cmp (cadr ir) (caddr ir) 11 td))
    ((h0-has-tag-n ir (ir-tag-cmp-gt))
     (h0-codegen-cmp (cadr ir) (caddr ir) 12 td))
    ((h0-has-tag-n ir (ir-tag-cmp-le))
     (h0-codegen-cmp (cadr ir) (caddr ir) 13 td))
    ((h0-has-tag-n ir (ir-tag-cmp-ge))
     (h0-codegen-cmp (cadr ir) (caddr ir) 10 td))

    ;; If
    ((h0-has-tag-n ir (ir-tag-if))
     (let* ((test-ir (cadr ir))
            (then-ir (caddr ir))
            (else-ir (cadddr ir))
            (test-code (h0-codegen test-ir td))
            (then-code (h0-codegen then-ir td))
            (else-code (h0-codegen else-ir td))
            (then-len (length then-code))
            (else-len (length else-code)))
       (bytes-append-all
        (list test-code
              (cmp :x0 0 :imm t)                  ; test == 0?
              (b.eq (ash (+ then-len 8) -2))      ; skip then + jump (instruction offset)
              then-code
              (b (ash (+ else-len 4) -2))         ; skip else (instruction offset)
              else-code))))

    ;; Cons
    ((h0-has-tag-n ir (ir-tag-cons))
     (let* ((slot-off (+ 48 (* td 8)))
            (car-code (h0-codegen (cadr ir) td))
            (save-car (str :x0 :sp :offset slot-off))
            (cdr-code (h0-codegen (caddr ir) (+ td 1)))
            (move-cdr (mov :x1 :x0)))
       (let* ((load-car (ldr :x0 :sp :offset slot-off))
              (store-car (str :x0 :x28 :offset 0))
              (store-cdr (str :x1 :x28 :offset 8))
              (get-ptr (mov :x0 :x28))
              (tag-cons (add :x0 :x0 1 :imm t))
              (bump-heap (add :x28 :x28 16 :imm t)))
         (bytes-append-all
          (list car-code save-car cdr-code move-cdr load-car
                store-car store-cdr get-ptr tag-cons bump-heap)))))        ; bump heap

    ;; Car
    ((h0-has-tag-n ir (ir-tag-car))
     (let* ((arg-ir (cadr ir))
            (arg-code (h0-codegen arg-ir td))
            (untag (sub :x0 :x0 1 :imm t))
            (load-car (ldr :x0 :x0 :offset 0)))
       (bytes-append-all
        (list arg-code untag load-car))))

    ;; Cdr
    ((h0-has-tag-n ir (ir-tag-cdr))
     (let* ((arg-ir (cadr ir))
            (arg-code (h0-codegen arg-ir td))
            (untag (sub :x0 :x0 1 :imm t))
            (load-cdr (ldr :x0 :x0 :offset 8)))
       (bytes-append-all
        (list arg-code untag load-cdr))))          ; load cdr

    ;; Null check - compare to nil (0x6), not zero
    ((h0-has-tag-n ir (ir-tag-null))
     (let* ((arg-ir (cadr ir))
            (arg-code (h0-codegen arg-ir td))
            (cmp-nil (cmp :x0 6 :imm t))
            (set-cond (cset :x0 0))
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all
        (list arg-code cmp-nil set-cond tag-result))))

    ;; Let binding
    ;; h0-compile assigns offset 0 to the innermost binding
    ;; Offset 0 -> [x20-0], offset 1 -> [x20-8], etc.
    ;; Nested lets: inner var gets offset 0, outer var gets offset 1
    ;; Store value at x20-0 (the slot for offset 0), decrement x20 for body
    ((h0-has-tag-n ir (ir-tag-let))
     (let* ((val-ir (caddr ir))
            (body-ir (cadddr ir))
            (val-code (h0-codegen val-ir td))
            (body-code (h0-codegen body-ir td)))
       (bytes-append-all
        (list val-code
              ;; Decrement x20 BEFORE storing (so offset 0 refers to new slot)
              (sub :x20 :x20 8 :imm t)            ; x20 -= 8 (grow frame)
              (str :x0 :x20 :offset 0)            ; [x20] = value (at new x20)
              body-code
              (add :x20 :x20 8 :imm t)))))        ; x20 += 8 (restore frame)

    ;; Setq - variable assignment
    ;; IR: (setq-ir offset value-ir)
    ;; Evaluate value, then store to variable's stack slot
    ((h0-has-tag-n ir (ir-tag-setq))
     (let* ((offset (cadr ir))
            (val-ir (caddr ir))
            (byte-off (* offset 8))
            (val-code (h0-codegen val-ir td))
            ;; Calculate address: x1 = x20 - byte_offset
            (sub-code (sub :x1 :x20 byte-off :imm t))
            ;; Store x0 to [x1]
            (str-code (str :x0 :x1 :offset 0)))
       (bytes-append-all (list val-code sub-code str-code))))

    ;; Progn
    ((h0-has-tag-n ir (ir-tag-progn))
     (h0-codegen-progn (cadr ir) td))

    ;; String-length: get length from string header (offset -8 from tagged ptr)
    ;; String layout: [length:u64][chars...]  with tag 4
    ((h0-has-tag-n ir (ir-tag-str-len))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            ;; Untag (subtract 4), then load length from offset 0
            (untag (sub :x0 :x0 4 :imm t))
            (load-len (ldr :x0 :x0 :offset 0))
            ;; Length is already untagged, need to tag it
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list arg-code untag load-len tag-result))))

    ;; String-ref: get char at index
    ;; String layout: [length:u64][chars...]
    ((h0-has-tag-n ir (ir-tag-str-ref))
     (let* ((slot-off (+ 48 (* td 8)))
            (str-code (h0-codegen (cadr ir) td))
            (save-str (str :x0 :sp :offset slot-off))
            (idx-code (h0-codegen (caddr ir) (+ td 1)))
            ;; Untag index
            (untag-idx (lsr :x1 :x0 4 :imm t))
            ;; Load string ptr
            (load-str (ldr :x0 :sp :offset slot-off))
            ;; Untag string (subtract 4)
            (untag-str (sub :x0 :x0 4 :imm t))
            ;; Add 8 to skip length field, then add index
            (add-offset (add :x0 :x0 8 :imm t))
            (add-idx (add :x0 :x0 :x1))
            ;; Load byte
            (load-byte (ldrb :x0 :x0 0))
            ;; Tag result
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list str-code save-str idx-code untag-idx load-str
                               untag-str add-offset add-idx load-byte tag-result))))

    ;; EQ: pointer equality (cond-eq = 0)
    ((h0-has-tag-n ir (ir-tag-eq))
     (h0-codegen-cmp (cadr ir) (caddr ir) 0 td))

    ;; Consp: check if tag is 1
    ((h0-has-tag-n ir (ir-tag-consp))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            (and-tag (and* :x0 :x0 #xF :imm t))
            (cmp-tag (cmp :x0 1 :imm t))
            (cset-code (cset :x0 0))
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list arg-code and-tag cmp-tag cset-code tag-result))))

    ;; Symbolp: check if tag is 2
    ((h0-has-tag-n ir (ir-tag-symbolp))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            (and-tag (and* :x0 :x0 #xF :imm t))
            (cmp-tag (cmp :x0 2 :imm t))
            (cset-code (cset :x0 0))
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list arg-code and-tag cmp-tag cset-code tag-result))))

    ;; Numberp: check if tag is 0 (fixnum)
    ((h0-has-tag-n ir (ir-tag-numberp))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            (and-tag (and* :x0 :x0 #xF :imm t))
            (cmp-tag (cmp :x0 0 :imm t))
            (cset-code (cset :x0 0))
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list arg-code and-tag cmp-tag cset-code tag-result))))

    ;; Stringp: check if tag is 4
    ((h0-has-tag-n ir (ir-tag-stringp))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            (and-tag (and* :x0 :x0 #xF :imm t))
            (cmp-tag (cmp :x0 4 :imm t))
            (cset-code (cset :x0 0))
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list arg-code and-tag cmp-tag cset-code tag-result))))

    ;; Keywordp: check if tag is 7
    ((h0-has-tag-n ir (ir-tag-keywordp))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            (and-tag (and* :x0 :x0 #xF :imm t))
            (cmp-tag (cmp :x0 7 :imm t))
            (cset-code (cset :x0 0))
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list arg-code and-tag cmp-tag cset-code tag-result))))

    ;; Symbol-name: extract name string from symbol
    ;; Symbol structure: tagged with |2, points to [name-string | value | plist | package]
    ;; name-string is at offset 0 from untagged pointer
    ((h0-has-tag-n ir (ir-tag-symbol-name))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            ;; Remove tag: and x0, x0, #~7 (clear lower 3 bits)
            (untag (and* :x0 :x0 #xFFFFFFFFFFFFFFF8 :imm t))
            ;; Load name string: ldr x0, [x0, #0]
            (load-name (ldr :x0 :x0 :offset 0)))
       (bytes-append-all (list arg-code untag load-name))))

    ;; Logand: bitwise AND - must untag, operate, retag
    ((h0-has-tag-n ir (ir-tag-logand))
     (let* ((slot-off (+ 48 (* td 8)))
            (left-code (h0-codegen (cadr ir) td))
            (save-left (str :x0 :sp :offset slot-off))
            (right-code (h0-codegen (caddr ir) (+ td 1)))
            (move-right (mov :x1 :x0))
            (load-left (ldr :x0 :sp :offset slot-off))
            (untag-x0 (asr :x0 :x0 4 :imm t))
            (untag-x1 (asr :x1 :x1 4 :imm t))
            (do-and (and* :x0 :x0 :x1))
            (retag (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all
        (list left-code save-left right-code move-right load-left
              untag-x0 untag-x1 do-and retag))))

    ;; Logior: bitwise OR - must untag, operate, retag
    ((h0-has-tag-n ir (ir-tag-logior))
     (let* ((slot-off (+ 48 (* td 8)))
            (left-code (h0-codegen (cadr ir) td))
            (save-left (str :x0 :sp :offset slot-off))
            (right-code (h0-codegen (caddr ir) (+ td 1)))
            (move-right (mov :x1 :x0))
            (load-left (ldr :x0 :sp :offset slot-off))
            (untag-x0 (asr :x0 :x0 4 :imm t))
            (untag-x1 (asr :x1 :x1 4 :imm t))
            (do-or (orr :x0 :x0 :x1))
            (retag (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all
        (list left-code save-left right-code move-right load-left
              untag-x0 untag-x1 do-or retag))))

    ;; Lognot: bitwise NOT (untag, MVN, retag)
    ((h0-has-tag-n ir (ir-tag-lognot))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            ;; Untag: ASR x0, x0, #4
            (untag (asr :x0 :x0 4 :imm t))
            ;; MVN x0, x0 - bitwise complement
            (invert (mvn :x0 :x0))
            ;; Retag: LSL x0, x0, #4
            (retag (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list arg-code untag invert retag))))

    ;; ASH: arithmetic shift (untag, shift, retag)
    ;; Positive shift = left, negative = right
    ((h0-has-tag-n ir (ir-tag-ash))
     (let* ((slot-off (+ 48 (* td 8)))
            (val-code (h0-codegen (cadr ir) td))
            (save-val (str :x0 :sp :offset slot-off))
            (shift-code (h0-codegen (caddr ir) (+ td 1)))
            ;; Untag shift amount
            (untag-shift (asr :x1 :x0 4 :imm t))
            ;; Load value
            (load-val (ldr :x0 :sp :offset slot-off))
            ;; Untag value
            (untag-val (asr :x0 :x0 4 :imm t))
            ;; Variable shift: if x1 >= 0, LSL; else ASR by -x1
            ;; For simplicity, use LSL for now (assume positive shifts)
            (shift-op (lsl :x0 :x0 :x1))
            ;; Retag
            (retag (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list val-code save-val shift-code untag-shift
                               load-val untag-val shift-op retag))))

    ;; NOT: boolean negation (nil -> t, anything else -> nil)
    ((h0-has-tag-n ir (ir-tag-not))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            ;; Compare to nil (6)
            (cmp-nil (cmp :x0 6 :imm t))
            ;; If equal to nil, result is 1 (t), else 0
            (cset-code (cset :x0 0))
            ;; Tag result
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list arg-code cmp-nil cset-code tag-result))))

    ;; LENGTH: count cons cells in list
    ;; Loop: x0 = list ptr, x1 = counter
    ;; While x0 != nil: x1++, x0 = cdr(x0)
    ((h0-has-tag-n ir (ir-tag-length))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            ;; Initialize counter to 0
            (init-counter (movz :x1 0))
            ;; Loop start label offset (will be patched)
            ;; Check if x0 == nil (6)
            (loop-start-offset 0)
            (cmp-nil (cmp :x0 6 :imm t))
            ;; Branch to end if equal (skip to move-result)
            (branch-end (b.eq 5))  ; skip 5 instructions
            ;; Increment counter (tagged add: add 16 for +1)
            (inc-counter (add :x1 :x1 16 :imm t))
            ;; Get CDR: untag, load offset 8, keep tagged
            (untag-cons (sub :x0 :x0 1 :imm t))
            (load-cdr (ldr :x0 :x0 :offset 8))
            ;; Branch back to loop start (back to cmp-nil)
            (branch-loop (b -5))  ; back 5 instructions
            ;; Move counter to result
            (move-result (mov :x0 :x1)))
       (bytes-append-all (list arg-code init-counter cmp-nil branch-end
                               inc-counter untag-cons load-cdr branch-loop
                               move-result))))

    ;; MAKE-VECTOR: Allocate vector on heap
    ;; Vector layout: [length:u64][elem0][elem1]...
    ;; Tagged with tag 3 (vector)
    ;; Input: size (tagged fixnum)
    ;; Output: tagged vector pointer
    ((h0-has-tag-n ir (ir-tag-make-vector))
     (let* ((size-code (h0-codegen (cadr ir) td))
            ;; Unshift size from tagged fixnum: lsr x1, x0, #4
            (unshift-size (lsr :x1 :x0 4 :imm t))
            ;; Store length at heap: str x1, [x28]
            (store-len (str :x1 :x28 :offset 0))
            ;; Calculate total bytes: (size + 1) * 8
            ;; x2 = x1 + 1
            (add-one (add :x2 :x1 1 :imm t))
            ;; x2 = x2 * 8 = x2 << 3
            (mul-eight (lsl :x2 :x2 3 :imm t))
            ;; Tag result: x0 = x28 + 3
            (mov-ptr (mov :x0 :x28))
            (tag-vec (add :x0 :x0 3 :imm t))
            ;; Bump heap pointer: x28 += x2
            (bump-heap (add :x28 :x28 :x2)))
       (bytes-append-all (list size-code unshift-size store-len
                               add-one mul-eight mov-ptr tag-vec bump-heap))))

    ;; VECTOR-REF: Get element from vector
    ;; Vector layout: [length:u64][elem0][elem1]...
    ;; Input: vec (tagged), idx (tagged fixnum)
    ;; Output: element value
    ((h0-has-tag-n ir (ir-tag-vector-ref))
     (let* ((slot-off (+ 48 (* td 8)))
            (vec-code (h0-codegen (cadr ir) td))
            (save-vec (str :x0 :sp :offset slot-off))
            (idx-code (h0-codegen (caddr ir) (+ td 1)))
            ;; Load vec to x2
            (load-vec (ldr :x2 :sp :offset slot-off))
            ;; Untag vec: sub x2, x2, #3
            (untag-vec (sub :x2 :x2 3 :imm t))
            ;; Unshift idx: lsr x1, x0, #4
            (unshift-idx (lsr :x1 :x0 4 :imm t))
            ;; Add 1 to skip length slot: add x1, x1, #1
            (add-one (add :x1 :x1 1 :imm t))
            ;; Calculate byte offset: lsl x1, x1, #3 (multiply by 8)
            (calc-offset (lsl :x1 :x1 3 :imm t))
            ;; Add offset to base: add x2, x2, x1
            (add-offset (add :x2 :x2 :x1))
            ;; Load element: ldr x0, [x2]
            (load-elem (ldr :x0 :x2 :offset 0)))
       (bytes-append-all (list vec-code save-vec idx-code load-vec
                               untag-vec unshift-idx add-one calc-offset
                               add-offset load-elem))))

    ;; VECTOR-SET: Set element in vector
    ;; Input: vec (tagged), idx (tagged fixnum), val
    ;; Output: val (return the value that was set)
    ((h0-has-tag-n ir (ir-tag-vector-set))
     (let* ((slot-vec (+ 48 (* td 8)))
            (slot-idx (+ 48 (* (+ td 1) 8)))
            (vec-code (h0-codegen (cadr ir) td))
            (save-vec (str :x0 :sp :offset slot-vec))
            (idx-code (h0-codegen (caddr ir) (+ td 1)))
            (save-idx (str :x0 :sp :offset slot-idx))
            (val-code (h0-codegen (cadddr ir) (+ td 2)))
            ;; Save val in x3
            (save-val (mov :x3 :x0))
            ;; Load vec and idx
            (load-vec (ldr :x2 :sp :offset slot-vec))
            (load-idx (ldr :x1 :sp :offset slot-idx))
            ;; Untag vec: sub x2, x2, #3
            (untag-vec (sub :x2 :x2 3 :imm t))
            ;; Unshift idx: lsr x1, x1, #4
            (unshift-idx (lsr :x1 :x1 4 :imm t))
            ;; Add 1 to skip length slot
            (add-one (add :x1 :x1 1 :imm t))
            ;; Calculate byte offset: lsl x1, x1, #3
            (calc-offset (lsl :x1 :x1 3 :imm t))
            ;; Add offset to base: add x2, x2, x1
            (add-offset (add :x2 :x2 :x1))
            ;; Store: str x3, [x2]
            (store-elem (str :x3 :x2 :offset 0))
            ;; Return val: mov x0, x3
            (ret-val (mov :x0 :x3)))
       (bytes-append-all (list vec-code save-vec idx-code save-idx val-code
                               save-val load-vec load-idx untag-vec unshift-idx
                               add-one calc-offset add-offset store-elem ret-val))))

    ;; VECTOR-LENGTH: Get length of vector
    ;; Input: vec (tagged)
    ;; Output: length (tagged fixnum)
    ((h0-has-tag-n ir (ir-tag-vector-length))
     (let* ((vec-code (h0-codegen (cadr ir) td))
            ;; Untag vec: sub x0, x0, #3
            (untag-vec (sub :x0 :x0 3 :imm t))
            ;; Load length: ldr x0, [x0]
            (load-len (ldr :x0 :x0 :offset 0))
            ;; Shift to tag: lsl x0, x0, #4
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list vec-code untag-vec load-len tag-result))))

    ;; LAMBDA: Create closure (simplified - no lambda lifting yet)
    ;; Layout: [fn-offset:8][env-ptr:8] = 16 bytes
    ;; Returns tagged pointer with tag 5
    ((h0-has-tag-n ir (ir-tag-lambda))
     (let* ((params (cadr ir))
            (body-ir (caddr ir))
            (free-vars (cadddr ir))
            (free-offsets (nth 4 ir)))
       ;; Simplified closure: placeholder fn-offset (0) and nil env (0x6)
       (let* (;; Store placeholder function offset (0) at heap+0
              (mov-fn-offset (movz :x0 0))               ; x0 = 0
              (str-fn-offset (str :x0 :x28 :offset 0))   ; [x28+0] = x0
              ;; Store nil environment (0x6) at heap+8
              (mov-env (movz :x0 6))                     ; x0 = 6 (nil)
              (str-env (str :x0 :x28 :offset 8))         ; [x28+8] = x0
              ;; Create tagged closure pointer: x28 | 5
              (mov-ptr (mov :x0 :x28))                   ; x0 = x28
              (tag-closure (add :x0 :x0 5 :imm t))       ; x0 = x0 | 5
              ;; Bump heap pointer by 16 bytes
              (bump-heap (add :x28 :x28 16 :imm t)))     ; x28 += 16
         (bytes-append-all (list mov-fn-offset str-fn-offset
                                 mov-env str-env
                                 mov-ptr tag-closure bump-heap)))))

    ;; FUNCALL: Call closure (supports up to 8 args via x0-x7)
    ((h0-has-tag-n ir (ir-tag-funcall))
     (let* ((fn-ir (cadr ir))
            (args-ir (caddr ir))
            (num-args (h0-list-length args-ir)))
       ;; Support 0-8 arguments (ARM64 calling convention)
       (if (> num-args 8)
           (fatal-error "h0-codegen: FUNCALL supports max 8 args")
           (h0-codegen-funcall fn-ir args-ir num-args td))))

    ;; STRING=: Compare two strings byte-by-byte
    ;; Both strings must have same length and same bytes
    ;; Returns 1 (true) or 0 (false) as tagged fixnum
    ((h0-has-tag-n ir (ir-tag-string-eq))
     (let* ((slot-off (+ 48 (* td 8)))
            (slot-off2 (+ 48 (* (+ td 1) 8)))
            ;; Compile both string arguments
            (str1-code (h0-codegen (cadr ir) td))
            (save-str1 (str :x0 :sp :offset slot-off))
            (str2-code (h0-codegen (caddr ir) (+ td 1)))
            (save-str2 (str :x0 :sp :offset slot-off2))
            ;; Load str1 and untag (subtract 4 for string tag)
            (load-str1 (ldr :x0 :sp :offset slot-off))
            (untag-str1 (sub :x0 :x0 4 :imm t))      ; x0 = str1 untagged
            ;; Load str2 and untag
            (load-str2 (ldr :x1 :sp :offset slot-off2))
            (untag-str2 (sub :x1 :x1 4 :imm t))      ; x1 = str2 untagged
            ;; Compare lengths first: [x0+0] vs [x1+0]
            (ldr-len1 (ldr :x2 :x0 :offset 0))       ; x2 = len1
            (ldr-len2 (ldr :x3 :x1 :offset 0))       ; x3 = len2
            (cmp-lens (cmp :x2 :x3))                 ; compare lengths
            ;; Branch to fail if lengths differ (11 instructions forward)
            (branch-ne-lens (b.ne 11))
            ;; Setup loop: x4 = 0 (index), x2 = len (already loaded)
            (mov-idx (movz :x4 0))                   ; x4 = 0
            ;; Loop: while x4 < x2
            (cmp-idx-len (cmp :x4 :x2))              ; compare idx < len
            ;; Skip loop body (5) + success (2) = 7 instructions
            (branch-done (b.ge 7))
            ;; Calculate offset: x5 = x4 + 8 (skip length header)
            (add-off (add :x5 :x4 8 :imm t))         ; x5 = idx + 8
            ;; Load bytes from both strings at offset
            (ldrb-byte1 (ldrb :x6 :x0 :x5 :reg t))   ; x6 = str1[idx+8]
            (ldrb-byte2 (ldrb :x7 :x1 :x5 :reg t))   ; x7 = str2[idx+8]
            (cmp-bytes (cmp :x6 :x7))                ; compare bytes
            ;; Skip inc+loop (2) + success (2) = 4 instructions
            (branch-ne-byte (b.ne 4))
            ;; Increment and loop back
            (inc-idx (add :x4 :x4 1 :imm t))         ; x4++
            ;; Back to cmp-idx-len: -6 instructions
            (branch-loop (b -6))
            ;; Success: return tagged 1 (16)
            (mov-one (movz :x0 16))                  ; x0 = 16 (tagged 1)
            ;; Skip fail (1) + nop (1) = 2 instructions
            (branch-end (b 2))
            ;; Fail: return tagged 0
            (mov-zero (movz :x0 0))                  ; x0 = 0 (tagged 0)
            ;; NOP to align (common exit point)
            (nop-exit (nop)))
       (bytes-append-all (list str1-code save-str1 str2-code save-str2
                               load-str1 untag-str1 load-str2 untag-str2
                               ldr-len1 ldr-len2 cmp-lens branch-ne-lens
                               mov-idx cmp-idx-len branch-done
                               add-off ldrb-byte1 ldrb-byte2 cmp-bytes branch-ne-byte
                               inc-idx branch-loop
                               mov-one branch-end
                               mov-zero nop-exit))))

    ;; QUOTE-SYM: Allocate symbol on heap
    ;; Symbol layout: [name-string | value | plist | package] = 32 bytes
    ;; Returns tagged pointer with tag 2
    ((h0-has-tag-n ir (ir-tag-quote-sym))
     (let* ((sym (cadr ir))
            (name (symbol-name sym))
            (name-len (string-length name))
            ;; First allocate the name string on heap
            (name-total-size (logand (+ name-len 8 15) (lognot 15)))
            (name-code (h0-codegen-str-lit name name-len name-total-size))
            ;; Save name string pointer
            (save-name (mov :x1 :x0))
            ;; Now allocate symbol: 32 bytes for [name | value | plist | package]
            ;; Store name string at heap[0]
            (str-name (str :x1 :x28 :offset 0))
            ;; Store nil (0x6) for value at heap[8]
            (mov-nil (movz :x0 6))
            (str-value (str :x0 :x28 :offset 8))
            ;; Store nil for plist at heap[16]
            (str-plist (str :x0 :x28 :offset 16))
            ;; Store nil for package at heap[24]
            (str-package (str :x0 :x28 :offset 24))
            ;; Get symbol pointer and tag with 2
            (mov-ptr (mov :x0 :x28))
            (tag-sym (add :x0 :x0 2 :imm t))
            ;; Bump heap by 32 bytes
            (bump-heap (add :x28 :x28 32 :imm t)))
       (bytes-append-all (list name-code save-name str-name mov-nil
                               str-value str-plist str-package
                               mov-ptr tag-sym bump-heap))))

    ;; EQL - compare for equality (works for numbers and symbols)
    ;; For tagged values: compare directly, result is 1 (t) or 0 (nil)
    ((h0-has-tag-n ir (ir-tag-eql))
     (h0-codegen-cmp (cadr ir) (caddr ir) 0 td))

    ;; GET-TAG - extract tag bits from tagged value
    ;; Result is tag (0-15) as tagged fixnum
    ((h0-has-tag-n ir (ir-tag-get-tag))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            ;; Extract tag: and x0, x0, #0xF
            (extract-tag (and* :x0 :x0 #xF :imm t))
            ;; Tag result as fixnum
            (tag-result (lsl :x0 :x0 4 :imm t)))
       (bytes-append-all (list arg-code extract-tag tag-result))))

    ;; SET-TAG - change tag bits on a pointer value
    ;; (set-tag value new-tag) where new-tag is a tagged fixnum 0-15
    ;; Result is value with its low 4 bits replaced by (untag new-tag)
    ((h0-has-tag-n ir (ir-tag-set-tag))
     (let* ((slot-off (+ 48 (* td 8)))               ; temp slot for value
            (val-code (h0-codegen (cadr ir) td))     ; compile value -> x0
            (save-val (str :x0 :sp :offset slot-off)) ; save value to temp slot
            (tag-code (h0-codegen (caddr ir) (+ td 1))) ; new-tag -> x0 (use next temp)
            (untag-new (asr :x0 :x0 4 :imm t))       ; untag new-tag: x0 = tag>>4
            (load-val (ldr :x1 :sp :offset slot-off)) ; restore value to x1
            ;; Clear low 4 bits of value: load 15 to x2, then BIC x1, x1, x2
            (load-mask (movz :x2 15 :lsl 0))         ; x2 = 15 (0xF)
            (clear-old-tag (bic :x1 :x1 :x2))        ; x1 = x1 & ~x2 (clear low 4 bits)
            (apply-new-tag (orr :x0 :x1 :x0)))       ; x0 = cleared_value | new_tag
       (bytes-append-all (list val-code save-val tag-code untag-new
                               load-val load-mask clear-old-tag apply-new-tag))))

    ;; MAKE-STRING-FROM-VECTOR - create string from vector of character codes
    ;; This is complex and needs a loop - simplified version
    ((h0-has-tag-n ir (ir-tag-make-string-from-vector))
     ;; For now, just call the runtime primitive via placeholder
     ;; In a full implementation, this would generate loop code
     (fatal-error "h0-codegen: MAKE-STRING-FROM-VECTOR not yet implemented"))

    ;; MAKE-SYMBOL-FROM-STRING - create symbol from string
    ((h0-has-tag-n ir (ir-tag-make-symbol-from-string))
     (let* ((name-code (h0-codegen (cadr ir) td))
            ;; Save name string at heap
            (str-name (str :x0 :x28 :offset 0))
            ;; Store nil (0x6) for value, plist, package
            (mov-nil (movz :x0 6))
            (str-value (str :x0 :x28 :offset 8))
            (str-plist (str :x0 :x28 :offset 16))
            (str-package (str :x0 :x28 :offset 24))
            ;; Get symbol pointer and tag with 2
            (mov-ptr (mov :x0 :x28))
            (tag-sym (add :x0 :x0 2 :imm t))
            ;; Bump heap by 32 bytes
            (bump-heap (add :x28 :x28 32 :imm t)))
       (bytes-append-all (list name-code str-name mov-nil
                               str-value str-plist str-package
                               mov-ptr tag-sym bump-heap))))

    ;; ERROR - crash the program
    ((h0-has-tag-n ir (ir-tag-error))
     ;; Generate invalid instruction to crash: udf #0
     (udf 0))

    ;; Default - CRASH: unknown IR tag
    (t (fatal-error "h0-codegen: Unknown IR tag"))))

;;; ==========================================================================
;;; IR Evaluator - for testing the compiler without native execution
;;; ==========================================================================

;; Evaluate IR directly (for testing compiler output)
;; Uses numeric tags for native code compatibility
(defun h0-eval-ir (ir env)
  (cond
    ;; Literal
    ((h0-has-tag-n ir (ir-tag-lit))
     (let ((val (cadr ir)))
       ;; Convert 0 to NIL for proper list handling
       (if (= val #x0) nil val)))
    ;; Variable reference
    ((h0-has-tag-n ir (ir-tag-var))
     (let ((off (cadr ir)))
       (ir-env-get env off)))
    ;; Arithmetic
    ((h0-has-tag-n ir (ir-tag-add))
     (+ (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-sub))
     (- (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-mul))
     (* (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-div))
     (/ (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-mod))
     (mod (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ;; Comparisons
    ((h0-has-tag-n ir (ir-tag-cmp-eq))
     (if (= (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-cmp-lt))
     (if (< (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-cmp-gt))
     (if (> (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-cmp-le))
     (if (<= (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-cmp-ge))
     (if (>= (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ;; Conditional
    ((h0-has-tag-n ir (ir-tag-if))
     (if (= (h0-eval-ir (cadr ir) env) #x0)
         (h0-eval-ir (cadddr ir) env)
         (h0-eval-ir (caddr ir) env)))
    ;; Cons/car/cdr
    ((h0-has-tag-n ir (ir-tag-cons))
     (cons (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-car))
     (car (h0-eval-ir (cadr ir) env)))
    ((h0-has-tag-n ir (ir-tag-cdr))
     (cdr (h0-eval-ir (cadr ir) env)))
    ((h0-has-tag-n ir (ir-tag-null))
     (if (null (h0-eval-ir (cadr ir) env)) #x1 #x0))
    ;; Let binding
    ((h0-has-tag-n ir (ir-tag-let))
     (let* ((val (h0-eval-ir (caddr ir) env))
            (new-env (cons val env)))
       (h0-eval-ir (cadddr ir) new-env)))
    ;; Setq - variable assignment (stub: mutation requires rplaca primitive)
    ((h0-has-tag-n ir (ir-tag-setq))
     (let* ((val (h0-eval-ir (caddr ir) env)))
       val))
    ;; Progn
    ((h0-has-tag-n ir (ir-tag-progn))
     (h0-eval-ir-progn (cadr ir) env))
    ;; String operations
    ((h0-has-tag-n ir (ir-tag-str-lit))
     (cadr ir))
    ((h0-has-tag-n ir (ir-tag-str-len))
     (string-length (h0-eval-ir (cadr ir) env)))
    ((h0-has-tag-n ir (ir-tag-str-ref))
     (let* ((str (h0-eval-ir (cadr ir) env))
            (idx (h0-eval-ir (caddr ir) env)))
       (string-ref str idx)))
    ;; String equality
    ((h0-has-tag-n ir (ir-tag-string-eq))
     (let* ((str1 (h0-eval-ir (cadr ir) env))
            (str2 (h0-eval-ir (caddr ir) env)))
       (if (string= str1 str2) #x1 #x0)))
    ;; Keyword operations
    ((h0-has-tag-n ir (ir-tag-kw-lit))
     (cadr ir))
    ;; Eq comparison
    ((h0-has-tag-n ir (ir-tag-eq))
     (if (eq (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ;; Type predicates
    ((h0-has-tag-n ir (ir-tag-consp))
     (if (consp (h0-eval-ir (cadr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-symbolp))
     (if (symbolp (h0-eval-ir (cadr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-numberp))
     (if (numberp (h0-eval-ir (cadr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-stringp))
     (if (stringp (h0-eval-ir (cadr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-keywordp))
     (if (keywordp (h0-eval-ir (cadr ir) env)) #x1 #x0))
    ;; Symbol-name extraction
    ((h0-has-tag-n ir (ir-tag-symbol-name))
     (symbol-name (h0-eval-ir (cadr ir) env)))
    ;; Bitwise operations
    ((h0-has-tag-n ir (ir-tag-logand))
     (logand (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-logior))
     (logior (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-ash))
     (ash (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-lognot))
     (lognot (h0-eval-ir (cadr ir) env)))
    ;; Boolean not
    ((h0-has-tag-n ir (ir-tag-not))
     (let ((val (h0-eval-ir (cadr ir) env)))
       (if (or (null val) (= val #x0)) #x1 #x0)))
    ;; List length
    ((h0-has-tag-n ir (ir-tag-length))
     (h0-list-length (h0-eval-ir (cadr ir) env)))
    ;; Vector operations
    ((h0-has-tag-n ir (ir-tag-make-vector))
     (let ((size (h0-eval-ir (cadr ir) env)))
       (make-vector size)))
    ((h0-has-tag-n ir (ir-tag-vector-ref))
     (let* ((vec (h0-eval-ir (cadr ir) env))
            (idx (h0-eval-ir (caddr ir) env)))
       (vector-ref vec idx)))
    ((h0-has-tag-n ir (ir-tag-vector-set))
     (let* ((vec (h0-eval-ir (cadr ir) env))
            (idx (h0-eval-ir (caddr ir) env))
            (val (h0-eval-ir (cadddr ir) env)))
       (vector-set vec idx val)
       val))
    ((h0-has-tag-n ir (ir-tag-vector-length))
     (let ((vec (h0-eval-ir (cadr ir) env)))
       (vector-length vec)))
    ;; Quoted symbol - return the symbol itself
    ((h0-has-tag-n ir (ir-tag-quote-sym))
     (cadr ir))
    ;; Lambda - create closure
    ((h0-has-tag-n ir (ir-tag-lambda))
     (let* ((params (cadr ir))
            (body-ir (caddr ir))
            (free-vars (cadddr ir))
            (free-offsets (nth #x4 ir))
            ;; Capture only the free variables using their offsets
            (captured-vals (h0-capture-free-vars free-offsets env)))
       ;; Create closure: (closure params body-ir captured-vals)
       ;; captured-vals contains the values of free variables at closure creation
       (list (intern "CLOSURE") params body-ir captured-vals)))
    ;; Funcall - call closure
    ((h0-has-tag-n ir (ir-tag-funcall))
     (let* ((fn-val (h0-eval-ir (cadr ir) env))
            (args-ir (caddr ir))
            (args-vals (h0-eval-ir-args args-ir env)))
       ;; fn-val should be (closure params body-ir captured-vals)
       (if (and (consp fn-val) (eq (car fn-val) 'CLOSURE))
           (let* ((body-ir (caddr fn-val))
                  (captured-vals (cadddr fn-val))
                  ;; Build env: free vars (captured) first, then args
                  ;; This matches the IR's variable indexing scheme
                  (new-env (append captured-vals args-vals)))
             (h0-eval-ir body-ir new-env))
           (fatal-error "h0-eval-ir: FUNCALL on non-closure"))))
    ;; EQL - equal for numbers and symbols
    ((h0-has-tag-n ir (ir-tag-eql))
     (let* ((left (h0-eval-ir (cadr ir) env))
            (right (h0-eval-ir (caddr ir) env)))
       (if (eql left right) #x1 #x0)))
    ;; GET-TAG - extract tag from tagged value
    ((h0-has-tag-n ir (ir-tag-get-tag))
     (let ((val (h0-eval-ir (cadr ir) env)))
       (get-tag val)))
    ;; SET-TAG - change tag bits on a pointer value
    ((h0-has-tag-n ir (ir-tag-set-tag))
     (let ((val (h0-eval-ir (cadr ir) env))
           (new-tag (h0-eval-ir (caddr ir) env)))
       (set-tag val new-tag)))
    ;; MAKE-STRING-FROM-VECTOR - create string from vector of chars
    ((h0-has-tag-n ir (ir-tag-make-string-from-vector))
     (let ((vec (h0-eval-ir (cadr ir) env)))
       (make-string-from-vector vec)))
    ;; MAKE-SYMBOL-FROM-STRING - create symbol from string
    ((h0-has-tag-n ir (ir-tag-make-symbol-from-string))
     (let ((str (h0-eval-ir (cadr ir) env)))
       (make-symbol-from-string str)))
    ;; ERROR - signal error and crash
    ((h0-has-tag-n ir (ir-tag-error))
     (fatal-error "h0-eval-ir: ERROR called"))
    ;; Default - CRASH: unknown IR tag
    (t (fatal-error "h0-eval-ir: Unknown IR tag"))))

(defun h0-eval-ir-progn (forms env)
  (if (null forms)
      #x0
      (if (null (cdr forms))
          (h0-eval-ir (car forms) env)
          (progn
            (h0-eval-ir (car forms) env)
            (h0-eval-ir-progn (cdr forms) env)))))

(defun ir-env-get (env off)
  (if (= off #x0)
      (car env)
      (ir-env-get (cdr env) (- off #x1))))

;; Evaluate a list of argument IRs
(defun h0-eval-ir-args (args-ir env)
  (if (null args-ir)
      nil
      (cons (h0-eval-ir (car args-ir) env)
            (h0-eval-ir-args (cdr args-ir) env))))

;; Capture free variables from env using their compile-time offsets
;; Returns a list of captured values in order
(defun h0-capture-free-vars (offsets env)
  (if (null offsets)
      nil
      (cons (ir-env-get env (car offsets))
            (h0-capture-free-vars (cdr offsets) env))))

;; Extend environment by binding params to args
;; params is a list of parameter symbols, args-vals is a list of values
;; Returns new env with args prepended in order
(defun h0-extend-env-with-params (params args-vals base-env)
  (if (null params)
      base-env
      (cons (car args-vals)
            (h0-extend-env-with-params (cdr params) (cdr args-vals) base-env))))

;;; ==========================================================================
;;; Test Mode - compile expression and evaluate IR
;;; ==========================================================================

(defun h0-compile-and-eval (expr)
  "Compile expression to IR and evaluate it"
  (let ((ir (h0-compile expr nil nil)))
    (h0-eval-ir ir nil)))

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
  (let* ((i1 (sub :sp :sp 512 :imm t))             ; sub sp, sp, #512
         (i2 (str :x30 :sp :offset 0))             ; str x30, [sp]
         (i3 (str :x28 :sp :offset 8))             ; str x28, [sp, #8]
         (i4 (str :x26 :sp :offset 16))            ; str x26, [sp, #16]
         (i5 (str :x27 :sp :offset 24)))           ; str x27, [sp, #24]
    (let* ((i6 (str :x20 :sp :offset 32))          ; str x20, [sp, #32]
           (i7 (add :x20 :sp 64 :imm t))           ; add x20, sp, #64
           (i8 (macho-adrp 28 heap-page-offset))   ; adrp x28, heap_page
           (i9 (mov :x27 :x28))                    ; mov x27, x28
           (i10 (add :x28 :x28 16 :imm t)))        ; add x28, x28, #16
      (let* ((i11 (macho-adr 26 40))               ; adr x26, +40
             (i12 (macho-bl 9))                    ; bl +9
             (i13 (lsr :x0 :x0 4 :imm t))          ; lsr x0, x0, #4
             (i14 (ldr :x20 :sp :offset 32))       ; ldr x20, [sp, #32]
             (i15 (ldr :x27 :sp :offset 24)))      ; ldr x27, [sp, #24]
        (let* ((i16 (ldr :x26 :sp :offset 16))     ; ldr x26, [sp, #16]
               (i17 (ldr :x28 :sp :offset 8))      ; ldr x28, [sp, #8]
               (i18 (ldr :x30 :sp :offset 0))      ; ldr x30, [sp]
               (i19 (add :sp :sp 512 :imm t))      ; add sp, sp, #512
               (i20 (ret)))                        ; ret
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
                  ;; Compile test mode: compile and eval IR
                  ((if (numberp first-form) (= first-form #x100) nil)
                   (if (null (cdr forms))
                       (fatal-error "main: mode 256 requires expression")
                       (h0-compile-and-eval (cadr forms))))
                  ;; Codegen test mode: compile and return bytecode length
                  ((if (numberp first-form) (= first-form #x200) nil)
                   (if (null (cdr forms))
                       (fatal-error "main: mode 512 requires expression")
                       (let* ((ir (h0-compile (cadr forms) nil nil))
                              (code (h0-codegen ir #x0)))
                         (length code))))
                  ;; Link test mode: compile, codegen, link to /tmp/h0out
                  ((if (numberp first-form) (= first-form #x300) nil)
                   (if (null (cdr forms))
                       (fatal-error "main: mode 768 requires expression")
                       (let* ((ir (h0-compile (cadr forms) nil nil))
                              (code (h0-codegen ir #x0)))
                         (deliver-with-imports-and-heap "/tmp/h0out"
                                                        code
                                                        (list "_write")
                                                        #x100000))))
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
