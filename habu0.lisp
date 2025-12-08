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
(defvar *op-string-length* nil)
(defvar *op-string-ref* nil)
(defvar *op-string=* nil)
(defvar *op-symbol-name* nil)
(defvar *op-logand* nil)
(defvar *op-logior* nil)
(defvar *op-ash* nil)
(defvar *op-eq* nil)
(defvar *op-eql* nil)
(defvar *op-get-tag* nil)
(defvar *op-length* nil)
(defvar *op-make-vector* nil)
(defvar *op-vector-length* nil)
(defvar *op-vector-set* nil)
(defvar *op-vector-ref* nil)
(defvar *op-reverse* nil)
(defvar *op-make-string-from-vector* nil)
(defvar *op-make-symbol-from-string* nil)

;;; Package system globals
;;; Packages are ((name . symbols) ...) where symbols is ((name . sym) ...)
(defvar *packages* nil)
(defvar *current-package* nil)  ; string name of current package, nil = CL-USER

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
(defun find-interned (name table)
  (if (null table)
      nil
      (let ((entry (car table)))
        (if (string= (car entry) name)
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
  ;; Keywords use tag 7, mask off to get raw pointer
  (let ((ptr (logand kw (lognot #xF))))
    ;; Same layout as symbol: [length:8][chars:N]
    ;; symbol-name works on raw pointer + tag, so use raw ptr | 2 to trick it
    (symbol-name (logior ptr #x2))))

;; Make keyword from string - allocate like symbol but with tag 7
(defun make-keyword-from-string (name)
  ;; Use make-symbol-from-string which allocates [length:8][chars:N]
  ;; Then change tag from 2 to 7
  (let ((sym (make-symbol-from-string name)))
    (logior (logand sym (lognot #xF)) #x7)))

;; Keyword table - for now, use intern table to store keywords too
;; TODO: Add get-keyword-table-ir and set-keyword-table-ir primitives to compiler
;; for proper separate keyword table at [x27+120]
(defun get-keyword-table ()
  (get-intern-table))  ; Temporarily share intern table

(defun set-keyword-table (table)
  (set-intern-table table))  ; Temporarily share intern table

;; Search keyword table for name
(defun find-keyword (name table)
  (if (null table)
      nil
      (let ((entry (car table)))
        (if (string= (car entry) name)
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
      (if (string= (caar alist) key)
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
          ;; Create new symbol with qualified name
          (let* ((full-name (if (string= pkg-name "CL-USER")
                               sym-name
                               (string-concat3 pkg-name ":" sym-name)))
                 (sym (make-symbol-from-string full-name)))
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
        (t #x0)))

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

;; Operator checks - pure eq comparison
;; All symbols are properly interned, so eq is sufficient
;; If eq fails, that indicates a symbol interning bug - fail fast
(defun op=quote (sym) (eq sym *op-quote*))
(defun op=if (sym) (eq sym *op-if*))
(defun op=let (sym) (eq sym *op-let*))
(defun op=defun (sym) (eq sym *op-defun*))
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
(defun op=string-length (sym) (eq sym *op-string-length*))
(defun op=string-ref (sym) (eq sym *op-string-ref*))
(defun op=string= (sym) (eq sym *op-string=*))
(defun op=symbol-name (sym) (eq sym *op-symbol-name*))
(defun op=logand (sym) (eq sym *op-logand*))
(defun op=logior (sym) (eq sym *op-logior*))
(defun op=ash (sym) (eq sym *op-ash*))
(defun op=eq (sym) (eq sym *op-eq*))
(defun op=eql (sym) (eq sym *op-eql*))
(defun op=get-tag (sym) (eq sym *op-get-tag*))
(defun op=length (sym) (eq sym *op-length*))
(defun op=make-vector (sym) (eq sym *op-make-vector*))
(defun op=vector-length (sym) (eq sym *op-vector-length*))
(defun op=vector-set (sym) (eq sym *op-vector-set*))
(defun op=vector-ref (sym) (eq sym *op-vector-ref*))
(defun op=reverse (sym) (eq sym *op-reverse*))
(defun op=make-string-from-vector (sym) (eq sym *op-make-string-from-vector*))
(defun op=make-symbol-from-string (sym) (eq sym *op-make-symbol-from-string*))

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
             ;; habu0 has no features (not SBCL), so always skip form and read next
             ((= ch #x2B)
              (let* ((p2 (+ p #x2))                    ; position after #+
                     (p3 (skip-symbol source p2))      ; position after feature name
                     (skip-result (read-one p3))       ; read form to skip
                     (p4 (cdr skip-result)))           ; position after skipped form
                (read-one p4)))                        ; read and return next form
             ;; #- reader conditional - include if feature NOT present
             ;; habu0 has no features (not SBCL), so always include the form
             ((= ch #x2D)
              (let* ((p2 (+ p #x2))                    ; position after #-
                     (p3 (skip-symbol source p2)))     ; position after feature name
                (read-one p3)))                        ; read and return the form
             (t (cons nil (+ p #x2))))))
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
;; Stores (symbol . value) pairs for eq-based lookup
(defun bind-args (params args env)
  (if (null params) env
      (cons (cons (car params) (car args))
            (bind-args (cdr params) (cdr args) env))))

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

;; Find value for a keyword in argument list
;; Args: (:foo val1 :bar val2 ...), key: :foo
;; Returns (found . value) or nil if not found
(defun find-key-arg (args key-name)
  (cond
    ((null args) nil)
    ((null (cdr args)) nil)  ; keyword without value - error case
    ((keywordp (car args))
     (if (string= (keyword-name (car args)) key-name)
         (cons t (cadr args))   ; Found it
         (find-key-arg (cddr args) key-name)))
    (t (find-key-arg (cdr args) key-name))))

;; Bind keyword arguments to environment
;; key-params: list of (name default) or just name
;; key-args: list of :key val :key val ...
(defun bind-key-args (key-params key-args env fenv)
  (if (null key-params) env
      (let* ((spec (car key-params))
             (name (key-param-name spec))
             (default (key-param-default spec))
             (name-str (symbol-name name))
             (found (find-key-arg key-args name-str))
             (val (if found
                      (cdr found)
                      (if default
                          (h0-eval default nil fenv)
                          nil))))
        (bind-key-args (cdr key-params)
                       key-args
                       (cons (cons name val) env)
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

;; Look up by symbol in environment using eq
;; Returns the entry (cons sym val) or nil if not found
;; This allows distinguishing "not found" from "found with nil value"
(defun env-lookup (sym env)
  (if (null env) nil
      (let ((entry (car env)))
        (if (eq sym (car entry))
            entry  ; Return whole entry so caller can check nil values
            (env-lookup sym (cdr env))))))

;; Helper for let bindings - stores (symbol . value) pairs for eq lookup
(defun h0-eval-let (bindings body env fenv)
  (if (null bindings)
      (h0-eval body env fenv)
      (let* ((b (car bindings))
             (var (car b))  ;; Keep as symbol for eq lookup
             (val (h0-eval (cadr b) env fenv)))
        (h0-eval-let (cdr bindings) body (cons (cons var val) env) fenv))))

;; Helper for progn - evaluates forms in sequence, returns last value
(defun h0-eval-progn (forms env fenv)
  (if (null forms)
      nil
      (if (null (cdr forms))
          (h0-eval (car forms) env fenv)
          (progn
            (h0-eval (car forms) env fenv)
            (h0-eval-progn (cdr forms) env fenv)))))

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
                   (string= (symbol-name keys) "OTHERWISE")
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
    ((if (symbolp expr) (string= (symbol-name expr) "NIL") nil) nil)
    ;; t is true
    ((if (symbolp expr) (op=t expr) nil) t)
    ;; Symbol lookup in variable environment
    ((symbolp expr)
     (let ((entry (env-lookup expr env)))
       (if entry
           (cdr entry)  ; Extract value from entry
           ;; Not found - undefined symbol
           (fatal-error "h0-eval: undefined symbol"))))
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
         ((if (symbolp op) (op=let op) nil)
          (h0-eval-let (cadr expr) (caddr expr) env fenv))
         ;; Let* - same as let for sequential binding
         ((if (symbolp op) (op=let-star op) nil)
          (h0-eval-let (cadr expr) (caddr expr) env fenv))
         ;; Progn - evaluate forms in sequence
         ((if (symbolp op) (op=progn op) nil)
          (h0-eval-progn (cdr expr) env fenv))
         ;; Cond - multi-way conditional
         ((if (symbolp op) (op=cond op) nil)
          (h0-eval-cond (cdr expr) env fenv))
         ;; Defun - returns nil but defines function
         ((if (symbolp op) (op=defun op) nil) nil)
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
         ;; Error - signal error (crash with message)
         ((if (symbolp op) (op=error op) nil)
          (fatal-error "h0-eval: error called"))
         ;; Arithmetic - use cached op= functions
         ((if (symbolp op) (op=plus op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (+ left right)))
         ((if (symbolp op) (op=minus op) nil)
          (let* ((left (h0-eval (cadr expr) env fenv))
                 (right (h0-eval (caddr expr) env fenv)))
            (- left right)))
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
         ;; String primitives (use cached symbols)
         ((if (symbolp op) (op=string-length op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (string-length arg)))
         ((if (symbolp op) (op=string-ref op) nil)
          (let* ((str (h0-eval (cadr expr) env fenv))
                 (idx (h0-eval (caddr expr) env fenv)))
            (string-ref str idx)))
         ((if (symbolp op) (op=string= op) nil)
          (let* ((s1 (h0-eval (cadr expr) env fenv))
                 (s2 (h0-eval (caddr expr) env fenv)))
            (if (string= s1 s2) t nil)))
         ;; Symbol primitives (use cached symbols)
         ((if (symbolp op) (op=symbol-name op) nil)
          (let ((arg (h0-eval (cadr expr) env fenv)))
            (symbol-name arg)))
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
         ;; Function call - look up in fenv
         (t
          (let ((fn-entry (fenv-lookup op fenv)))
            (if fn-entry
                (let* ((params (car fn-entry))
                       (body (cdr fn-entry))
                       (args (h0-eval-list (cdr expr) env fenv))
                       ;; Use bind-lambda-args to support &key parameters
                       (new-env (bind-lambda-args params args nil fenv)))
                  (h0-eval body new-env fenv))
                ;; Unknown function
                (fatal-error "h0-eval: unknown function")))))))
    ;; Unknown expression type
    (t (fatal-error "h0-eval: unknown expression type"))))

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
                   (body (cadddr form)))
              (collect-defuns (cdr forms) (cons (cons name (cons params body)) fenv)))
            (collect-defuns (cdr forms) fenv)))))

;; Eval forms with collected function definitions
(defun h0-eval-forms (forms env fenv)
  (if (null forms)
      nil
      (let ((form (car forms)))
        ;; Skip defun forms during evaluation
        (if (and (consp form) (symbolp (car form)) (op=defun (car form)))
            (h0-eval-forms (cdr forms) env fenv)
            (if (null (cdr forms))
                (h0-eval form env fenv)
                (progn
                  (h0-eval form env fenv)
                  (h0-eval-forms (cdr forms) env fenv)))))))

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
  (setq *op-string-length* (intern "STRING-LENGTH"))
  (setq *op-string-ref* (intern "STRING-REF"))
  (setq *op-string=* (intern "STRING="))
  (setq *op-symbol-name* (intern "SYMBOL-NAME"))
  (setq *op-logand* (intern "LOGAND"))
  (setq *op-logior* (intern "LOGIOR"))
  (setq *op-ash* (intern "ASH"))
  (setq *op-eq* (intern "EQ"))
  (setq *op-eql* (intern "EQL"))
  (setq *op-get-tag* (intern "GET-TAG"))
  (setq *op-length* (intern "LENGTH"))
  (setq *op-make-vector* (intern "MAKE-VECTOR"))
  (setq *op-vector-length* (intern "VECTOR-LENGTH"))
  (setq *op-vector-set* (intern "VECTOR-SET"))
  (setq *op-vector-ref* (intern "VECTOR-REF"))
  (setq *op-reverse* (intern "REVERSE"))
  (setq *op-make-string-from-vector* (intern "MAKE-STRING-FROM-VECTOR"))
  (setq *op-make-symbol-from-string* (intern "MAKE-SYMBOL-FROM-STRING"))
  nil)

;; Environment lookup for compilation - returns offset or nil
;; Now uses eq since all symbols are properly interned
;; Note: Uses separate helper functions to avoid nested closure issues
(defun c-env-lookup (sym env)
  (c-env-search (symbol-name sym) env #x0))

;; Search environment for matching name - returns (cons offset nil) or nil
;; Returns cons cell so offset 0 is distinguishable from not-found (nil)
(defun c-env-search (sym-name env offset)
  (if (null env)
      nil
      (let ((entry-name (car (car env))))
        (if (c-names-match sym-name entry-name)
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

;; Compare characters of two strings up to length len, starting at index i
(defun c-chars-match (s1 s2 len i)
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (c-chars-match s1 s2 len (+ i #x1))
          nil)))

;; Extend compilation environment with new bindings
;; Bindings is list of (name . value) pairs, we just need the names
(defun c-env-extend (bindings env)
  (if (null bindings)
      env
      (let ((b (car bindings)))
        (c-env-extend (cdr bindings)
                      (cons (cons (symbol-name (car b)) nil) env)))))

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

;; Check if IR node has a specific tag (numeric comparison)
(defun h0-has-tag-n (ir tag)
  (if (consp ir)
      (= (car ir) tag)
      nil))


;;; Free variable analysis for closures

;; Check if a symbol is in the environment (string-based lookup)
(defun h0-in-env (sym env)
  (if (null env)
      nil
      (if (string= (symbol-name sym) (car (car env)))
          t
          (h0-in-env sym (cdr env)))))

;; Check if a symbol is in a list (using string= on symbol names)
(defun h0-member-sym (sym lst)
  (if (null lst)
      nil
      (if (string= (symbol-name sym) (symbol-name (car lst)))
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
    ((sym= (car expr) "QUOTE") acc)
    ((sym= (car expr) "LAMBDA")
     (let ((params (cadr expr))
           (body (caddr expr)))
       (h0-collect-free body (h0-append-lists params bound) env acc)))
    ((sym= (car expr) "LET")
     (let* ((bindings (cadr expr))
            (body (caddr expr))
            (names (h0-binding-names bindings))
            (vals (h0-binding-vals bindings))
            (acc2 (h0-collect-free-list vals bound env acc))
            (new-bound (h0-append-lists names bound)))
       (h0-collect-free body new-bound env acc2)))
    ((sym= (car expr) "LET*")
     (let* ((bindings (cadr expr))
            (body (caddr expr)))
       (h0-collect-free-let* bindings body bound env acc)))
    (t (h0-collect-free-list expr bound env acc))))

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
      (if (string= (symbol-name sym) (car (car env)))
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

(defun h0-make-env-with-offset (syms base rest)
  (if (null syms)
      rest
      (cons (cons (symbol-name (car syms)) nil)
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
;; Uses sym= for string-based symbol comparison (no symbol deduplication)
(defun h0-compile (expr env fenv)
  (cond
    ;; Numbers compile to literals
    ((numberp expr) (list (ir-tag-lit) expr))
    ;; nil is 0
    ((null expr) (list (ir-tag-lit) #x0))
    ;; t is 1
    ((sym= expr "T") (list (ir-tag-lit) #x1))
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
         ((sym= op "QUOTE")
          (let ((val (cadr expr)))
            (if (numberp val)
                (list (ir-tag-lit) val)
                (fatal-error-ir "h0-compile: Non-number quote"))))
         ;; If
         ((sym= op "IF")
          (let* ((test-ir (h0-compile (cadr expr) env fenv))
                 (then-ir (h0-compile (caddr expr) env fenv))
                 (else-ir (if (cadddr expr)
                              (h0-compile (cadddr expr) env fenv)
                              (list (ir-tag-lit) #x0))))
            (list (ir-tag-if) test-ir then-ir else-ir)))
         ;; Let
         ((sym= op "LET")
          (h0-compile-let (cadr expr) (caddr expr) env fenv))
         ;; Let*
         ((sym= op "LET*")
          (h0-compile-let (cadr expr) (caddr expr) env fenv))
         ;; Setq
         ((sym= op "SETQ")
          (let* ((var-sym (cadr expr))
                 (val-ir (h0-compile (caddr expr) env fenv))
                 (result (c-env-lookup var-sym env)))
            (if result
                (list (ir-tag-setq) (car result) val-ir)
                (fatal-error-ir "h0-compile: SETQ unknown variable"))))
         ;; Progn
         ((sym= op "PROGN")
          (h0-compile-progn (cdr expr) env fenv))
         ;; Defun returns nil during compilation
         ((sym= op "DEFUN")
          (list (ir-tag-lit) #x0))
         ;; Arithmetic
         ((sym= op "+")
          (h0-compile-add (cdr expr) env fenv))
         ((sym= op "-")
          (h0-compile-sub (cdr expr) env fenv))
         ((sym= op "*")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-mul) l r)))
         ((sym= op "/")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-div) l r)))
         ((sym= op "MOD")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-mod) l r)))
         ;; Comparisons
         ((sym= op "=")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-eq) l r)))
         ((sym= op "<")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-lt) l r)))
         ((sym= op ">")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-gt) l r)))
         ((sym= op "<=")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-le) l r)))
         ((sym= op ">=")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-ge) l r)))
         ;; List operations
         ((sym= op "CONS")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cons) l r)))
         ((sym= op "CAR")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-car) v)))
         ((sym= op "CDR")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-cdr) v)))
         ((sym= op "NULL")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-null) v)))
         ;; String operations
         ((sym= op "STRING-LENGTH")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-str-len) v)))
         ((sym= op "STRING-REF")
          (let* ((str (h0-compile (cadr expr) env fenv))
                 (idx (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-str-ref) str idx)))
         ;; Type predicates
         ((sym= op "EQ")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-eq) l r)))
         ((sym= op "CONSP")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-consp) v)))
         ((sym= op "SYMBOLP")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-symbolp) v)))
         ((sym= op "NUMBERP")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-numberp) v)))
         ((sym= op "STRINGP")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-stringp) v)))
         ((sym= op "KEYWORDP")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-keywordp) v)))
         ;; Bitwise operations
         ((sym= op "LOGAND")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-logand) l r)))
         ((sym= op "LOGIOR")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-logior) l r)))
         ((sym= op "ASH")
          (let* ((val (h0-compile (cadr expr) env fenv))
                 (shift (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-ash) val shift)))
         ;; Boolean not
         ((sym= op "NOT")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-not) v)))
         ;; OR - expand to if chain: (or a b c) => (if a a (if b b c))
         ((sym= op "OR")
          (h0-compile-or (cdr expr) env fenv))
         ;; AND - expand to if chain: (and a b c) => (if a (if b c nil) nil)
         ((sym= op "AND")
          (h0-compile-and (cdr expr) env fenv))
         ;; LENGTH - list length
         ((sym= op "LENGTH")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-length) v)))
         ;; COND - expand to nested IFs
         ((sym= op "COND")
          (h0-compile-cond (cdr expr) env fenv))
         ;; CASE - expand to nested IFs with EQ comparisons
         ((sym= op "CASE")
          (h0-compile-case (cadr expr) (cddr expr) env fenv))
         ;; WHEN - expand to (if test (progn body...))
         ((sym= op "WHEN")
          (let* ((test-ir (h0-compile (cadr expr) env fenv))
                 (body-ir (h0-compile-progn (cddr expr) env fenv))
                 (else-ir (list (ir-tag-lit) #x0)))
            (list (ir-tag-if) test-ir body-ir else-ir)))
         ;; UNLESS - expand to (if (not test) (progn body...))
         ((sym= op "UNLESS")
          (let* ((test-ir (h0-compile (cadr expr) env fenv))
                 (not-test-ir (list (ir-tag-not) test-ir))
                 (body-ir (h0-compile-progn (cddr expr) env fenv))
                 (else-ir (list (ir-tag-lit) #x0)))
            (list (ir-tag-if) not-test-ir body-ir else-ir)))
         ;; LAMBDA - create closure
         ((sym= op "LAMBDA")
          (let* ((params (cadr expr))
                 (body (caddr expr))
                 (free-vars (h0-find-free-vars body params env))
                 (free-offsets (h0-get-free-offsets free-vars env))
                 (param-env (h0-make-param-env params free-vars))
                 (body-ir (h0-compile body param-env fenv)))
            (list (ir-tag-lambda) params body-ir free-vars free-offsets)))
         ;; FUNCALL - call function value
         ((sym= op "FUNCALL")
          (let* ((fn-ir (h0-compile (cadr expr) env fenv))
                 (args (cddr expr))
                 (args-ir (h0-compile-args args env fenv)))
            (list (ir-tag-funcall) fn-ir args-ir)))
         ;; FLET - local function definitions (non-recursive)
         ((sym= op "FLET")
          (h0-compile-flet (cadr expr) (cddr expr) env fenv))
         ;; LABELS - local function definitions (recursive)
         ((sym= op "LABELS")
          (h0-compile-labels (cadr expr) (cddr expr) env fenv))
         ;; MAPCAR - expand to labels loop
         ;; (mapcar fn list) => (labels ((loop (l acc) ...)) (loop list nil))
         ((sym= op "MAPCAR")
          (h0-compile-mapcar (cadr expr) (caddr expr) env fenv))
         ;; REVERSE - expand to labels loop
         ((sym= op "REVERSE")
          (h0-compile-reverse (cadr expr) env fenv))
         ;; LIST - expand to nested CONS
         ((sym= op "LIST")
          (h0-compile-list (cdr expr) env fenv))
         ;; CADR - (car (cdr x))
         ((sym= op "CADR")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-car) (list (ir-tag-cdr) v))))
         ;; CDDR - (cdr (cdr x))
         ((sym= op "CDDR")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-cdr) (list (ir-tag-cdr) v))))
         ;; CADDR - (car (cdr (cdr x)))
         ((sym= op "CADDR")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-car) (list (ir-tag-cdr) (list (ir-tag-cdr) v)))))
         ;; CADDDR - (car (cdr (cdr (cdr x))))
         ((sym= op "CADDDR")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-car) (list (ir-tag-cdr) (list (ir-tag-cdr) (list (ir-tag-cdr) v))))))
         ;; CAAR - (car (car x))
         ((sym= op "CAAR")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-car) (list (ir-tag-car) v))))
         ;; CDAR - (cdr (car x))
         ((sym= op "CDAR")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-cdr) (list (ir-tag-car) v))))
         ;; NTH - expand to nested CDRs and CAR
         ((sym= op "NTH")
          (h0-compile-nth (cadr expr) (caddr expr) env fenv))
         ;; LOGNOT - (logxor x -1) but we don't have logxor, use (- -1 x) for 2's complement
         ((sym= op "LOGNOT")
          (let ((v (h0-compile (cadr expr) env fenv)))
            ;; lognot x = -1 - x in 2's complement (actually xor with -1)
            ;; For now use subtraction: (- (- 0 1) x) = -1 - x
            (list (ir-tag-sub) (list (ir-tag-lit) #x-1) v)))
         ;; >= comparison already exists, add /= (not equal)
         ((sym= op "/=")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-not) (list (ir-tag-cmp-eq) l r))))
         ;; If op is a cons (e.g., lambda expression), compile as funcall
         ((consp op)
          (let* ((fn-ir (h0-compile op env fenv))
                 (args (cdr expr))
                 (args-ir (h0-compile-args args env fenv)))
            (list (ir-tag-funcall) fn-ir args-ir)))
         ;; Default - unknown operator - CRASH
         (t (fatal-error-ir "h0-compile: Unknown operator")))))
    ;; Default - unknown expression type - CRASH
    (t (fatal-error-ir "h0-compile: Unknown expression type"))))

;; Compile addition with constant folding
(defun h0-compile-add (args env fenv)
  (if (null args)
      (fatal-error-ir "h0-compile-add: Empty addition")
      (if (null (cdr args))
          (h0-compile (car args) env fenv)
          (let* ((left-ir (h0-compile (car args) env fenv))
                 (right-ir (h0-compile (cadr args) env fenv)))
            ;; Constant folding
            (if (and (h0-has-tag-n left-ir (ir-tag-lit)) (h0-has-tag-n right-ir (ir-tag-lit)))
                (list (ir-tag-lit) (+ (cadr left-ir) (cadr right-ir)))
                (list (ir-tag-add) left-ir right-ir))))))

;; Compile subtraction with constant folding
(defun h0-compile-sub (args env fenv)
  (if (null args)
      (fatal-error-ir "h0-compile-sub: Empty subtraction")
      (if (null (cdr args))
          ;; Unary minus
          (let ((arg-ir (h0-compile (car args) env fenv)))
            (if (h0-has-tag-n arg-ir (ir-tag-lit))
                (list (ir-tag-lit) (- #x0 (cadr arg-ir)))
                (list (ir-tag-sub) (list (ir-tag-lit) #x0) arg-ir)))
          (let* ((left-ir (h0-compile (car args) env fenv))
                 (right-ir (h0-compile (cadr args) env fenv)))
            ;; Constant folding
            (if (and (h0-has-tag-n left-ir (ir-tag-lit)) (h0-has-tag-n right-ir (ir-tag-lit)))
                (list (ir-tag-lit) (- (cadr left-ir) (cadr right-ir)))
                (list (ir-tag-sub) left-ir right-ir))))))

;; Compile let - iterate through bindings, extending environment
;; Store symbol name (string) in env for string-based lookup
(defun h0-compile-let (bindings body env fenv)
  (if (null bindings)
      (h0-compile body env fenv)
      (let* ((b (car bindings))
             (var-sym (car b))
             (var-name (symbol-name var-sym))
             (val-ir (h0-compile (cadr b) env fenv))
             ;; Store symbol name string for string= lookup
             (new-env (cons (cons var-name nil) env))
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
                 (temp-name "OR-TMP")
                 (temp-env (cons (cons temp-name nil) env))
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
            (if (sym= test "T")
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
            (if (sym= test "T")
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
         (temp-name "#:CASE-KEY")
         (new-env (cons (cons temp-name nil) env)))
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
        (if (or (sym= keys "OTHERWISE") (sym= keys "T"))
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
;;; ARM64 Code Generation - IR to machine code
;;; ==========================================================================
;;; Generates ARM64 machine code bytes from IR
;;; Uses tagged fixnum representation: value << 4
;;; Registers:
;;;   x0-x4   - arguments and return value
;;;   x20     - environment base (stack frame)
;;;   x28     - heap bump pointer

;; ARM64 instruction encoders (inline for self-hosting)

;; MOVZ Xd, #imm16
(defun a64-movz (rd imm)
  (let ((inst (logior #xD2800000
                      (ash (logand imm #xFFFF) #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; MOVK Xd, #imm16, LSL #shift
(defun a64-movk (rd imm shift)
  (let* ((hw (/ shift #x10))  ; hw field: 0, 1, 2, or 3 for 0, 16, 32, 48
         (inst (logior #xF2800000
                       (ash hw #x15)  ; hw at bits 21-22
                       (ash (logand imm #xFFFF) #x5)
                       rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; ADD Xd, Xn, #imm12
(defun a64-add-imm (rd rn imm)
  (let ((inst (logior #x91000000
                      (ash (logand imm #xFFF) #xA)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; ADD Xd, Xn, Xm
(defun a64-add-reg (rd rn rm)
  (let ((inst (logior #x8B000000
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; SUB Xd, Xn, #imm12
(defun a64-sub-imm (rd rn imm)
  (let ((inst (logior #xD1000000
                      (ash (logand imm #xFFF) #xA)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; SUB Xd, Xn, Xm
(defun a64-sub-reg (rd rn rm)
  (let ((inst (logior #xCB000000
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; MUL Xd, Xn, Xm (actually MADD Xd, Xn, Xm, XZR)
(defun a64-mul (rd rn rm)
  (let ((inst (logior #x9B007C00
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; SDIV Xd, Xn, Xm
(defun a64-sdiv (rd rn rm)
  (let ((inst (logior #x9AC00C00
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; MSUB Xd, Xn, Xm, Xa (Xa - Xn*Xm)
(defun a64-msub (rd rn rm ra)
  (let ((inst (logior #x9B008000
                      (ash rm #x10)
                      (ash ra #xA)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LSL Xd, Xn, #shift (actually UBFM)
(defun a64-lsl-imm (rd rn shift)
  (let* ((immr (logand (- #x40 shift) #x3F))
         (imms (- #x3F shift))
         (inst (logior #xD3400000
                       (ash immr #x10)
                       (ash imms #xA)
                       (ash rn #x5)
                       rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LSR Xd, Xn, #shift (actually UBFM)
(defun a64-lsr-imm (rd rn shift)
  (let ((inst (logior #xD340FC00
                      (ash shift #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; CMP Xn, #imm12 (actually SUBS XZR, Xn, #imm)
(defun a64-cmp-imm (rn imm)
  (let ((inst (logior #xF100001F
                      (ash (logand imm #xFFF) #xA)
                      (ash rn #x5))))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; CMP Xn, Xm (actually SUBS XZR, Xn, Xm)
(defun a64-cmp-reg (rn rm)
  (let ((inst (logior #xEB00001F
                      (ash rm #x10)
                      (ash rn #x5))))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; CSET Xd, cond (actually CSINC Xd, XZR, XZR, invert(cond))
(defun a64-cset (rd cond)
  (let* ((inv-cond (logxor cond #x1))  ; Invert condition
         (inst (logior #x9A9F07E0
                       (ash inv-cond #xC)
                       rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; Condition codes
(defun cond-eq () #x0)
(defun cond-ne () #x1)
(defun cond-lt () #xB)
(defun cond-gt () #xC)
(defun cond-le () #xD)
(defun cond-ge () #xA)

;; B.cond offset (conditional branch)
(defun a64-b-cond (cond offset)
  (let* ((imm19 (logand (ash offset #x-2) #x7FFFF))
         (inst (logior #x54000000
                       (ash imm19 #x5)
                       cond)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; B offset (unconditional branch)
(defun a64-b (offset)
  (let* ((imm26 (logand (ash offset #x-2) #x3FFFFFF))
         (inst (logior #x14000000 imm26)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LDR Xt, [Xn, #imm12*8]
(defun a64-ldr (rt rn imm)
  (let* ((offset (ash imm #x-3))  ; Divide by 8 for scaled offset
         (inst (logior #xF9400000
                       (ash (logand offset #xFFF) #xA)
                       (ash rn #x5)
                       rt)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; STR Xt, [Xn, #imm12*8]
(defun a64-str (rt rn imm)
  (let* ((offset (ash imm #x-3))  ; Divide by 8 for scaled offset
         (inst (logior #xF9000000
                       (ash (logand offset #xFFF) #xA)
                       (ash rn #x5)
                       rt)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; MOV Xd, Xm (ORR Xd, XZR, Xm)
(defun a64-mov-reg (rd rm)
  (let ((inst (logior #xAA0003E0
                      (ash rm #x10)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; RET (BR LR)
(defun a64-ret ()
  (list #xC0 #x03 #x5F #xD6))

;; BLR Xn - branch with link to register (indirect call)
;; Encoding: 0xD63F0000 | (Rn << 5)
(defun a64-blr (rn)
  (let ((inst (logior #xD63F0000
                      (ash rn #x5))))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; BR Xn - branch to register (indirect jump, no link)
;; Encoding: 0xD61F0000 | (Rn << 5)
(defun a64-br (rn)
  (let ((inst (logior #xD61F0000
                      (ash rn #x5))))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; STP X1, X2, [Xn, #imm] - store pair (for saving registers)
;; Encoding: 0xA9000000 | (imm7 << 15) | (Rt2 << 10) | (Rn << 5) | Rt
;; imm7 is offset/8 (must be multiple of 8)
(defun a64-stp (rt rt2 rn imm)
  (let* ((imm7 (logand (ash imm #x-3) #x7F))
         (inst (logior #xA9000000
                       (ash imm7 #xF)
                       (ash rt2 #xA)
                       (ash rn #x5)
                       rt)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LDP X1, X2, [Xn, #imm] - load pair (for restoring registers)
;; Encoding: 0xA9400000 | (imm7 << 15) | (Rt2 << 10) | (Rn << 5) | Rt
(defun a64-ldp (rt rt2 rn imm)
  (let* ((imm7 (logand (ash imm #x-3) #x7F))
         (inst (logior #xA9400000
                       (ash imm7 #xF)
                       (ash rt2 #xA)
                       (ash rn #x5)
                       rt)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LDRB Wt, [Xn, #offset] - load byte, zero-extend
;; Encoding: 0x39400000 | (offset << 10) | (Rn << 5) | Rt
(defun a64-ldrb (rt rn offset)
  (let ((inst (logior #x39400000
                      (ash (logand offset #xFFF) #xA)
                      (ash rn #x5)
                      rt)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; STRB Wt, [Xn, #offset] - store byte
;; Encoding: 0x39000000 | (offset << 10) | (Rn << 5) | Rt
(defun a64-strb (rt rn offset)
  (let ((inst (logior #x39000000
                      (ash (logand offset #xFFF) #xA)
                      (ash rn #x5)
                      rt)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; AND Xd, Xn, #mask - AND with immediate
;; ARM64 logical immediate encoding is complex
;; For common masks: #xF (low 4 bits), #x7 (low 3 bits)
(defun a64-and-imm (rd rn mask)
  (let* ((base #x92400000)
         (inst (cond
                 ;; Low 3 bits: N=1, immr=0, imms=2
                 ((= mask #x7) (logior base (ash #x2 #xA) (ash rn #x5) rd))
                 ;; Low 4 bits: N=1, immr=0, imms=3
                 ((= mask #xF) (logior base (ash #x3 #xA) (ash rn #x5) rd))
                 ;; Low 8 bits: N=1, immr=0, imms=7
                 ((= mask #xFF) (logior base (ash #x7 #xA) (ash rn #x5) rd))
                 ;; Low 16 bits: N=1, immr=0, imms=15
                 ((= mask #xFFFF) (logior base (ash #xF #xA) (ash rn #x5) rd))
                 ;; Default: return nop for unsupported masks
                 (t #xD503201F))))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; AND Xd, Xn, Xm - AND registers
;; Encoding: 0x8A000000 | (Rm << 16) | (Rn << 5) | Rd
(defun a64-and-reg (rd rn rm)
  (let ((inst (logior #x8A000000
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; ORR Xd, Xn, Xm - OR registers
;; Encoding: 0xAA000000 | (Rm << 16) | (Rn << 5) | Rd
(defun a64-orr-reg (rd rn rm)
  (let ((inst (logior #xAA000000
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; ASR Xd, Xn, #shift - arithmetic shift right immediate
;; Encoding: SBFM Xd, Xn, #shift, #63 = 0x9340FC00 | (shift << 16) | (Rn << 5) | Rd
(defun a64-asr-imm (rd rn shift)
  (let ((inst (logior #x9340FC00
                      (ash shift #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LSL Xd, Xn, Xm - logical shift left register
;; Encoding: LSLV = 0x9AC02000 | (Rm << 16) | (Rn << 5) | Rd
(defun a64-lsl-reg (rd rn rm)
  (let ((inst (logior #x9AC02000
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

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
             (mov-char (a64-movz #x0 char))
             ;; STRB w0, [x1, #idx]
             (strb (a64-strb #x0 #x1 idx)))
        (bytes-append mov-char
                      (bytes-append strb
                                    (h0-gen-str-bytes str (+ idx #x1)))))))

;; Codegen helper for string literals
;; Allocates string on heap: [length:8][chars:N][padding]
;; Returns tagged pointer with tag 4
;; MUST be defined before h0-codegen which calls it
(defun h0-codegen-str-lit (str len total-size)
  (let* (;; Store length at x28
         (mov-len-lo (a64-movz #x0 (logand len #xFFFF)))
         (str-len (a64-str #x0 #x1C #x0))
         ;; Get string base address (x28 + 8)
         (add-base (a64-add-imm #x1 #x1C #x8))
         ;; Generate STRB instructions for each character
         (char-stores (h0-gen-str-bytes str #x0))
         ;; Save tagged pointer to x0: x28 | 4
         (mov-ptr (a64-mov-reg #x0 #x1C))
         (tag-ptr (a64-add-imm #x0 #x0 #x4))
         ;; Bump heap pointer
         (bump-heap (a64-add-imm #x1C #x1C total-size)))
    (bytes-append-all (list mov-len-lo str-len add-base
                            char-stores mov-ptr tag-ptr bump-heap))))

;; Codegen helper for keyword literals
;; Same layout as strings but with tag 7 instead of 4
;; MUST be defined before h0-codegen which calls it
(defun h0-codegen-kw-lit (str len total-size)
  (let* (;; Store length at x28
         (mov-len-lo (a64-movz #x0 (logand len #xFFFF)))
         (str-len (a64-str #x0 #x1C #x0))
         ;; Get string base address (x28 + 8)
         (add-base (a64-add-imm #x1 #x1C #x8))
         ;; Generate STRB instructions for each character
         (char-stores (h0-gen-str-bytes str #x0))
         ;; Save tagged pointer to x0: x28 | 7 (keyword tag)
         (mov-ptr (a64-mov-reg #x0 #x1C))
         (tag-ptr (a64-add-imm #x0 #x0 #x7))
         ;; Bump heap pointer
         (bump-heap (a64-add-imm #x1C #x1C total-size)))
    (bytes-append-all (list mov-len-lo str-len add-base
                            char-stores mov-ptr tag-ptr bump-heap))))

;; Codegen helper for binary operations
;; Inline temp slot calculation: 48 + td*8
(defun h0-codegen-binop (left-ir right-ir op-instrs td)
  (let* ((slot-off (+ #x30 (* td #x8)))
         (left-code (h0-codegen left-ir td))
         (save-left (a64-str #x0 #x1F slot-off))
         (right-code (h0-codegen right-ir (+ td #x1)))
         (move-right (a64-mov-reg #x1 #x0))
         (load-left (a64-ldr #x0 #x1F slot-off)))
    (bytes-append-all
     (list left-code save-left right-code move-right load-left op-instrs))))

;; Codegen helper for comparisons
;; Inline temp slot calculation: 48 + td*8
(defun h0-codegen-cmp (left-ir right-ir cond td)
  (let* ((slot-off (+ #x30 (* td #x8)))
         (left-code (h0-codegen left-ir td))
         (save-left (a64-str #x0 #x1F slot-off))
         (right-code (h0-codegen right-ir (+ td #x1)))
         (move-right (a64-mov-reg #x1 #x0))
         (load-left (a64-ldr #x0 #x1F slot-off)))
    (let* ((cmp-code (a64-cmp-reg #x0 #x1))
           (cset-code (a64-cset #x0 cond))
           (tag-code (a64-lsl-imm #x0 #x0 #x4)))
      (bytes-append-all
       (list left-code save-left right-code move-right load-left
             cmp-code cset-code tag-code)))))

;; Codegen helper for progn (list of IR forms)
(defun h0-codegen-progn (forms td)
  (if (null forms)
      (a64-movz #x0 #x0)
      (if (null (cdr forms))
          (h0-codegen (car forms) td)
          (bytes-append (h0-codegen (car forms) td)
                        (h0-codegen-progn (cdr forms) td)))))

;; Helper: Generate code for FUNCALL (0-2 args)
;; Closure structure (assumed): [env_ptr:8][code_ptr:8] with tag 5
;; ARM64 calling convention: x0-x7 for args, x24 (0x18) for closure env
(defun h0-codegen-funcall (fn-ir args-ir num-args td)
  (let* (;; Evaluate function expression to x0 (closure pointer)
         (fn-code (h0-codegen fn-ir td))
         ;; Save closure to temp slot 0
         (fn-slot (+ #x30 (* td #x8)))
         (save-fn (a64-str #x0 #x1F fn-slot))
         ;; Generate code to evaluate and save arguments
         (arg-code-list (h0-codegen-funcall-args args-ir (+ td #x1) #x0 nil))
         ;; arg-code-list is now a list of code sequences
         (arg-code (if arg-code-list
                       (bytes-append-all arg-code-list)
                       nil)))
    ;; Now load arguments back to x0, x1, etc. and call
    (let* (;; Load closure from temp slot 0
           (load-fn (a64-ldr #x9 #x1F fn-slot))
           ;; Untag closure (subtract 5)
           (untag-fn (a64-sub-imm #x9 #x9 #x5))
           ;; Load env pointer to x24 from [x9+0]
           (load-env (a64-ldr #x18 #x9 #x0))
           ;; Load code pointer from [x9+8] directly to x9
           (load-code (a64-ldr #x9 #x9 #x8))
           ;; Load args back to registers
           (load-args (h0-codegen-funcall-load-args num-args (+ td #x1)))
           ;; Call via BLR x9
           (call (a64-blr #x9)))
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
             (slot-off (+ #x30 (* td #x8)))
             (save-code (a64-str #x0 #x1F slot-off))
             (combined (bytes-append arg-code save-code)))
        (h0-codegen-funcall-args (cdr args-ir) (+ td #x1) (+ idx #x1)
                                 (cons combined acc)))))

;; Helper: Generate code to load arguments back to x0, x1, etc.
(defun h0-codegen-funcall-load-args (num-args td)
  (if (= num-args #x0)
      nil
      (if (= num-args #x1)
          (let ((slot-off (+ #x30 (* td #x8))))
            (a64-ldr #x0 #x1F slot-off))
          (if (= num-args #x2)
              (let ((slot-off1 (+ #x30 (* td #x8)))
                    (slot-off2 (+ #x30 (* (+ td #x1) #x8))))
                (bytes-append (a64-ldr #x0 #x1F slot-off1)
                              (a64-ldr #x1 #x1F slot-off2)))
              (fatal-error "h0-codegen-funcall-load-args: too many args")))))

;; Generate code for IR (using numeric tags)
;; td = temp slot depth (for nested expressions)
(defun h0-codegen (ir td)
  (cond
    ;; Literal - MOVZ x0, #(val << 4)
    ((h0-has-tag-n ir (ir-tag-lit))
     (let* ((val (cadr ir))
            (tagged (ash val #x4)))
       (if (< tagged #x10000)
           (a64-movz #x0 tagged)
           ;; Larger values need MOVZ + MOVK
           (let ((movz-code (a64-movz #x0 (logand tagged #xFFFF)))
                 (movk-code (a64-movk #x0 (logand (ash tagged #x-10) #xFFFF) #x10)))
             (bytes-append movz-code movk-code)))))

    ;; String literal - allocate on heap
    ;; Layout: [length:8][chars:N][padding to 16]
    ;; Returns tagged pointer with tag 4
    ((h0-has-tag-n ir (ir-tag-str-lit))
     (let* ((str (cadr ir))
            (len (string-length str))
            ;; Round up (len + 8) to 16-byte boundary
            (total-size (logand (+ len #x8 #xF) (lognot #xF))))
       (h0-codegen-str-lit str len total-size)))

    ;; Keyword literal - allocate on heap
    ;; Layout: same as string [length:8][chars:N][padding to 16]
    ;; Returns tagged pointer with tag 7
    ((h0-has-tag-n ir (ir-tag-kw-lit))
     (let* ((kw (cadr ir))
            (str (keyword-name kw))
            (len (string-length str))
            ;; Round up (len + 8) to 16-byte boundary
            (total-size (logand (+ len #x8 #xF) (lognot #xF))))
       (h0-codegen-kw-lit str len total-size)))

    ;; Variable - load from stack frame at x20
    ((h0-has-tag-n ir (ir-tag-var))
     (let* ((off (cadr ir))
            (byte-off (* off #x8))
            (sub-code (a64-sub-imm #x1 #x14 byte-off))
            (ldr-code (a64-ldr #x0 #x1 #x0)))
       (bytes-append sub-code ldr-code)))

    ;; Addition
    ((h0-has-tag-n ir (ir-tag-add))
     (h0-codegen-binop (cadr ir) (caddr ir)
                       (a64-add-reg #x0 #x0 #x1)
                       td))

    ;; Subtraction
    ((h0-has-tag-n ir (ir-tag-sub))
     (h0-codegen-binop (cadr ir) (caddr ir)
                       (a64-sub-reg #x0 #x0 #x1)
                       td))

    ;; Multiplication (need to untag one operand)
    ((h0-has-tag-n ir (ir-tag-mul))
     (let* ((slot-off (+ #x30 (* td #x8)))
            (left-code (h0-codegen (cadr ir) td))
            (save-code (a64-str #x0 #x1F slot-off))
            (right-code (h0-codegen (caddr ir) (+ td #x1)))
            (untag-code (a64-lsr-imm #x1 #x0 #x4))
            (load-code (a64-ldr #x0 #x1F slot-off)))
       (let ((mul-code (a64-mul #x0 #x0 #x1)))
         (bytes-append-all
          (list left-code save-code right-code untag-code load-code mul-code)))))

    ;; Division
    ((h0-has-tag-n ir (ir-tag-div))
     (let* ((slot-off (+ #x30 (* td #x8)))
            (left-code (h0-codegen (cadr ir) td))
            (save-left (a64-str #x0 #x1F slot-off))
            (right-code (h0-codegen (caddr ir) (+ td #x1)))
            (untag-right (a64-lsr-imm #x1 #x0 #x4))
            (load-left (a64-ldr #x0 #x1F slot-off))
            (untag-left (a64-lsr-imm #x0 #x0 #x4))
            (divide (a64-sdiv #x0 #x0 #x1))
            (retag (a64-lsl-imm #x0 #x0 #x4)))
       (bytes-append-all
        (list left-code save-left right-code untag-right load-left
              untag-left divide retag))))           ; retag result

    ;; Modulo (a mod b = a - (a/b)*b)
    ((h0-has-tag-n ir (ir-tag-mod))
     (let* ((slot-off (+ #x30 (* td #x8)))
            (slot-off2 (+ #x30 (* (+ td #x1) #x8)))
            (left-code (h0-codegen (cadr ir) td))
            (save-left (a64-str #x0 #x1F slot-off))
            (right-code (h0-codegen (caddr ir) (+ td #x1)))
            (save-right (a64-str #x0 #x1F slot-off2)))
       (let* ((untag-right (a64-lsr-imm #x1 #x0 #x4))
              (load-left (a64-ldr #x0 #x1F slot-off))
              (untag-left (a64-lsr-imm #x0 #x0 #x4))
              (divide (a64-sdiv #x2 #x0 #x1))
              (msub (a64-msub #x0 #x2 #x1 #x0))
              (retag (a64-lsl-imm #x0 #x0 #x4)))
         (bytes-append-all
          (list left-code save-left right-code save-right untag-right
                load-left untag-left divide msub retag)))))                ; retag

    ;; Comparisons
    ((h0-has-tag-n ir (ir-tag-cmp-eq))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-eq) td))
    ((h0-has-tag-n ir (ir-tag-cmp-lt))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-lt) td))
    ((h0-has-tag-n ir (ir-tag-cmp-gt))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-gt) td))
    ((h0-has-tag-n ir (ir-tag-cmp-le))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-le) td))
    ((h0-has-tag-n ir (ir-tag-cmp-ge))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-ge) td))

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
              (a64-cmp-imm #x0 #x0)               ; test == 0?
              (a64-b-cond (cond-eq) (+ then-len #x8)) ; skip then + jump
              then-code
              (a64-b (+ else-len #x4))            ; skip else
              else-code))))

    ;; Cons
    ((h0-has-tag-n ir (ir-tag-cons))
     (let* ((slot-off (+ #x30 (* td #x8)))
            (car-code (h0-codegen (cadr ir) td))
            (save-car (a64-str #x0 #x1F slot-off))
            (cdr-code (h0-codegen (caddr ir) (+ td #x1)))
            (move-cdr (a64-mov-reg #x1 #x0)))
       (let* ((load-car (a64-ldr #x0 #x1F slot-off))
              (store-car (a64-str #x0 #x1C #x0))
              (store-cdr (a64-str #x1 #x1C #x8))
              (get-ptr (a64-mov-reg #x0 #x1C))
              (tag-cons (a64-add-imm #x0 #x0 #x1))
              (bump-heap (a64-add-imm #x1C #x1C #x10)))
         (bytes-append-all
          (list car-code save-car cdr-code move-cdr load-car
                store-car store-cdr get-ptr tag-cons bump-heap)))))        ; bump heap

    ;; Car
    ((h0-has-tag-n ir (ir-tag-car))
     (let* ((arg-ir (cadr ir))
            (arg-code (h0-codegen arg-ir td))
            (untag (a64-sub-imm #x0 #x0 #x1))
            (load-car (a64-ldr #x0 #x0 #x0)))
       (bytes-append-all
        (list arg-code untag load-car))))

    ;; Cdr
    ((h0-has-tag-n ir (ir-tag-cdr))
     (let* ((arg-ir (cadr ir))
            (arg-code (h0-codegen arg-ir td))
            (untag (a64-sub-imm #x0 #x0 #x1))
            (load-cdr (a64-ldr #x0 #x0 #x8)))
       (bytes-append-all
        (list arg-code untag load-cdr))))          ; load cdr

    ;; Null check - compare to nil (0x6), not zero
    ((h0-has-tag-n ir (ir-tag-null))
     (let* ((arg-ir (cadr ir))
            (arg-code (h0-codegen arg-ir td))
            (cmp-nil (a64-cmp-imm #x0 #x6))
            (set-cond (a64-cset #x0 (cond-eq)))
            (tag-result (a64-lsl-imm #x0 #x0 #x4)))
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
              (a64-sub-imm #x14 #x14 #x8)        ; x20 -= 8 (grow frame)
              (a64-str #x0 #x14 #x0)             ; [x20] = value (at new x20)
              body-code
              (a64-add-imm #x14 #x14 #x8)))))    ; x20 += 8 (restore frame)

    ;; Setq - variable assignment
    ;; IR: (setq-ir offset value-ir)
    ;; Evaluate value, then store to variable's stack slot
    ((h0-has-tag-n ir (ir-tag-setq))
     (let* ((offset (cadr ir))
            (val-ir (caddr ir))
            (byte-off (* offset #x8))
            (val-code (h0-codegen val-ir td))
            ;; Calculate address: x1 = x20 - byte_offset
            (sub-code (a64-sub-imm #x1 #x14 byte-off))
            ;; Store x0 to [x1]
            (str-code (a64-str #x0 #x1 #x0)))
       (bytes-append-all (list val-code sub-code str-code))))

    ;; Progn
    ((h0-has-tag-n ir (ir-tag-progn))
     (h0-codegen-progn (cadr ir) td))

    ;; String-length: get length from string header (offset -8 from tagged ptr)
    ;; String layout: [length:u64][chars...]  with tag 4
    ((h0-has-tag-n ir (ir-tag-str-len))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            ;; Untag (subtract 4), then load length from offset 0
            (untag (a64-sub-imm #x0 #x0 #x4))
            (load-len (a64-ldr #x0 #x0 #x0))
            ;; Length is already untagged, need to tag it
            (tag-result (a64-lsl-imm #x0 #x0 #x4)))
       (bytes-append-all (list arg-code untag load-len tag-result))))

    ;; String-ref: get char at index
    ;; String layout: [length:u64][chars...]
    ((h0-has-tag-n ir (ir-tag-str-ref))
     (let* ((slot-off (+ #x30 (* td #x8)))
            (str-code (h0-codegen (cadr ir) td))
            (save-str (a64-str #x0 #x1F slot-off))
            (idx-code (h0-codegen (caddr ir) (+ td #x1)))
            ;; Untag index
            (untag-idx (a64-lsr-imm #x1 #x0 #x4))
            ;; Load string ptr
            (load-str (a64-ldr #x0 #x1F slot-off))
            ;; Untag string (subtract 4)
            (untag-str (a64-sub-imm #x0 #x0 #x4))
            ;; Add 8 to skip length field, then add index
            (add-offset (a64-add-imm #x0 #x0 #x8))
            (add-idx (a64-add-reg #x0 #x0 #x1))
            ;; Load byte
            (load-byte (a64-ldrb #x0 #x0 #x0))
            ;; Tag result
            (tag-result (a64-lsl-imm #x0 #x0 #x4)))
       (bytes-append-all (list str-code save-str idx-code untag-idx load-str
                               untag-str add-offset add-idx load-byte tag-result))))

    ;; EQ: pointer equality
    ((h0-has-tag-n ir (ir-tag-eq))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-eq) td))

    ;; Consp: check if tag is 1
    ((h0-has-tag-n ir (ir-tag-consp))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            (and-tag (a64-and-imm #x0 #x0 #xF))
            (cmp-tag (a64-cmp-imm #x0 #x1))
            (cset (a64-cset #x0 (cond-eq)))
            (tag-result (a64-lsl-imm #x0 #x0 #x4)))
       (bytes-append-all (list arg-code and-tag cmp-tag cset tag-result))))

    ;; Symbolp: check if tag is 2
    ((h0-has-tag-n ir (ir-tag-symbolp))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            (and-tag (a64-and-imm #x0 #x0 #xF))
            (cmp-tag (a64-cmp-imm #x0 #x2))
            (cset (a64-cset #x0 (cond-eq)))
            (tag-result (a64-lsl-imm #x0 #x0 #x4)))
       (bytes-append-all (list arg-code and-tag cmp-tag cset tag-result))))

    ;; Numberp: check if tag is 0 (fixnum)
    ((h0-has-tag-n ir (ir-tag-numberp))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            (and-tag (a64-and-imm #x0 #x0 #xF))
            (cmp-tag (a64-cmp-imm #x0 #x0))
            (cset (a64-cset #x0 (cond-eq)))
            (tag-result (a64-lsl-imm #x0 #x0 #x4)))
       (bytes-append-all (list arg-code and-tag cmp-tag cset tag-result))))

    ;; Stringp: check if tag is 4
    ((h0-has-tag-n ir (ir-tag-stringp))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            (and-tag (a64-and-imm #x0 #x0 #xF))
            (cmp-tag (a64-cmp-imm #x0 #x4))
            (cset (a64-cset #x0 (cond-eq)))
            (tag-result (a64-lsl-imm #x0 #x0 #x4)))
       (bytes-append-all (list arg-code and-tag cmp-tag cset tag-result))))

    ;; Keywordp: check if tag is 7
    ((h0-has-tag-n ir (ir-tag-keywordp))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            (and-tag (a64-and-imm #x0 #x0 #xF))
            (cmp-tag (a64-cmp-imm #x0 #x7))
            (cset (a64-cset #x0 (cond-eq)))
            (tag-result (a64-lsl-imm #x0 #x0 #x4)))
       (bytes-append-all (list arg-code and-tag cmp-tag cset tag-result))))

    ;; Logand: bitwise AND (both operands tagged, result tagged)
    ((h0-has-tag-n ir (ir-tag-logand))
     (h0-codegen-binop (cadr ir) (caddr ir)
                       (a64-and-reg #x0 #x0 #x1)
                       td))

    ;; Logior: bitwise OR
    ((h0-has-tag-n ir (ir-tag-logior))
     (h0-codegen-binop (cadr ir) (caddr ir)
                       (a64-orr-reg #x0 #x0 #x1)
                       td))

    ;; ASH: arithmetic shift (untag, shift, retag)
    ;; Positive shift = left, negative = right
    ((h0-has-tag-n ir (ir-tag-ash))
     (let* ((slot-off (+ #x30 (* td #x8)))
            (val-code (h0-codegen (cadr ir) td))
            (save-val (a64-str #x0 #x1F slot-off))
            (shift-code (h0-codegen (caddr ir) (+ td #x1)))
            ;; Untag shift amount
            (untag-shift (a64-asr-imm #x1 #x0 #x4))
            ;; Load value
            (load-val (a64-ldr #x0 #x1F slot-off))
            ;; Untag value
            (untag-val (a64-asr-imm #x0 #x0 #x4))
            ;; Variable shift: if x1 >= 0, LSL; else ASR by -x1
            ;; For simplicity, use LSL for now (assume positive shifts)
            (shift-op (a64-lsl-reg #x0 #x0 #x1))
            ;; Retag
            (retag (a64-lsl-imm #x0 #x0 #x4)))
       (bytes-append-all (list val-code save-val shift-code untag-shift
                               load-val untag-val shift-op retag))))

    ;; NOT: boolean negation (nil -> t, anything else -> nil)
    ((h0-has-tag-n ir (ir-tag-not))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            ;; Compare to nil (6)
            (cmp-nil (a64-cmp-imm #x0 #x6))
            ;; If equal to nil, result is 1 (t), else 0
            (cset (a64-cset #x0 (cond-eq)))
            ;; Tag result
            (tag-result (a64-lsl-imm #x0 #x0 #x4)))
       (bytes-append-all (list arg-code cmp-nil cset tag-result))))

    ;; LENGTH: count cons cells in list
    ;; Loop: x0 = list ptr, x1 = counter
    ;; While x0 != nil: x1++, x0 = cdr(x0)
    ((h0-has-tag-n ir (ir-tag-length))
     (let* ((arg-code (h0-codegen (cadr ir) td))
            ;; Initialize counter to 0
            (init-counter (a64-movz #x1 #x0))
            ;; Loop start label offset (will be patched)
            ;; Check if x0 == nil (6)
            (loop-start-offset #x0)
            (cmp-nil (a64-cmp-imm #x0 #x6))
            ;; Branch to end if equal (skip to move-result)
            (branch-end (a64-b-cond (cond-eq) #x14))  ; skip 20 bytes (5 instructions * 4)
            ;; Increment counter (tagged add: add 16 for +1)
            (inc-counter (a64-add-imm #x1 #x1 #x10))
            ;; Get CDR: untag, load offset 8, keep tagged
            (untag-cons (a64-sub-imm #x0 #x0 #x1))
            (load-cdr (a64-ldr #x0 #x0 #x8))
            ;; Branch back to loop start (back to cmp-nil)
            (branch-loop (a64-b #x-14))  ; back 20 bytes (5 instructions)
            ;; Move counter to result
            (move-result (a64-mov-reg #x0 #x1)))
       (bytes-append-all (list arg-code init-counter cmp-nil branch-end
                               inc-counter untag-cons load-cdr branch-loop
                               move-result))))
    ;; LAMBDA: Create closure (simplified - no lambda lifting yet)
    ;; Layout: [fn-offset:8][env-ptr:8] = 16 bytes
    ;; Returns tagged pointer with tag 5
    ((h0-has-tag-n ir (ir-tag-lambda))
     (let* ((params (cadr ir))
            (body-ir (caddr ir))
            (free-vars (cadddr ir))
            (free-offsets (nth #x4 ir)))
       ;; Simplified closure: placeholder fn-offset (0) and nil env (0x6)
       (let* (;; Store placeholder function offset (0) at heap+0
              (mov-fn-offset (a64-movz #x0 #x0))         ; x0 = 0
              (str-fn-offset (a64-str #x0 #x1C #x0))     ; [x28+0] = x0
              ;; Store nil environment (0x6) at heap+8
              (mov-env (a64-movz #x0 #x6))               ; x0 = 6 (nil)
              (str-env (a64-str #x0 #x1C #x8))           ; [x28+8] = x0
              ;; Create tagged closure pointer: x28 | 5
              (mov-ptr (a64-mov-reg #x0 #x1C))           ; x0 = x28
              (tag-closure (a64-add-imm #x0 #x0 #x5))    ; x0 = x0 | 5
              ;; Bump heap pointer by 16 bytes
              (bump-heap (a64-add-imm #x1C #x1C #x10)))  ; x28 += 16
         (bytes-append-all (list mov-fn-offset str-fn-offset
                                 mov-env str-env
                                 mov-ptr tag-closure bump-heap)))))

    ;; FUNCALL: Call closure (simplified version for 0-2 args)
    ((h0-has-tag-n ir (ir-tag-funcall))
     (let* ((fn-ir (cadr ir))
            (args-ir (caddr ir))
            (num-args (h0-list-length args-ir)))
       ;; Support 0-2 arguments for now
       (if (> num-args #x2)
           (fatal-error "h0-codegen: FUNCALL supports max 2 args")
           (h0-codegen-funcall fn-ir args-ir num-args td))))


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
    ((h0-has-tag-n ir (ir-tag-lit)) (cadr ir))
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
    ;; Lambda - create closure
    ((h0-has-tag-n ir (ir-tag-lambda))
     (let* ((params (cadr ir))
            (body-ir (caddr ir))
            (free-vars (cadddr ir))
            (free-offsets (nth #x4 ir)))
       ;; Create closure: (closure params body-ir env)
       ;; The env captures free variables at closure creation time
       (list (intern "CLOSURE") params body-ir env)))
    ;; Funcall - call closure
    ((h0-has-tag-n ir (ir-tag-funcall))
     (let* ((fn-val (h0-eval-ir (cadr ir) env))
            (args-ir (caddr ir))
            (args-vals (h0-eval-ir-args args-ir env)))
       ;; fn-val should be (closure params body-ir closure-env)
       (if (and (consp fn-val) (sym= (car fn-val) "CLOSURE"))
           (let* ((params (cadr fn-val))
                  (body-ir (caddr fn-val))
                  (closure-env (cadddr fn-val))
                  ;; Bind args to params in new env extending closure-env
                  (new-env (h0-extend-env-with-params params args-vals closure-env)))
             (h0-eval-ir body-ir new-env))
           (fatal-error "h0-eval-ir: FUNCALL on non-closure"))))
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

;; Extend environment by binding params to args
;; params is a list of parameter symbols, args-vals is a list of values
;; Returns new env with args prepended in order
(defun h0-extend-env-with-params (params args-vals base-env)
  (if (null params)
      base-env
      (h0-extend-env-with-params
        (cdr params)
        (cdr args-vals)
        (cons (car args-vals) base-env))))

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
  (let* ((i1 (a64-sub-imm #x1F #x1F #x200))        ; sub sp, sp, #512
         (i2 (a64-str #x1E #x1F #x0))              ; str x30, [sp]
         (i3 (a64-str #x1C #x1F #x8))              ; str x28, [sp, #8]
         (i4 (a64-str #x1A #x1F #x10))             ; str x26, [sp, #16]
         (i5 (a64-str #x1B #x1F #x18)))            ; str x27, [sp, #24]
    (let* ((i6 (a64-str #x14 #x1F #x20))           ; str x20, [sp, #32]
           (i7 (a64-add-imm #x14 #x1F #x40))       ; add x20, sp, #64
           (i8 (macho-adrp #x1C heap-page-offset)) ; adrp x28, heap_page
           (i9 (a64-mov-reg #x1B #x1C))            ; mov x27, x28
           (i10 (a64-add-imm #x1C #x1C #x10)))     ; add x28, x28, #16
      (let* ((i11 (macho-adr #x1A #x28))           ; adr x26, +40
             (i12 (macho-bl #x9))                  ; bl +9
             (i13 (a64-lsr-imm #x0 #x0 #x4))       ; lsr x0, x0, #4
             (i14 (a64-ldr #x14 #x1F #x20))        ; ldr x20, [sp, #32]
             (i15 (a64-ldr #x1B #x1F #x18)))       ; ldr x27, [sp, #24]
        (let* ((i16 (a64-ldr #x1A #x1F #x10))      ; ldr x26, [sp, #16]
               (i17 (a64-ldr #x1C #x1F #x8))       ; ldr x28, [sp, #8]
               (i18 (a64-ldr #x1E #x1F #x0))       ; ldr x30, [sp]
               (i19 (a64-add-imm #x1F #x1F #x200)) ; add sp, sp, #512
               (i20 (a64-ret)))                    ; ret
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

(defun write-macho-with-imports-and-heap (output-path code-bytes imports heap-size)
  "Write a Mach-O executable with external imports and heap."
  (let* ((num-imports (length imports))
         (code-size (length code-bytes)))
    ;; DEBUG: test basic let* then return
    (+ num-imports code-size)))                              ; Success

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
    ;; DEBUG: return wrapped code length
    (length wrapped-code)))

;;; Main entry point
;;; Mode is determined by first form in input.lisp:
;;;   #x100 - compile expression to IR and evaluate (compile-test)
;;;   #x200 - compile expression to IR and generate bytecode length (codegen-test)
;;;   #x300 - compile, codegen, and link to executable (link-test)
;;;   anything else - interpret using h0-eval
(defun main ()
  ;; Initialize compile-time operators first (uses eq, no symbol-name)
  (init-compile-ops)
  (let ((source (native-read-file "input.lisp")))
    (if (null source)
        #xFF  ;; File not found
        (let ((forms (read-all source)))
          (if (null forms)
              #xFE  ;; Parse error
              (let ((first-form (car forms)))
                (cond
                  ;; Compile test mode: compile and eval IR
                  ((if (numberp first-form) (= first-form #x100) nil)
                   (if (null (cdr forms))
                       #xFD  ;; No expression to compile
                       (h0-compile-and-eval (cadr forms))))
                  ;; Codegen test mode: compile and return bytecode length
                  ((if (numberp first-form) (= first-form #x200) nil)
                   (if (null (cdr forms))
                       #xFD
                       (let* ((ir (h0-compile (cadr forms) nil nil))
                              (code (h0-codegen ir #x0)))
                         (length code))))
                  ;; Link test mode: compile, codegen, link to /tmp/h0out
                  ((if (numberp first-form) (= first-form #x300) nil)
                   (if (null (cdr forms))
                       #xFD
                       (let* ((ir (h0-compile (cadr forms) nil nil))
                              (code (h0-codegen ir #x0)))
                         (deliver-with-imports-and-heap "/tmp/h0out"
                                                        code
                                                        (list "_write")
                                                        #x100000))))
                  ;; Normal interpretation mode
                  (t
                   (let ((fenv (collect-defuns forms nil)))
                     (h0-eval-forms forms nil fenv))))))))))

(main)
