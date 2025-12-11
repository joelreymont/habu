;;;; native-jit.lisp - Native JIT execution for self-hosted Habu
;;;;
;;;; This file contains primitives for JIT compilation and execution
;;;; when running natively (not under SBCL). These functions use:
;;;;   - mmap with MAP_JIT for executable memory
;;;;   - pthread_jit_write_protect_np for memory protection
;;;;   - sys_icache_invalidate / sys_dcache_flush for cache coherence
;;;;
;;;; For use in Stage 1+ self-hosted REPL.

;;; ============================================================
;;; Constants
;;; ============================================================

;; mmap protection flags
(defconstant +prot-read+ 1)
(defconstant +prot-write+ 2)
(defconstant +prot-exec+ 4)

;; mmap flags
(defconstant +map-private+ 2)
(defconstant +map-anon+ #x1000)
(defconstant +map-jit+ #x0800)

;; Page size for ARM64 macOS
(defconstant +jit-page-size+ 16384)

;;; ============================================================
;;; JIT Memory Allocation (extern calls)
;;; ============================================================

;; mmap(addr, len, prot, flags, fd, offset) -> addr or -1
;; addr = 0 (null), prot = RWX, flags = MAP_PRIVATE | MAP_ANON | MAP_JIT
;; fd = -1, offset = 0
(defun jit-alloc (size)
  "Allocate JIT-capable memory. Returns pointer or nil on failure."
  ;; jit-mmap handles alignment, protection, and flags internally
  (jit-mmap size))

;; munmap(addr, len) -> 0 on success
;; Note: munmap not yet implemented in native compiler, stub for now
(defun jit-free (ptr size)
  "Free JIT memory. (Currently a no-op - memory not freed)"
  ;; TODO: Add jit-munmap primitive
  0)

;;; ============================================================
;;; JIT Write Protection (ARM64 macOS specific)
;;; ============================================================

;; pthread_jit_write_protect_np(enabled)
;; enabled = 0: allow write, disallow execute
;; enabled = 1: disallow write, allow execute
(defun jit-write-enable ()
  "Enable writing to JIT memory (disables execution)."
  (jit-write-protect 0))

(defun jit-write-disable ()
  "Disable writing to JIT memory (enables execution)."
  (jit-write-protect 1))

;;; ============================================================
;;; Cache Coherence (ARM64 specific)
;;; ============================================================

;; sys_dcache_flush(start, size)
;; Flush data cache after writing code
(defun jit-flush-dcache (ptr size)
  "Flush data cache for JIT region."
  (jit-dcache-flush ptr size))

;; sys_icache_invalidate(start, size)
;; Invalidate instruction cache before execution
(defun jit-invalidate-icache (ptr size)
  "Invalidate instruction cache for JIT region."
  (jit-icache-invalidate ptr size))

;;; ============================================================
;;; JIT Code Loading
;;; ============================================================

(defun jit-load-code (code-bytes)
  "Load bytecode into JIT memory and make it executable.
   CODE-BYTES is a vector of bytes.
   Returns pointer to executable code or nil on failure."
  (let* ((size (vector-length code-bytes))
         (ptr (jit-alloc size)))
    (if (null ptr)
        nil
        (progn
          ;; Enable writing
          (jit-write-enable)
          ;; Copy bytecode
          (jit-copy-bytes ptr code-bytes size)
          ;; Flush data cache
          (jit-flush-dcache ptr size)
          ;; Disable writing (enable execution)
          (jit-write-disable)
          ;; Invalidate instruction cache
          (jit-invalidate-icache ptr size)
          ;; Return executable pointer
          ptr))))

(defun jit-copy-bytes (dst src len)
  "Copy LEN bytes from SRC vector to DST pointer."
  (labels ((copy-loop (i)
             (if (>= i len)
                 nil
                 (progn
                   (mem-set-byte dst i (vector-ref src i))
                   (copy-loop (+ i 1))))))
    (copy-loop 0)))

;;; ============================================================
;;; JIT Execution
;;; ============================================================

;; This needs inline assembly or a trampoline to:
;; 1. Save callee-saved registers
;; 2. Set up x27 (GC globals) and x28 (heap pointer)
;; 3. Call the JIT code
;; 4. Restore registers
;; 5. Return result

;; For now, we can use a simpler approach: the JIT code
;; is generated with prologue/epilogue that expects x27/x28
;; to be set up by the caller (the REPL main loop).

(defun jit-call (code-ptr)
  "Call JIT-compiled code at CODE-PTR.
   Assumes x27/x28 are already set up by the runtime.
   Returns the result in x0."
  ;; This is a placeholder - actual implementation requires
  ;; either inline assembly or a wrapper function.
  ;; The blr instruction would be: blr code-ptr
  (funcall-ptr code-ptr))

;;; ============================================================
;;; Print Functions for REPL
;;; ============================================================
;;; Note: sys-write-char has codegen issues, so we use sys-write
;;; with pre-defined single-character strings instead.

;; Single-character constant strings for output
(defvar *char-newline* "
")
(defvar *char-minus* "-")
(defvar *char-zero* "0")
(defvar *char-space* " ")
(defvar *char-lparen* "(")
(defvar *char-rparen* ")")
(defvar *char-dot* ".")
(defvar *char-quote* "\"")
(defvar *char-gt* ">")
(defvar *digit-chars* "0123456789")

(defun print-string (s)
  "Print a string to stdout."
  (sys-write 1 s (string-length s)))

(defun print-newline ()
  "Print a newline."
  (print-string *char-newline*))

(defun print-fixnum (n)
  "Print a fixnum (integer) to stdout."
  (if (< n 0)
      (progn
        (print-string *char-minus*)
        (print-fixnum-positive (- 0 n)))
      (if (= n 0)
          (print-string *char-zero*)
          (print-fixnum-positive n))))

(defun print-fixnum-positive (n)
  "Print a positive fixnum (recursive, prints digits in order)."
  (if (= n 0)
      nil
      (progn
        (print-fixnum-positive (/ n 10))
        ;; Print the last digit using string indexing
        (let ((digit (mod n 10)))
          (print-fixnum-digit digit)))))

(defun print-fixnum-digit (d)
  "Print a single digit 0-9 using the digit string."
  ;; Create a 1-char string for the digit
  (let ((s (make-string 1)))
    (string-set! s 0 (code-char (+ 48 d)))
    (print-string s)))

(defun print-nil ()
  "Print NIL."
  (print-string "NIL"))

(defun print-t ()
  "Print T."
  (print-string "T"))

;;; ============================================================
;;; Value Printer (dispatches by type)
;;; ============================================================

(defun print-value (val)
  "Print a tagged Habu value (hybrid 1+3 bit scheme)."
  (cond
    ;; nil = 0
    ((= val +nil-value+) (print-nil))
    ;; Fixnum: bit0 = 1
    ((= (logand val +fixnum-bit+) 1) (print-fixnum (ash val -1)))
    ;; Pointer types: check tag
    (t (let ((tag (logand val +tag-mask+)))
         (case tag
           (#.+tag-cons+    (print-cons val))
           (#.+tag-symbol+  (print-symbol val))
           (#.+tag-vector+  (print-vector val))
           (#.+tag-string+  (print-string-value val))
           (#.+tag-closure+ (print-string "#<CLOSURE>"))
           (#.+tag-keyword+ (print-string "#<KEYWORD>"))
           (t (print-string "#<UNKNOWN>")))))))

(defun print-cons (val)
  "Print a cons cell."
  (print-string *char-lparen*)
  (print-cons-contents val)
  (print-string *char-rparen*))

(defun print-cons-contents (val)
  "Print cons contents (car cdr ...) - hybrid scheme."
  (if (= val +nil-value+)  ;; nil = 0
      nil
      (let ((tag (logand val +tag-mask+)))
        (if (= tag +tag-cons+)  ;; cons tag 0
            (let ((ptr (logand val +ptr-mask+)))  ;; mask off tag bits
              (print-value (car-raw ptr))
              (let ((tail (cdr-raw ptr)))
                (if (= tail +nil-value+)  ;; nil = 0
                    nil
                    (let ((tail-tag (logand tail +tag-mask+)))
                      (if (= tail-tag +tag-cons+)  ;; another cons
                          (progn
                            (print-string *char-space*)
                            (print-cons-contents tail))
                          (progn
                            (print-string " . ")
                            (print-value tail)))))))
            (print-value val)))))

(defun print-symbol (val)
  "Print a symbol (hybrid scheme: ptr|tag, tag=2)."
  ;; Symbol is a pointer with tag 2, need to look up name
  (print-string "#<SYM:")
  (print-fixnum (logand val +ptr-mask+))  ; print pointer part
  (print-string *char-gt*))

(defun print-vector (val)
  "Print a vector."
  (print-string "#(")
  ;; TODO: print vector contents
  (print-string "...)"))

(defun print-string-value (val)
  "Print a string value (with quotes)."
  (print-string *char-quote*)
  ;; val is already a tagged string - sys-write handles untagging
  (sys-write 1 val (string-length val))
  (print-string *char-quote*))

;;; ============================================================
;;; Low-level Memory Access (for cons cells)
;;; ============================================================

;; Note: These work with raw pointers (tag already masked off)
;; The caller is responsible for masking: (logand val -16)

(defun car-raw (ptr)
  "Load car from raw cons pointer (no tag)."
  (mem-load-64 ptr 0))

(defun cdr-raw (ptr)
  "Load cdr from raw cons pointer (no tag)."
  (mem-load-64 ptr 8))

;;; ============================================================
;;; Input Reading
;;; ============================================================

;; Buffer for stdin reading (1KB should be plenty for a single line)
(defvar *stdin-buffer* nil)
(defconstant +stdin-buffer-size+ 1024)

(defun read-line-stdin ()
  "Read a line from stdin. Returns string or nil on EOF.
   Uses sys-read to read one byte at a time until newline or EOF."
  (if (null *stdin-buffer*)
      (setq *stdin-buffer* (make-vector +stdin-buffer-size+)))
  (read-line-loop 0))

(defun read-line-loop (pos)
  "Read bytes into buffer until newline or EOF."
  (if (>= pos (- +stdin-buffer-size+ 1))
      ;; Buffer full, return what we have
      (buffer-to-string *stdin-buffer* pos)
      ;; Read one byte - sys-read reads to start of buffer
      ;; We use a temp buffer approach: read 1 byte, then copy to pos
      (let ((n (sys-read-byte 0)))
        (if (< n 0)
            ;; EOF or error
            (if (= pos 0)
                nil  ;; Nothing read, return nil for EOF
                (buffer-to-string *stdin-buffer* pos))
            ;; Got a byte
            (if (= n 10)  ;; newline
                (buffer-to-string *stdin-buffer* pos)
                (progn
                  (buffer-byte-set *stdin-buffer* pos n)
                  (read-line-loop (+ pos 1))))))))

;;; ============================================================
;;; REPL Main Loop
;;; ============================================================

(defvar *repl-prompt* "habu> ")

(defun repl ()
  "Read-Eval-Print Loop for Habu."
  (print-string "Habu REPL")
  (print-newline)
  (print-string "Type expressions to evaluate. Ctrl-D or empty line to exit.")
  (print-newline)
  (repl-loop))

(defun repl-loop ()
  "REPL iteration."
  (print-string *repl-prompt*)
  (let ((input (read-line-stdin)))
    (if (null input)
        (progn
          (print-newline)
          (print-string "Goodbye.")
          (print-newline))
        (if (= (string-length input) 0)
            (progn
              (print-string "Goodbye.")
              (print-newline))
            (progn
              (let ((result (repl-eval-string input)))
                (print-value result)
                (print-newline))
              (repl-loop))))))

(defun repl-eval-string (str)
  "Parse, compile, and execute a string expression."
  ;; 1. Parse
  (let ((expr (read-from-string str)))
    ;; 2. Compile to IR
    (let ((ir (compile-expr-full expr nil nil)))
      ;; 3. Generate code
      (let ((code (codegen-main ir nil)))
        ;; 4. Flatten to bytes
        (let ((bytes (resolve-calls-simple code)))
          ;; 5. Load to JIT memory
          (let ((ptr (jit-load-code bytes)))
            ;; 6. Execute
            (jit-call ptr)))))))

;;; ============================================================
;;; Echo REPL (for testing I/O primitives)
;;; ============================================================
;;; Simple test REPL that echoes input back. Used to verify
;;; that stdin reading and stdout writing work correctly.

(defun echo-repl ()
  "Echo REPL for testing I/O primitives.
   Reads a line and echoes it back. Empty line or Ctrl-D exits."
  (print-string "Echo REPL")
  (print-newline)
  (print-string "Type text and press Enter. Empty line exits.")
  (print-newline)
  (echo-loop))

(defun echo-loop ()
  "Echo loop iteration."
  (print-string "> ")
  (let ((input (read-line-stdin)))
    (if (null input)
        (progn
          (print-newline)
          (print-string "Goodbye.")
          (print-newline))
        (if (= (string-length input) 0)
            (progn
              (print-string "Goodbye.")
              (print-newline))
            (progn
              (print-string "You typed: ")
              (sys-write 1 input (string-length input))
              (print-newline)
              (echo-loop))))))

;;; Entry point - call echo-repl
(echo-repl)
