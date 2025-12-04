;;; Compiling REPL - compiles arithmetic to ARM64 and executes

;;; ============================================================
;;; ARM64 Instruction Encoders (minimal set)
;;; ============================================================

;; MOVZ Xd, #imm16 -> load immediate
;; Encoding: 1 10 100101 00 imm16[15:0] Rd[4:0]
;; = 0xD2800000 | (imm << 5) | rd
(defun arm64-movz (rd imm)
  (logior #xD2800000 (ash imm 5) rd))

;; ADD Xd, Xn, Xm -> add registers
;; Encoding: 1 00 01011 00 0 Rm[4:0] 000000 Rn[4:0] Rd[4:0]
;; = 0x8B000000 | (rm << 16) | (rn << 5) | rd
(defun arm64-add (rd rn rm)
  (logior #x8B000000 (ash rm 16) (ash rn 5) rd))

;; SUB Xd, Xn, Xm -> subtract registers
;; Encoding: 1 10 01011 00 0 Rm[4:0] 000000 Rn[4:0] Rd[4:0]
;; = 0xCB000000 | (rm << 16) | (rn << 5) | rd
(defun arm64-sub (rd rn rm)
  (logior #xCB000000 (ash rm 16) (ash rn 5) rd))

;; MUL Xd, Xn, Xm -> multiply (MADD with XZR)
;; Encoding: 1 00 11011 000 Rm[4:0] 0 11111 Rn[4:0] Rd[4:0]
;; = 0x9B007C00 | (rm << 16) | (rn << 5) | rd
(defun arm64-mul (rd rn rm)
  (logior #x9B007C00 (ash rm 16) (ash rn 5) rd))

;; SDIV Xd, Xn, Xm -> signed divide
;; Encoding: 1 00 11010 110 Rm[4:0] 00001 1 Rn[4:0] Rd[4:0]
;; = 0x9AC00C00 | (rm << 16) | (rn << 5) | rd
(defun arm64-sdiv (rd rn rm)
  (logior #x9AC00C00 (ash rm 16) (ash rn 5) rd))

;; RET -> return (BR LR)
;; Encoding: 1101011 0010 11111 0000 00 11110 00000
;; = 0xD65F03C0
(defun arm64-ret ()
  #xD65F03C0)

;; Write 32-bit instruction to buffer (little-endian)
(defun emit-insn (buf offset insn)
  (buffer-byte-set buf offset (logand insn #xFF))
  (buffer-byte-set buf (+ offset 1) (logand (ash insn -8) #xFF))
  (buffer-byte-set buf (+ offset 2) (logand (ash insn -16) #xFF))
  (buffer-byte-set buf (+ offset 3) (logand (ash insn -24) #xFF))
  (+ offset 4))

;;; ============================================================
;;; Reader (simplified)
;;; ============================================================

(defun whitespace? (ch)
  (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))

(defun digit? (ch)
  (and (>= ch #x30) (<= ch #x39)))

(defun char-at (source pos)
  (if (< pos (string-length source))
      (string-ref source pos)
      #x0))

(defun digit-val (ch) (- ch #x30))

(defun skip-ws (source pos)
  (let ((ch (char-at source pos)))
    (if (whitespace? ch) (skip-ws source (+ pos 1)) pos)))

(defun read-number (source pos)
  (let ((ch (char-at source pos)))
    (if (digit? ch)
        (read-number-acc source (+ pos 1) (digit-val ch))
        (cons 0 pos))))

(defun read-number-acc (source pos acc)
  (let ((ch (char-at source pos)))
    (if (digit? ch)
        (read-number-acc source (+ pos 1) (+ (* acc 10) (digit-val ch)))
        (cons acc pos))))

(defun read-symbol (source pos)
  (let ((ch (char-at source pos)))
    (cond
      ((= ch #x2B) (cons 'ADD (+ pos 1)))
      ((= ch #x2D) (cons 'SUB (+ pos 1)))
      ((= ch #x2A) (cons 'MUL (+ pos 1)))
      ((= ch #x2F) (cons 'DIV (+ pos 1)))
      (t (cons 'UNKNOWN (+ pos 1))))))

(defun habu-read (source pos)
  (let ((pos2 (skip-ws source pos)))
    (let ((ch (char-at source pos2)))
      (cond
        ((= ch 0) (cons nil pos2))
        ((digit? ch) (read-number source pos2))
        ((= ch #x28) (read-list source (+ pos2 1)))
        ((or (= ch #x2B) (= ch #x2D) (= ch #x2A) (= ch #x2F))
         (read-symbol source pos2))
        (t (cons nil (+ pos2 1)))))))

(defun read-list (source pos)
  (let ((pos2 (skip-ws source pos)))
    (let ((ch (char-at source pos2)))
      (if (= ch #x29)
          (cons nil (+ pos2 1))
          (let ((first (habu-read source pos2)))
            (let ((rest (read-list-tail source (cdr first))))
              (cons (cons (car first) (car rest)) (cdr rest))))))))

(defun read-list-tail (source pos)
  (let ((pos2 (skip-ws source pos)))
    (let ((ch (char-at source pos2)))
      (if (= ch #x29)
          (cons nil (+ pos2 1))
          (let ((elem (habu-read source pos2)))
            (let ((rest (read-list-tail source (cdr elem))))
              (cons (cons (car elem) (car rest)) (cdr rest))))))))

(defun read-from-string (s)
  (car (habu-read s 0)))

;;; ============================================================
;;; Compiler - generates ARM64 code
;;; ============================================================

;; State: buf = code buffer, off = current offset, reg = next register (1-7)
;; Returns (value-reg . new-state) where value-reg has the result

(defun compile-expr (expr buf off reg)
  (cond
    ;; Number literal - load tagged value
    ((numberp expr)
     (let ((tagged (ash expr 4)))  ;; tag fixnum
       (let ((new-off (emit-insn buf off (arm64-movz reg tagged))))
         (cons reg (cons new-off (+ reg 1))))))

    ;; Binary operation
    ((consp expr)
     (let ((op (car expr))
           (arg1 (cadr expr))
           (arg2 (caddr expr)))
       ;; Compile first argument
       (let ((r1 (compile-expr arg1 buf off reg)))
         (let ((reg1 (car r1))
               (off1 (cadr r1))
               (reg2 (cddr r1)))
           ;; Compile second argument
           (let ((r2 (compile-expr arg2 buf off1 reg2)))
             (let ((reg2-val (car r2))
                   (off2 (cadr r2))
                   (next-reg (cddr r2)))
               ;; Generate operation (result in x0)
               ;; Simplified: operate on tagged values directly for + and -
               (cond
                 ((eq op 'ADD)
                  (let ((new-off (emit-insn buf off2 (arm64-add 0 reg1 reg2-val))))
                    (cons 0 (cons new-off next-reg))))
                 ((eq op 'SUB)
                  (let ((new-off (emit-insn buf off2 (arm64-sub 0 reg1 reg2-val))))
                    (cons 0 (cons new-off next-reg))))
                 ((eq op 'MUL)
                  (let ((new-off (emit-insn buf off2 (arm64-mul 0 reg1 reg2-val))))
                    (let ((final-off (emit-insn buf new-off #x9344FC00)))
                      (cons 0 (cons final-off next-reg)))))
                 ((eq op 'DIV)
                  (let ((off3 (emit-insn buf off2 (arm64-sdiv 0 reg1 reg2-val))))
                    (let ((off4 (emit-insn buf off3 (arm64-movz 9 16))))
                      (let ((final-off (emit-insn buf off4 (arm64-mul 0 0 9))))
                        (cons 0 (cons final-off next-reg))))))
                 (t (cons 0 (cons off2 next-reg))))))))))
    (t (cons 0 (cons off reg)))))

(defun compile-to-code (expr)
  (let ((buf (make-vector 256)))
    (let ((result (compile-expr expr buf 0 1)))
      (let ((final-off (cadr result)))
        ;; Add ret instruction
        (let ((code-size (emit-insn buf final-off (arm64-ret))))
          (cons buf code-size))))))

;;; ============================================================
;;; I/O
;;; ============================================================

(defvar *newline* "
")

(defun print-string (s) (sys-write 1 s (string-length s)))
(defun print-newline () (print-string *newline*))

(defun print-fixnum (n)
  (if (< n 0)
      (progn (print-string "-") (print-fixnum-pos (- 0 n)))
      (if (= n 0) (print-string "0") (print-fixnum-pos n))))

(defun print-fixnum-pos (n)
  (if (= n 0) nil
      (progn
        (print-fixnum-pos (/ n 10))
        (let ((s (make-string 1)))
          (string-set! s 0 (code-char (+ 48 (mod n 10))))
          (print-string s)))))

(defvar *stdin-buffer* nil)
(defconstant +stdin-buffer-size+ 1024)

(defun read-line-stdin ()
  (if (null *stdin-buffer*)
      (setq *stdin-buffer* (make-vector +stdin-buffer-size+)))
  (read-line-loop 0))

(defun read-line-loop (pos)
  (if (>= pos (- +stdin-buffer-size+ 1))
      (buffer-to-string *stdin-buffer* pos)
      (let ((n (sys-read-byte 0)))
        (if (< n 0)
            (if (= pos 0) nil (buffer-to-string *stdin-buffer* pos))
            (if (= n 10)
                (buffer-to-string *stdin-buffer* pos)
                (progn
                  (buffer-byte-set *stdin-buffer* pos n)
                  (read-line-loop (+ pos 1))))))))

;;; ============================================================
;;; JIT Execution
;;; ============================================================

(defun copy-to-jit (dst src len idx)
  (if (>= idx len)
      nil
      (progn
        (mem-set-byte dst idx (buffer-byte-ref src idx))
        (copy-to-jit dst src len (+ idx 1)))))

(defun jit-execute (code size)
  ;; Allocate JIT memory
  (let ((jit-mem (jit-mmap 16384)))
    ;; Enable write
    (jit-write-protect 0)
    ;; Copy code
    (copy-to-jit jit-mem code size 0)
    ;; Flush caches
    (jit-dcache-flush jit-mem size)
    ;; Enable execute
    (jit-write-protect 1)
    (jit-icache-invalidate jit-mem size)
    ;; Call and return result
    (funcall-ptr jit-mem)))

;;; ============================================================
;;; REPL
;;; ============================================================

(defun compile-repl ()
  (print-string "Compiling REPL")
  (print-newline)
  (print-string "Compiles arithmetic to ARM64 and executes")
  (print-newline)
  (print-string "Example: (+ 1 2) or (* 3 (+ 1 2))")
  (print-newline)
  (repl-loop))

(defun repl-loop ()
  (print-string "> ")
  (let ((input (read-line-stdin)))
    (if (null input)
        (progn (print-newline) (print-string "Goodbye.") (print-newline))
        (if (= (string-length input) 0)
            (progn (print-string "Goodbye.") (print-newline))
            (progn
              (let ((expr (read-from-string input)))
                (let ((compiled (compile-to-code expr)))
                  (let ((code (car compiled))
                        (size (cdr compiled)))
                    (let ((result (jit-execute code size)))
                      (print-fixnum result)
                      (print-newline)))))
              (repl-loop))))))

(compile-repl)
