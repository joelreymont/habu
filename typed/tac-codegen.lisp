;;;; TAC Codegen - Generate ARM64 from TAC
;;;;
;;;; Input: list of tac-instr + allocation-result
;;;; Output: list of ARM64 machine code bytes
;;;;
;;;; Uses arm64/asm.lisp encoders.

(defpackage :habu.codegen
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:import-from :habu.tac :tac-instr)
  (:import-from :habu.regalloc :allocation-result
                :allocation-result-vreg-to-reg
                :allocation-result-stack-size)
  (:export :generate-code :codegen-function))

(in-package :habu.codegen)

;;; Load ARM64 encoders if not already loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :arm64)
    (load "arm64/asm.lisp")))

;;; Code generation state
(defvar *code* nil "List of instruction bytes (reversed)")
(defvar *vreg-to-reg* nil "Hash table from vreg to physical register")
(defvar *labels* nil "Hash table from label -> byte offset")
(defvar *fixups* nil "List of (offset label type) for forward refs")

(defun reset-codegen ()
  (setf *code* nil)
  (setf *labels* (make-hash-table :test 'equal))
  (setf *fixups* nil))

(defun emit (&rest items)
  "Emit bytes to code stream. ARM64 functions return byte lists."
  (dolist (item items)
    (cond
      ((listp item) (dolist (byte item) (push byte *code*)))
      ((integerp item) (push item *code*))
      (t (error "emit: invalid item ~S" item)))))

(defun current-offset ()
  (length *code*))

(defun vreg->reg (vreg)
  "Convert vreg to physical register keyword."
  (let ((reg (gethash vreg *vreg-to-reg*)))
    (cond
      ((null reg) (error "vreg ~D not allocated" vreg))
      ((eq reg :spill) (error "vreg ~D spilled, need spill code" vreg))
      (t (arm64:num-to-reg reg)))))

;;; Main code generation

(defun generate-code (tac-instrs alloc)
  "Generate ARM64 code from TAC with register allocation.
   Returns: list of bytes"
  (reset-codegen)
  (setf *vreg-to-reg* (allocation-result-vreg-to-reg alloc))

  ;; First pass: collect labels
  (let ((offset 0))
    (dolist (instr tac-instrs)
      (when (and (consp instr) (eq (car instr) :tac-label))
        (setf (gethash (cadr instr) *labels*) offset))
      ;; Estimate: each TAC instruction ~1-3 ARM64 instructions (4-12 bytes)
      (incf offset 8)))

  ;; Second pass: generate code
  (dolist (instr tac-instrs)
    (codegen-instr instr))

  ;; Apply fixups
  (apply-fixups)

  (nreverse *code*))

(defun codegen-instr (instr)
  "Generate ARM64 for a single TAC instruction."
  (match tac-instr instr
    ;; === Data Movement ===
    (tac-lit (dest value)
      (emit (arm64:movz (vreg->reg dest) (logand value #xFFFF))))

    (tac-nil (dest)
      (emit (arm64:movz (vreg->reg dest) 0)))

    (tac-t (dest)
      ;; t = pointer to T symbol, use small constant for now
      (emit (arm64:movz (vreg->reg dest) 3)))

    (tac-move (dest src)
      (let ((rd (vreg->reg dest))
            (rs (vreg->reg src)))
        (unless (eq rd rs)
          (emit (arm64:mov rd rs)))))

    (tac-var (dest offset)
      ;; Load from env (x20) at offset
      (emit (arm64:ldr (vreg->reg dest) :x20 :offset (* offset 8))))

    (tac-setvar (offset src)
      (emit (arm64:str (vreg->reg src) :x20 :offset (* offset 8))))

    (tac-global (dest name)
      (declare (ignore name))
      ;; TODO: global variable lookup
      (emit (arm64:movz (vreg->reg dest) 0)))

    (tac-set-global (name src)
      (declare (ignore name src))
      ;; TODO: global variable store
      )

    ;; === Arithmetic ===
    (tac-add (dest left right)
      (emit (arm64:add (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (tac-sub (dest left right)
      (emit (arm64:sub (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (tac-mul (dest left right)
      (emit (arm64:mul (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (tac-div (dest left right)
      (emit (arm64:sdiv (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (tac-mod (dest left right)
      ;; mod = a - (a / b) * b
      ;; Use x19 as temp
      (emit (arm64:sdiv :x19 (vreg->reg left) (vreg->reg right)))
      (emit (arm64:msub (vreg->reg dest) :x19 (vreg->reg right) (vreg->reg left))))

    (tac-neg (dest value)
      (emit (arm64:neg (vreg->reg dest) (vreg->reg value))))

    ;; === Comparison ===
    (tac-eq (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    (tac-eql (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    (tac-lt (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-lt+)))

    (tac-gt (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-gt+)))

    (tac-le (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-le+)))

    (tac-ge (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-ge+)))

    (tac-zerop (dest value)
      (emit (arm64:cmp (vreg->reg value) 0 :imm t))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    ;; === Logical ===
    (tac-not (dest value)
      (emit (arm64:cmp (vreg->reg value) 0 :imm t))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    ;; === Bitwise ===
    (tac-band (dest left right)
      (emit (arm64:and* (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (tac-bor (dest left right)
      (emit (arm64:orr (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (tac-bxor (dest left right)
      (emit (arm64:eor (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (tac-bsh (dest value shift)
      ;; Positive = left shift, negative = right shift
      ;; For now, assume left shift
      (emit (arm64:lsl (vreg->reg dest) (vreg->reg value) (vreg->reg shift))))

    (tac-bnot (dest value)
      (emit (arm64:mvn (vreg->reg dest) (vreg->reg value))))

    ;; === Control Flow ===
    (tac-label (name)
      (setf (gethash name *labels*) (current-offset)))

    (tac-goto (target)
      (let ((target-offset (gethash target *labels*)))
        (if target-offset
            (emit (arm64:b (ash (- target-offset (current-offset)) -2)))
            (progn
              (push (list (current-offset) target :b) *fixups*)
              (emit (arm64:b 0))))))

    (tac-if (cond then-label)
      (emit (arm64:cmp (vreg->reg cond) 0 :imm t))
      (let ((target-offset (gethash then-label *labels*)))
        (if target-offset
            (emit (arm64:b.ne (ash (- target-offset (current-offset)) -2)))
            (progn
              (push (list (current-offset) then-label :b.ne) *fixups*)
              (emit (arm64:b.ne 0))))))

    (tac-ifnot (cond else-label)
      (emit (arm64:cmp (vreg->reg cond) 0 :imm t))
      (let ((target-offset (gethash else-label *labels*)))
        (if target-offset
            (emit (arm64:b.eq (ash (- target-offset (current-offset)) -2)))
            (progn
              (push (list (current-offset) else-label :b.eq) *fixups*)
              (emit (arm64:b.eq 0))))))

    (tac-return (value)
      ;; Move result to x0
      (let ((rv (vreg->reg value)))
        (unless (eq rv :x0)
          (emit (arm64:mov :x0 rv))))
      ;; Use fixed epilogue for habu calling convention
      (emit (fn-fixed-epilogue)))

    ;; === Function Calls ===
    (tac-param (dest index)
      ;; Load parameter from x0-x7
      (let ((rd (vreg->reg dest))
            (param-reg (arm64:num-to-reg index)))
        (unless (eq rd param-reg)
          (emit (arm64:mov rd param-reg)))))

    (tac-arg (index src)
      (let ((arg-reg (arm64:num-to-reg index))
            (rs (vreg->reg src)))
        (unless (eq arg-reg rs)
          (emit (arm64:mov arg-reg rs)))))

    (tac-call (dest name nargs)
      (declare (ignore name nargs))
      ;; TODO: resolve function address and emit BL
      (emit (arm64:bl 0))
      (let ((rd (vreg->reg dest)))
        (unless (eq rd :x0)
          (emit (arm64:mov rd :x0)))))

    (tac-funcall (dest fn nargs)
      (declare (ignore nargs))
      (emit (arm64:blr (vreg->reg fn)))
      (let ((rd (vreg->reg dest)))
        (unless (eq rd :x0)
          (emit (arm64:mov rd :x0)))))

    ;; === List Operations ===
    (tac-cons (dest car cdr)
      ;; Call runtime cons
      (emit (arm64:mov :x0 (vreg->reg car)))
      (emit (arm64:mov :x1 (vreg->reg cdr)))
      ;; TODO: call cons runtime
      (emit (arm64:bl 0))
      (let ((rd (vreg->reg dest)))
        (unless (eq rd :x0)
          (emit (arm64:mov rd :x0)))))

    (tac-car (dest cell)
      ;; Clear tag bits and load
      (emit (arm64:and* (vreg->reg dest) (vreg->reg cell) -16 :imm t))
      (emit (arm64:ldr (vreg->reg dest) (vreg->reg dest))))

    (tac-cdr (dest cell)
      (emit (arm64:and* (vreg->reg dest) (vreg->reg cell) -16 :imm t))
      (emit (arm64:ldr (vreg->reg dest) (vreg->reg dest) :offset 8)))

    (tac-list (dest elems)
      (declare (ignore elems))
      ;; TODO: build list
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === Type Predicates ===
    (tac-null (dest value)
      (emit (arm64:cmp (vreg->reg value) 0 :imm t))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    (tac-consp (dest value)
      ;; Check tag == 0 and value != 0
      (emit (arm64:and* :x19 (vreg->reg value) 15 :imm t))
      (emit (arm64:cmp :x19 0 :imm t))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    (tac-symbolp (dest value)
      (emit (arm64:and* :x19 (vreg->reg value) 15 :imm t))
      (emit (arm64:cmp :x19 2 :imm t))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    (tac-stringp (dest value)
      (emit (arm64:and* :x19 (vreg->reg value) 15 :imm t))
      (emit (arm64:cmp :x19 6 :imm t))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    (tac-numberp (dest value)
      ;; Fixnum: bit 0 = 1
      (emit (arm64:and* (vreg->reg dest) (vreg->reg value) 1 :imm t)))

    (tac-keywordp (dest value)
      (emit (arm64:and* :x19 (vreg->reg value) 15 :imm t))
      (emit (arm64:cmp :x19 10 :imm t))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    (tac-functionp (dest value)
      (emit (arm64:and* :x19 (vreg->reg value) 15 :imm t))
      (emit (arm64:cmp :x19 8 :imm t))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    ;; === String Operations ===
    (tac-string-length (dest str)
      (emit (arm64:and* :x19 (vreg->reg str) -16 :imm t))
      (emit (arm64:ldr (vreg->reg dest) :x19)))

    (tac-string-ref (dest str index)
      ;; Load byte at str + 8 + index
      (emit (arm64:and* :x19 (vreg->reg str) -16 :imm t))
      (emit (arm64:add :x19 :x19 8 :imm t))
      (emit (arm64:ldrb (vreg->reg dest) :x19 (vreg->reg index) :reg t)))

    (tac-string-concat (dest left right)
      (declare (ignore left right))
      ;; TODO: implement
      (emit (arm64:movz (vreg->reg dest) 0)))

    (tac-string-lit (dest string)
      (declare (ignore string))
      ;; TODO: load string literal address
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === Vector Operations ===
    (tac-make-vector (dest size init)
      (declare (ignore size init))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (tac-vector-ref (dest vec index)
      (emit (arm64:and* :x19 (vreg->reg vec) -16 :imm t))
      (emit (arm64:add :x19 :x19 8 :imm t))
      (emit (arm64:ldr (vreg->reg dest) :x19)))

    (tac-vector-set (vec index value)
      (declare (ignore vec index value)))

    (tac-vector-length (dest vec)
      (emit (arm64:and* :x19 (vreg->reg vec) -16 :imm t))
      (emit (arm64:ldr (vreg->reg dest) :x19)))

    ;; === Symbol Operations ===
    (tac-make-symbol (dest name)
      (declare (ignore name))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (tac-symbol-name (dest sym)
      (emit (arm64:and* :x19 (vreg->reg sym) -16 :imm t))
      (emit (arm64:ldr (vreg->reg dest) :x19 :offset 8)))

    (tac-intern (dest str)
      (declare (ignore str))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (tac-symbol-lit (dest name)
      (declare (ignore name))
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === Keyword Operations ===
    (tac-keyword-name (dest kw)
      (emit (arm64:and* :x19 (vreg->reg kw) -16 :imm t))
      (emit (arm64:ldr (vreg->reg dest) :x19 :offset 8)))

    (tac-keyword-lit (dest name)
      (declare (ignore name))
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === System ===
    (tac-exit (code)
      ;; Untag fixnum: x0 = value >> 1
      (emit (arm64:lsr :x0 (vreg->reg code) 1 :imm t))
      ;; syscall exit
      (emit (arm64:movz :x16 1))
      (emit (arm64:svc 0)))

    (tac-error (message)
      (declare (ignore message))
      ;; TODO: error handling
      )))

;;; Apply fixups for forward branches
(defun apply-fixups ()
  "Patch branch instructions with resolved offsets."
  ;; TODO: implement actual patching
  ;; For now, just report fixups (don't modify *code*)
  (dolist (fixup *fixups*)
    (let* ((offset (first fixup))
           (label (second fixup))
           (target (gethash label *labels*)))
      (declare (ignore offset label))
      (unless target
        (error "Undefined label in fixup")))))

;;; High-level function codegen

(defun fn-fixed-prologue ()
  "Generate function prologue with fixed 16KB frame (habu convention).
   Frame layout after prologue:
   sp+0x10:  x19, x20 (saved)
   sp+0x20:  x21, x22 (saved)
   sp+0x30:  x23, x24 (saved)
   sp+0x38:  x26 (code-base)
   sp+0x40:  temp slots
   sp+0x3F80: environment base (x20)
   sp+0x3FF0: x29 (fp)
   sp+0x3FF8: x30 (lr)"
  (append
   (arm64:sub :sp :sp #x4 :imm t :shift12 t) ;; sub sp, sp, #0x4000
   (arm64:str :x29 :sp :offset #x3FF0)
   (arm64:str :x30 :sp :offset #x3FF8)
   (arm64:add :x29 :sp #x3 :imm t :shift12 t) ;; fp = sp + 0x3000
   (arm64:add :x29 :x29 #xFF0 :imm t)         ;; fp = sp + 0x3FF0
   (arm64:stp :x19 :x20 :sp :offset 16)
   (arm64:stp :x21 :x22 :sp :offset 32)
   (arm64:stp :x23 :x24 :sp :offset 48)
   (arm64:str :x26 :sp :offset 56)
   (arm64:add :x20 :sp #x3 :imm t :shift12 t) ;; x20 = sp + 0x3000
   (arm64:add :x20 :x20 #xF80 :imm t)))       ;; x20 = sp + 0x3F80

(defun fn-fixed-epilogue ()
  "Generate function epilogue for fixed 16KB frame"
  (append
   (arm64:ldr :x26 :sp :offset 56)
   (arm64:ldp :x23 :x24 :sp :offset 48)
   (arm64:ldp :x21 :x22 :sp :offset 32)
   (arm64:ldp :x19 :x20 :sp :offset 16)
   (arm64:ldr :x29 :sp :offset #x3FF0)
   (arm64:ldr :x30 :sp :offset #x3FF8)
   (arm64:add :sp :sp #x4 :imm t :shift12 t) ;; add sp, sp, #0x4000
   (arm64:ret)))

(defun codegen-function (name params body-tac alloc)
  "Generate complete function code with prologue/epilogue."
  (declare (ignore name params))
  (reset-codegen)
  (setf *vreg-to-reg* (allocation-result-vreg-to-reg alloc))

  ;; Emit prologue
  (emit (fn-fixed-prologue))

  ;; Generate body
  (dolist (instr body-tac)
    (codegen-instr instr))

  ;; Apply fixups and return
  (apply-fixups)
  (nreverse *code*))
