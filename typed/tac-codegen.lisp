;;;; TAC Codegen - Generate ARM64 from TAC
;;;;
;;;; Input: list of tac-instr
;;;; Output: list of ARM64 machine code bytes
;;;;
;;;; Uses match macro for exhaustiveness checking.
;;;; Requires arm64/asm.lisp for instruction encoders.

(in-package :habu)

;;; Register allocation
;;; For now, use a simple scheme:
;;; - x0-x7: argument/result registers
;;; - x9-x15: temporary registers (caller-saved)
;;; - x19: temp for spills
;;; - x20: environment pointer (env)
;;; - x24: closure pointer
;;; - x27: runtime data pointer
;;; - x28: heap pointer
;;; - x29: frame pointer
;;; - x30: link register

(defvar *vreg-to-reg* nil "Hash table mapping vreg -> physical reg")
(defvar *next-temp-reg* 9 "Next temp register to allocate (x9-x15)")
(defvar *code-bytes* nil "Accumulated machine code bytes")
(defvar *label-offsets* nil "Hash table mapping label -> byte offset")
(defvar *fixups* nil "List of (offset . label) for forward references")

(defun reset-codegen-state ()
  (setf *vreg-to-reg* (make-hash-table))
  (setf *next-temp-reg* 9)
  (setf *code-bytes* nil)
  (setf *label-offsets* (make-hash-table))
  (setf *fixups* nil))

(defun emit-bytes (&rest bytes)
  "Emit bytes to code stream."
  (dolist (b bytes)
    (if (listp b)
        (dolist (byte b) (push byte *code-bytes*))
        (push b *code-bytes*))))

(defun current-offset ()
  "Current byte offset in code stream."
  (length *code-bytes*))

(defun allocate-reg (vreg)
  "Allocate a physical register for a vreg."
  (or (gethash vreg *vreg-to-reg*)
      (let ((reg (if (< *next-temp-reg* 16)
                     (prog1 *next-temp-reg* (incf *next-temp-reg*))
                     (error "Out of temp registers - need spilling"))))
        (setf (gethash vreg *vreg-to-reg*) reg)
        reg)))

(defun vreg-reg (vreg)
  "Get physical register for vreg, allocating if needed."
  (allocate-reg vreg))

(defun reg-keyword (num)
  "Convert register number to keyword (:x0, :x1, etc.)"
  (intern (format nil "X~D" num) :keyword))

;;; Main codegen function

(defun tac-to-arm64 (tac-instrs)
  "Generate ARM64 machine code from TAC.
   Returns: list of bytes"
  (reset-codegen-state)

  ;; First pass: collect labels
  (let ((offset 0))
    (dolist (instr tac-instrs)
      (when (tac-label-p instr)
        (setf (gethash (tac-label-name instr) *label-offsets*) offset))
      ;; Estimate instruction size (most are 4 bytes)
      (incf offset 4)))

  ;; Second pass: generate code
  (dolist (instr tac-instrs)
    (codegen-tac-instr instr))

  ;; Apply fixups for forward references
  (dolist (fixup *fixups*)
    (let* ((offset (car fixup))
           (label (cdr fixup))
           (target (gethash label *label-offsets*)))
      (unless target
        (error "Undefined label: ~S" label))
      ;; Patch the branch offset
      ;; This is simplified - real code would handle different instruction types
      ))

  (nreverse *code-bytes*))

(defun codegen-tac-instr (instr)
  "Generate ARM64 code for a single TAC instruction."
  (match tac-instr instr
    ;; === Data Movement ===
    (tac-lit (dest value)
      (let ((r (vreg-reg dest)))
        (emit-bytes (arm64:movz (reg-keyword r) value))))

    (tac-nil (dest)
      (let ((r (vreg-reg dest)))
        ;; nil = 0 in hybrid scheme
        (emit-bytes (arm64:movz (reg-keyword r) 0))))

    (tac-t (dest)
      (let ((r (vreg-reg dest)))
        ;; t = 3 (symbol tag 2, ptr 0, so 0|2 + odd = 3? or special value)
        (emit-bytes (arm64:movz (reg-keyword r) 3))))

    (tac-move (dest src)
      (let ((rd (vreg-reg dest))
            (rs (vreg-reg src)))
        (unless (= rd rs)
          (emit-bytes (arm64:mov-reg (reg-keyword rd) (reg-keyword rs))))))

    (tac-var (dest offset)
      (let ((r (vreg-reg dest)))
        ;; Load from env (x20) at offset
        (emit-bytes (arm64:ldr-offset (reg-keyword r) :x20 (* offset 8)))))

    (tac-setvar (offset src)
      (let ((rs (vreg-reg src)))
        ;; Store to env (x20) at offset
        (emit-bytes (arm64:str-offset (reg-keyword rs) :x20 (* offset 8)))))

    (tac-global (dest name)
      ;; TODO: global variable lookup
      (let ((r (vreg-reg dest)))
        (emit-bytes (arm64:movz (reg-keyword r) 0))))

    (tac-set-global (name src)
      ;; TODO: global variable store
      )

    ;; === Arithmetic ===
    (tac-add (dest left right)
      (codegen-arith #'arm64:add dest left right))

    (tac-sub (dest left right)
      (codegen-arith #'arm64:sub dest left right))

    (tac-mul (dest left right)
      ;; MUL needs to handle tags
      (let ((rd (vreg-reg dest))
            (rl (vreg-reg left))
            (rr (vreg-reg right)))
        ;; For tagged fixnums: result = ((a >> 1) * (b >> 1)) << 1 | 1
        ;; Simplified: just emit mul for now
        (emit-bytes (arm64:mul (reg-keyword rd)
                               (reg-keyword rl)
                               (reg-keyword rr)))))

    (tac-div (dest left right)
      (let ((rd (vreg-reg dest))
            (rl (vreg-reg left))
            (rr (vreg-reg right)))
        (emit-bytes (arm64:sdiv (reg-keyword rd)
                                (reg-keyword rl)
                                (reg-keyword rr)))))

    (tac-mod (dest left right)
      ;; MOD = a - (a / b) * b
      ;; Use msub: rd = rn - rm * ra
      (let ((rd (vreg-reg dest))
            (rl (vreg-reg left))
            (rr (vreg-reg right)))
        ;; First compute div into temp
        (emit-bytes (arm64:sdiv :x19 (reg-keyword rl) (reg-keyword rr)))
        ;; Then msub: rd = rl - x19 * rr
        (emit-bytes (arm64:msub (reg-keyword rd)
                                :x19
                                (reg-keyword rr)
                                (reg-keyword rl)))))

    (tac-neg (dest value)
      (let ((rd (vreg-reg dest))
            (rv (vreg-reg value)))
        (emit-bytes (arm64:neg (reg-keyword rd) (reg-keyword rv)))))

    ;; === Comparison ===
    (tac-eq (dest left right)
      (codegen-cmp #'arm64:cset-eq dest left right))

    (tac-eql (dest left right)
      (codegen-cmp #'arm64:cset-eq dest left right))

    (tac-lt (dest left right)
      (codegen-cmp #'arm64:cset-lt dest left right))

    (tac-gt (dest left right)
      (codegen-cmp #'arm64:cset-gt dest left right))

    (tac-le (dest left right)
      (codegen-cmp #'arm64:cset-le dest left right))

    (tac-ge (dest left right)
      (codegen-cmp #'arm64:cset-ge dest left right))

    (tac-zerop (dest value)
      (let ((rd (vreg-reg dest))
            (rv (vreg-reg value)))
        ;; Compare with 0, set if equal
        (emit-bytes (arm64:cmp (reg-keyword rv) 0))
        (emit-bytes (arm64:cset-eq (reg-keyword rd)))))

    ;; === Logical ===
    (tac-not (dest value)
      (let ((rd (vreg-reg dest))
            (rv (vreg-reg value)))
        ;; Logical not: if value is nil (0), return t (non-zero)
        (emit-bytes (arm64:cmp (reg-keyword rv) 0))
        (emit-bytes (arm64:cset-eq (reg-keyword rd)))))

    ;; === Bitwise ===
    (tac-band (dest left right)
      (codegen-arith #'arm64:and-reg dest left right))

    (tac-bor (dest left right)
      (codegen-arith #'arm64:orr dest left right))

    (tac-bxor (dest left right)
      (codegen-arith #'arm64:eor dest left right))

    (tac-bsh (dest value shift)
      ;; Shift - positive = left, negative = right
      ;; For simplicity, use LSL for now (needs runtime dispatch for sign)
      (let ((rd (vreg-reg dest))
            (rv (vreg-reg value))
            (rs (vreg-reg shift)))
        (emit-bytes (arm64:lsl-reg (reg-keyword rd)
                                   (reg-keyword rv)
                                   (reg-keyword rs)))))

    (tac-bnot (dest value)
      (let ((rd (vreg-reg dest))
            (rv (vreg-reg value)))
        (emit-bytes (arm64:mvn (reg-keyword rd) (reg-keyword rv)))))

    ;; === Control Flow ===
    (tac-label (name)
      ;; Record label position
      (setf (gethash name *label-offsets*) (current-offset)))

    (tac-goto (target)
      ;; Unconditional branch
      (let ((target-offset (gethash target *label-offsets*)))
        (if target-offset
            (let ((rel-offset (- target-offset (current-offset))))
              (emit-bytes (arm64:b rel-offset)))
            (progn
              (push (cons (current-offset) target) *fixups*)
              (emit-bytes (arm64:b 0))))))  ; placeholder

    (tac-if (cond then-label)
      (let ((rc (vreg-reg cond)))
        ;; Compare with nil (0), branch if not equal (true)
        (emit-bytes (arm64:cmp (reg-keyword rc) 0))
        (let ((target-offset (gethash then-label *label-offsets*)))
          (if target-offset
              (let ((rel-offset (- target-offset (current-offset))))
                (emit-bytes (arm64:b-ne rel-offset)))
              (progn
                (push (cons (current-offset) then-label) *fixups*)
                (emit-bytes (arm64:b-ne 0)))))))

    (tac-ifnot (cond else-label)
      (let ((rc (vreg-reg cond)))
        ;; Compare with nil (0), branch if equal (false)
        (emit-bytes (arm64:cmp (reg-keyword rc) 0))
        (let ((target-offset (gethash else-label *label-offsets*)))
          (if target-offset
              (let ((rel-offset (- target-offset (current-offset))))
                (emit-bytes (arm64:b-eq rel-offset)))
              (progn
                (push (cons (current-offset) else-label) *fixups*)
                (emit-bytes (arm64:b-eq 0)))))))

    (tac-return (value)
      (let ((rv (vreg-reg value)))
        ;; Move result to x0 if not already there
        (unless (= rv 0)
          (emit-bytes (arm64:mov-reg :x0 (reg-keyword rv))))
        ;; Emit return sequence
        (emit-bytes (arm64:ret))))

    ;; === Function Calls ===
    (tac-param (dest index)
      ;; Load parameter from x0-x7
      (let ((rd (vreg-reg dest)))
        (unless (= rd index)
          (emit-bytes (arm64:mov-reg (reg-keyword rd)
                                     (reg-keyword index))))))

    (tac-arg (index src)
      ;; Set argument register
      (let ((rs (vreg-reg src)))
        (unless (= rs index)
          (emit-bytes (arm64:mov-reg (reg-keyword index)
                                     (reg-keyword rs))))))

    (tac-call (dest name nargs)
      ;; Named function call
      ;; For now, just emit a BL placeholder
      (emit-bytes (arm64:bl 0))  ; TODO: resolve function address
      (let ((rd (vreg-reg dest)))
        (unless (= rd 0)
          (emit-bytes (arm64:mov-reg (reg-keyword rd) :x0)))))

    (tac-funcall (dest fn nargs)
      ;; Indirect call through register
      (let ((rf (vreg-reg fn)))
        (emit-bytes (arm64:blr (reg-keyword rf))))
      (let ((rd (vreg-reg dest)))
        (unless (= rd 0)
          (emit-bytes (arm64:mov-reg (reg-keyword rd) :x0)))))

    ;; === List Operations ===
    (tac-cons (dest car cdr)
      ;; Call runtime cons
      ;; For now, placeholder
      (let ((rd (vreg-reg dest))
            (ra (vreg-reg car))
            (rb (vreg-reg cdr)))
        ;; Move args to x0, x1
        (emit-bytes (arm64:mov-reg :x0 (reg-keyword ra)))
        (emit-bytes (arm64:mov-reg :x1 (reg-keyword rb)))
        ;; TODO: call cons runtime function
        (emit-bytes (arm64:bl 0))
        (unless (= rd 0)
          (emit-bytes (arm64:mov-reg (reg-keyword rd) :x0)))))

    (tac-car (dest cell)
      (let ((rd (vreg-reg dest))
            (rc (vreg-reg cell)))
        ;; car: clear tag bits, load word
        ;; Simplified: assume already untagged
        (emit-bytes (arm64:ldr-offset (reg-keyword rd)
                                      (reg-keyword rc)
                                      0))))

    (tac-cdr (dest cell)
      (let ((rd (vreg-reg dest))
            (rc (vreg-reg cell)))
        ;; cdr: offset 8 from cons cell
        (emit-bytes (arm64:ldr-offset (reg-keyword rd)
                                      (reg-keyword rc)
                                      8))))

    (tac-list (dest elems)
      ;; Build list from elements - call runtime
      (let ((rd (vreg-reg dest)))
        ;; TODO: implement list construction
        (emit-bytes (arm64:movz (reg-keyword rd) 0))))

    ;; === Type Predicates ===
    (tac-null (dest value)
      (let ((rd (vreg-reg dest))
            (rv (vreg-reg value)))
        ;; null check: compare with 0
        (emit-bytes (arm64:cmp (reg-keyword rv) 0))
        (emit-bytes (arm64:cset-eq (reg-keyword rd)))))

    (tac-consp (dest value)
      (codegen-type-pred dest value 0))  ; cons tag = 0

    (tac-symbolp (dest value)
      (codegen-type-pred dest value 2))  ; symbol tag = 2

    (tac-stringp (dest value)
      (codegen-type-pred dest value 6))  ; string tag = 6

    (tac-numberp (dest value)
      (let ((rd (vreg-reg dest))
            (rv (vreg-reg value)))
        ;; Fixnum: bit 0 = 1
        (emit-bytes (arm64:and-imm (reg-keyword rd) (reg-keyword rv) 1))
        ;; rd now contains 1 if fixnum, 0 otherwise
        ))

    (tac-keywordp (dest value)
      (codegen-type-pred dest value 10))  ; keyword tag = 10

    (tac-functionp (dest value)
      (codegen-type-pred dest value 8))   ; closure tag = 8

    ;; === String Operations ===
    (tac-string-length (dest str)
      (let ((rd (vreg-reg dest))
            (rs (vreg-reg str)))
        ;; String layout: [tag|len] [chars...]
        ;; Length is at offset 0, high bits
        ;; TODO: implement properly
        (emit-bytes (arm64:ldr-offset (reg-keyword rd) (reg-keyword rs) 0))))

    (tac-string-ref (dest str index)
      (let ((rd (vreg-reg dest))
            (rs (vreg-reg str))
            (ri (vreg-reg index)))
        ;; TODO: implement string-ref with proper bounds checking
        ;; Load byte at str + 8 + index (after header)
        (emit-bytes (arm64:add (reg-keyword rd) (reg-keyword rs) (reg-keyword ri)))
        (emit-bytes (arm64:ldrb (reg-keyword rd) (reg-keyword rd) 8))))

    (tac-string-concat (dest left right)
      ;; TODO: implement string concatenation
      (let ((rd (vreg-reg dest)))
        (emit-bytes (arm64:movz (reg-keyword rd) 0))))

    (tac-string-lit (dest string)
      ;; TODO: load string literal address
      (let ((rd (vreg-reg dest)))
        (emit-bytes (arm64:movz (reg-keyword rd) 0))))

    ;; === Vector Operations ===
    (tac-make-vector (dest size init)
      ;; TODO: implement
      (let ((rd (vreg-reg dest)))
        (emit-bytes (arm64:movz (reg-keyword rd) 0))))

    (tac-vector-ref (dest vec index)
      (let ((rd (vreg-reg dest))
            (rv (vreg-reg vec))
            (ri (vreg-reg index)))
        ;; Vector ref: load at vec + 8 + index * 8
        ;; TODO: proper implementation
        (emit-bytes (arm64:ldr-offset (reg-keyword rd) (reg-keyword rv) 0))))

    (tac-vector-set (vec index value)
      ;; TODO: implement
      )

    (tac-vector-length (dest vec)
      (let ((rd (vreg-reg dest))
            (rv (vreg-reg vec)))
        ;; TODO: implement
        (emit-bytes (arm64:ldr-offset (reg-keyword rd) (reg-keyword rv) 0))))

    ;; === Symbol Operations ===
    (tac-make-symbol (dest name)
      ;; TODO: implement
      (let ((rd (vreg-reg dest)))
        (emit-bytes (arm64:movz (reg-keyword rd) 0))))

    (tac-symbol-name (dest sym)
      (let ((rd (vreg-reg dest))
            (rs (vreg-reg sym)))
        ;; Symbol name is at offset 8
        (emit-bytes (arm64:ldr-offset (reg-keyword rd) (reg-keyword rs) 8))))

    (tac-intern (dest str)
      ;; TODO: implement
      (let ((rd (vreg-reg dest)))
        (emit-bytes (arm64:movz (reg-keyword rd) 0))))

    (tac-symbol-lit (dest name)
      ;; TODO: load symbol literal address
      (let ((rd (vreg-reg dest)))
        (emit-bytes (arm64:movz (reg-keyword rd) 0))))

    ;; === Keyword Operations ===
    (tac-keyword-name (dest kw)
      (let ((rd (vreg-reg dest))
            (rk (vreg-reg kw)))
        ;; Keyword name is at offset 8, convert to string by XOR 12
        (emit-bytes (arm64:ldr-offset (reg-keyword rd) (reg-keyword rk) 8))
        (emit-bytes (arm64:eor-imm (reg-keyword rd) (reg-keyword rd) 12))))

    (tac-keyword-lit (dest name)
      ;; TODO: load keyword literal address
      (let ((rd (vreg-reg dest)))
        (emit-bytes (arm64:movz (reg-keyword rd) 0))))

    ;; === System ===
    (tac-exit (code)
      (let ((rc (vreg-reg code)))
        ;; Move exit code to x0, call exit
        (emit-bytes (arm64:mov-reg :x0 (reg-keyword rc)))
        ;; TODO: call _exit
        (emit-bytes (arm64:bl 0))))

    (tac-error (message)
      ;; TODO: implement error
      )))

;;; Helper functions

(defun codegen-arith (op-fn dest left right)
  "Generate code for arithmetic binary operation."
  (let ((rd (vreg-reg dest))
        (rl (vreg-reg left))
        (rr (vreg-reg right)))
    (emit-bytes (funcall op-fn (reg-keyword rd)
                         (reg-keyword rl)
                         (reg-keyword rr)))))

(defun codegen-cmp (cset-fn dest left right)
  "Generate code for comparison operation."
  (let ((rd (vreg-reg dest))
        (rl (vreg-reg left))
        (rr (vreg-reg right)))
    (emit-bytes (arm64:cmp (reg-keyword rl) (reg-keyword rr)))
    (emit-bytes (funcall cset-fn (reg-keyword rd)))))

(defun codegen-type-pred (dest value expected-tag)
  "Generate code for type predicate."
  (let ((rd (vreg-reg dest))
        (rv (vreg-reg value)))
    ;; Extract tag (bottom 4 bits for pointers)
    (emit-bytes (arm64:and-imm :x19 (reg-keyword rv) 15))
    ;; Compare with expected tag
    (emit-bytes (arm64:cmp :x19 expected-tag))
    ;; Set result based on comparison
    (emit-bytes (arm64:cset-eq (reg-keyword rd)))))
