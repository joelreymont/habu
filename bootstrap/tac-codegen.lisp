;;;; TAC Codegen - Generate ARM64 from TAC
;;;;
;;;; Input: list of tac-instr + allocation-result
;;;; Output: list of ARM64 machine code bytes
;;;;
;;;; Uses arm64/asm.lisp encoders.

(defpackage :habu.codegen
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:import-from :habu.tac :tac-instr :tac-literal
                :lit-fixnum :lit-fixnum-p :lit-fixnum-value
                :lit-raw :lit-raw-p :lit-raw-value)
  (:import-from :habu.regalloc :allocation-result
                :allocation-result-vreg-to-reg
                :allocation-result-spills
                :allocation-result-stack-size)
  (:export :generate-code :codegen-function))

(in-package :habu.codegen)

;;; Load ARM64 encoders if not already loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :arm64)
    (load "arm64/asm.lisp")))

;;; Hybrid 1+3 bit tag constants (from shared/tags.lisp)
(defconstant +nil-value+ 0)
(defconstant +t-value+ 3)
(defconstant +fixnum-bit+ 1)
(defconstant +tag-mask+ 15)
(defconstant +ptr-mask+ -16)
(defconstant +tag-cons+ 0)
(defconstant +tag-symbol+ 2)
(defconstant +tag-vector+ 4)
(defconstant +tag-string+ 6)
(defconstant +tag-closure+ 8)
(defconstant +tag-keyword+ 10)

;;; Code generation state
(defvar *code* nil "List of instruction bytes (reversed)")
(defvar *byte-offset* 0 "Current byte offset in code stream")
(defvar *vreg-to-reg* nil "Hash table from vreg to physical register")
(defvar *spill-slots* nil "Hash table from spilled vreg to stack slot index")
(defvar *stack-size* 0 "Number of stack slots for spills")
(defvar *labels* nil "Hash table from label -> byte offset")
(defvar *fixups* nil "List of (offset label type) for forward refs")
(defvar *markers* nil "List of (offset marker-data) for linker resolution")
(defvar *local-functions* nil "List of function names in current compilation unit")
(defvar *extra-args-allocated* 0 "Number of extra arg slots allocated on stack (for args >= 8)")

;; Spill temp registers - use x8 (not used by habu calling convention)
(defconstant +spill-temp+ :x8)
(defconstant +spill-temp2+ :x17)  ; Second spill temp for binary ops

(defun reset-codegen ()
  (setf *code* nil)
  (setf *byte-offset* 0)
  (setf *labels* (make-hash-table :test 'equal))
  (setf *fixups* nil)
  (setf *markers* nil)
  (setf *spill-slots* nil)
  (setf *stack-size* 0)
  (setf *extra-args-allocated* 0))

(defun emit (&rest items)
  "Emit bytes to code stream. ARM64 functions return byte lists.
   Markers (:call-fn name) become 4 placeholder bytes + entry in *markers*."
  (dolist (item items)
    (cond
      ;; Marker - store in *markers* and emit 4 placeholder bytes
      ((and (consp item) (keywordp (car item)))
       (push (list *byte-offset* item) *markers*)
       ;; Emit 4 placeholder bytes (will be patched by linker)
       (push #xDE *code*) (push #xAD *code*)
       (push #xBE *code*) (push #xEF *code*)
       (incf *byte-offset* 4))
      ;; Regular byte list from ARM64 encoder
      ((listp item)
       (dolist (byte item)
         (push byte *code*)
         (incf *byte-offset*)))
      ;; Single byte
      ((integerp item)
       (push item *code*)
       (incf *byte-offset*))
      (t (error "emit: invalid item ~S" item)))))

(defun current-offset ()
  "Return current byte offset in code stream."
  *byte-offset*)

(defun is-spilled (vreg)
  "Check if vreg is spilled."
  (eq (gethash vreg *vreg-to-reg*) :spill))

(defun spill-offset (vreg)
  "Get stack offset for spilled vreg. Returns byte offset from sp.
   Spill slots start at sp+0x40 (after saved registers in habu frame)."
  (+ #x40 (* (gethash vreg *spill-slots*) 8)))

(defun vreg->reg (vreg)
  "Convert vreg to physical register keyword. Errors on spilled vregs."
  (let ((reg (gethash vreg *vreg-to-reg*)))
    (cond
      ((null reg) (error "vreg ~D not allocated" vreg))
      ((eq reg :spill) (error "vreg ~D spilled - use load-vreg/store-vreg" vreg))
      (t (arm64:num-to-reg reg)))))

(defun vreg->reg-or-temp (vreg &optional (temp +spill-temp+))
  "Get physical reg for vreg, or temp reg for spilled vreg (after loading).
   For spilled vregs, emits load from stack to temp."
  (if (is-spilled vreg)
      (progn
        (emit (arm64:ldr temp :sp :offset (spill-offset vreg)))
        temp)
      (vreg->reg vreg)))

(defun store-if-spilled (vreg &optional (temp +spill-temp+))
  "If vreg is spilled, emit store from temp to stack slot."
  (when (is-spilled vreg)
    (emit (arm64:str temp :sp :offset (spill-offset vreg)))))

(defun dest-reg (vreg)
  "Get register to use as destination for vreg.
   For non-spilled: the allocated register.
   For spilled: the spill temp (caller must store-if-spilled after)."
  (if (is-spilled vreg)
      +spill-temp+
      (vreg->reg vreg)))

(defun load-imm (rd value)
  "Load a 64-bit immediate value into register.
   Handles negative numbers using MOVN, positive using MOVZ/MOVK."
  (if (< value 0)
      ;; Negative: use MOVN with bitwise NOT of value
      ;; MOVN sets rd = ~(imm << lsl)
      ;; For values -1 to -65536, ~value fits in 16 bits
      (let ((inv (lognot value)))
        (if (<= inv #xFFFF)
            (arm64:movn rd inv)
            ;; Larger negative: use MOVN + MOVK sequence
            (let ((lo16 (logand inv #xFFFF))
                  (hi16 (logand (ash inv -16) #xFFFF)))
              (if (<= hi16 #xFFFF)
                  (append (arm64:movn rd #xFFFF :lsl 48)  ; Start with all 1s
                          (arm64:movk rd (logand value #xFFFF) :lsl 0)
                          (arm64:movk rd (logand (ash value -16) #xFFFF) :lsl 16)
                          (arm64:movk rd (logand (ash value -32) #xFFFF) :lsl 32))
                  ;; Very large negative - full sequence
                  (append (arm64:movn rd (logand (lognot (ash value -48)) #xFFFF) :lsl 48)
                          (arm64:movk rd (logand value #xFFFF) :lsl 0)
                          (arm64:movk rd (logand (ash value -16) #xFFFF) :lsl 16)
                          (arm64:movk rd (logand (ash value -32) #xFFFF) :lsl 32))))))
      ;; Positive: use MOVZ + MOVK
      (if (<= value #xFFFF)
          (arm64:movz rd value)
          (if (<= value #xFFFFFFFF)
              (append (arm64:movz rd (logand value #xFFFF))
                      (arm64:movk rd (logand (ash value -16) #xFFFF) :lsl 16))
              ;; Larger: need more MOVK
              (append (arm64:movz rd (logand value #xFFFF))
                      (arm64:movk rd (logand (ash value -16) #xFFFF) :lsl 16)
                      (arm64:movk rd (logand (ash value -32) #xFFFF) :lsl 32)
                      (arm64:movk rd (logand (ash value -48) #xFFFF) :lsl 48))))))

;;; Main code generation

(defun generate-code (tac-instrs alloc)
  "Generate ARM64 code from TAC with register allocation.
   Returns: list of bytes"
  (reset-codegen)
  (setf *vreg-to-reg* (allocation-result-vreg-to-reg alloc))

  ;; Generate code - labels record their position during this pass
  (dolist (instr tac-instrs)
    (codegen-instr instr))

  ;; Reverse to get correct order, then apply fixups, then return
  (setf *code* (nreverse *code*))
  (apply-fixups)
  *code*)

(defun codegen-instr (instr)
  "Generate ARM64 for a single TAC instruction."
  (match tac-instr instr
    ;; === Data Movement ===
    (lit (dest literal)
      ;; Type-safe literal handling via tac-literal ADT
      (let ((rd (dest-reg dest)))
        (match literal
          (fixnum (value)
            ;; Lisp fixnum → tag it: (value << 1) | 1
            (emit (load-imm rd (logior (ash value 1) 1))))
          (raw (value)
            ;; Raw integer → use as-is (for internal constants)
            (emit (load-imm rd value))))
        (store-if-spilled dest)))

    (nil (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (t (dest)
      ;; t = pointer to T symbol, use small constant for now
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 3))
        (store-if-spilled dest)))

    (move (dest src)
      (let ((rd (dest-reg dest))
            (rs (vreg->reg-or-temp src +spill-temp2+)))
        (unless (eq rd rs)
          (emit (arm64:mov rd rs)))
        (store-if-spilled dest)))

    (var (dest offset)
      ;; Load from env (x20) at offset
      (let ((rd (dest-reg dest)))
        (emit (arm64:ldr rd :x20 :offset (* offset 8)))
        (store-if-spilled dest)))

    (setvar (offset src)
      (let ((rs (vreg->reg-or-temp src)))
        (emit (arm64:str rs :x20 :offset (* offset 8)))))

    (global (dest name)
      (declare (ignore name))
      ;; TODO: global variable lookup
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (set-global (name src)
      (declare (ignore name src))
      ;; TODO: global variable store
      )

    ;; === Arithmetic ===
    ;; Hybrid tagging: fixnum = (value << 1) | 1
    ;; (2a+1) + (2b+1) = 2(a+b) + 2, but want 2(a+b) + 1, so subtract 1
    (add (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:add rd rl rr))
        (emit (arm64:sub rd rd 1 :imm t))
        (store-if-spilled dest)))

    ;; (2a+1) - (2b+1) = 2(a-b), but want 2(a-b) + 1, so add 1
    (sub (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:sub rd rl rr))
        (emit (arm64:add rd rd 1 :imm t))
        (store-if-spilled dest)))

    (mul (dest left right)
      ;; Untag both operands, multiply, re-tag result
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:asr :x9 rl 1 :imm t))    ; untag left → x9
        (emit (arm64:asr :x10 rr 1 :imm t))   ; untag right → x10
        (emit (arm64:mul rd :x9 :x10))        ; multiply
        (emit (arm64:lsl rd rd 1 :imm t))     ; shift left
        (emit (arm64:orr rd rd 1 :imm t))     ; set tag bit
        (store-if-spilled dest)))

    (div (dest left right)
      ;; Untag both operands, divide, re-tag result
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:asr :x9 rl 1 :imm t))    ; untag left → x9
        (emit (arm64:asr :x10 rr 1 :imm t))   ; untag right → x10
        (emit (arm64:sdiv rd :x9 :x10))       ; signed divide
        (emit (arm64:lsl rd rd 1 :imm t))     ; shift left
        (emit (arm64:orr rd rd 1 :imm t))     ; set tag bit
        (store-if-spilled dest)))

    (mod (dest left right)
      ;; Untag both, compute mod = a - (a/b)*b, retag
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:asr :x9 rl 1 :imm t))    ; untag left → x9
        (emit (arm64:asr :x10 rr 1 :imm t))   ; untag right → x10
        (emit (arm64:sdiv :x11 :x9 :x10))     ; div
        (emit (arm64:msub rd :x11 :x10 :x9))  ; dest = x9 - x11*x10
        (emit (arm64:lsl rd rd 1 :imm t))     ; shift left
        (emit (arm64:orr rd rd 1 :imm t))     ; set tag bit
        (store-if-spilled dest)))

    (neg (dest value)
      ;; neg(2a+1) = -(2a+1) = -2a-1, but want 2(-a)+1 = -2a+1
      ;; So negate and add 2
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        (emit (arm64:neg rd rv))
        (emit (arm64:add rd rd 2 :imm t))
        (store-if-spilled dest)))

    ;; === Comparison ===
    (eq (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:cmp rl rr))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        (store-if-spilled dest)))

    (eql (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:cmp rl rr))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        (store-if-spilled dest)))

    (lt (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:cmp rl rr))
        (emit (arm64:cset rd #.arm64:+cc-lt+))
        (store-if-spilled dest)))

    (gt (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:cmp rl rr))
        (emit (arm64:cset rd #.arm64:+cc-gt+))
        (store-if-spilled dest)))

    (le (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:cmp rl rr))
        (emit (arm64:cset rd #.arm64:+cc-le+))
        (store-if-spilled dest)))

    (ge (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:cmp rl rr))
        (emit (arm64:cset rd #.arm64:+cc-ge+))
        (store-if-spilled dest)))

    (zerop (dest value)
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        (emit (arm64:cmp rv 0 :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        (store-if-spilled dest)))

    ;; === Logical ===
    (not (dest value)
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        (emit (arm64:cmp rv 0 :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        (store-if-spilled dest)))

    ;; === Bitwise ===
    (band (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:and* rd rl rr))
        (store-if-spilled dest)))

    (bor (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:orr rd rl rr))
        (store-if-spilled dest)))

    (bxor (dest left right)
      (let ((rd (dest-reg dest))
            (rl (vreg->reg-or-temp left +spill-temp2+))
            (rr (vreg->reg-or-temp right +spill-temp+)))
        (emit (arm64:eor rd rl rr))
        (store-if-spilled dest)))

    (bsh (dest value shift)
      ;; Positive = left shift, negative = right shift
      ;; For now, assume left shift
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value +spill-temp2+))
            (rs (vreg->reg-or-temp shift +spill-temp+)))
        (emit (arm64:lsl rd rv rs))
        (store-if-spilled dest)))

    (bnot (dest value)
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        (emit (arm64:mvn rd rv))
        (store-if-spilled dest)))

    ;; === Control Flow ===
    (label (name)
      (setf (gethash name *labels*) (current-offset)))

    (goto (target)
      (let ((target-offset (gethash target *labels*)))
        (if target-offset
            (emit (arm64:b (ash (- target-offset (current-offset)) -2)))
            (progn
              (push (list (current-offset) target :b) *fixups*)
              (emit (arm64:b 0))))))

    (if (cond then-label)
      (let ((rc (vreg->reg-or-temp cond)))
        (emit (arm64:cmp rc 0 :imm t))
        (let ((target-offset (gethash then-label *labels*)))
          (if target-offset
              (emit (arm64:b.ne (ash (- target-offset (current-offset)) -2)))
              (progn
                (push (list (current-offset) then-label :b.ne) *fixups*)
                (emit (arm64:b.ne 0)))))))

    (ifnot (cond else-label)
      (let ((rc (vreg->reg-or-temp cond)))
        (emit (arm64:cmp rc 0 :imm t))
        (let ((target-offset (gethash else-label *labels*)))
          (if target-offset
              (emit (arm64:b.eq (ash (- target-offset (current-offset)) -2)))
              (progn
                (push (list (current-offset) else-label :b.eq) *fixups*)
                (emit (arm64:b.eq 0)))))))

    (return (value)
      ;; Move result to x0
      (let ((rv (vreg->reg-or-temp value)))
        (unless (eq rv :x0)
          (emit (arm64:mov :x0 rv))))
      ;; Use fixed epilogue for habu calling convention
      (emit (fn-fixed-epilogue)))

    ;; === Function Calls ===
    (param (dest index)
      ;; Load parameter from x0-x7
      (let ((rd (dest-reg dest))
            (param-reg (arm64:num-to-reg index)))
        (emit (arm64:mov rd param-reg))
        (store-if-spilled dest)))

    (arg (index src)
      ;; x0-x7 for first 8 args, stack for args >= 8
      ;; Extra args stored temporarily at sp+0x100+offset, copied to call stack in :call
      (let ((rs (vreg->reg-or-temp src)))
        (if (< index 8)
            ;; Register arg
            (let ((arg-reg (arm64:num-to-reg index)))
              (unless (eq arg-reg rs)
                (emit (arm64:mov arg-reg rs))))
            ;; Stack arg - store in temp area, will be copied in :call
            (let ((stack-index (- index 8)))
              ;; Track max extra args needed
              (when (>= stack-index *extra-args-allocated*)
                (setf *extra-args-allocated* (1+ stack-index)))
              ;; Store at temp location: sp+0x100+(stack_index*8)
              (emit (arm64:str rs :sp :offset (+ #x100 (* stack-index 8))))))))

    (call (dest name nargs)
      (declare (ignore nargs))
      ;; Remember extra args count for this call
      (let ((extra-args *extra-args-allocated*)
            (extra-args-space 0))
        ;; Save caller-saved registers x9-x15 before call (56 bytes, 8-aligned)
        ;; sub sp, sp, #64
        (emit (arm64:sub :sp :sp 64 :imm t))
        (emit (arm64:stp :x9 :x10 :sp :offset 0))
        (emit (arm64:stp :x11 :x12 :sp :offset 16))
        (emit (arm64:stp :x13 :x14 :sp :offset 32))
        (emit (arm64:str :x15 :sp :offset 48))

        ;; Allocate stack space for extra args and copy from temp area
        (when (> extra-args 0)
          ;; Round up to even for 16-byte alignment
          (setf extra-args-space (* (if (oddp extra-args) (1+ extra-args) extra-args) 8))
          (emit (arm64:sub :sp :sp extra-args-space :imm t))
          ;; Copy args from temp area (sp+64+0x100+i*8) to call stack (sp+i*8)
          (dotimes (i extra-args)
            (emit (arm64:ldr +spill-temp+ :sp :offset (+ 64 #x100 (* i 8))))
            (emit (arm64:str +spill-temp+ :sp :offset (* i 8)))))

        ;; BL to function - check if label is defined (local function)
        ;; or emit :call-fn marker for linker resolution
        (let ((target-offset (gethash name *labels*)))
          (cond
            ;; Label already defined - calculate offset
            (target-offset
             (let ((rel-instrs (ash (- target-offset (current-offset)) -2)))
               (emit (arm64:bl rel-instrs))))
            ;; Check if it's a known local function (forward ref within same code)
            ((member name *local-functions*)
             (push (list (current-offset) name :bl) *fixups*)
             (emit (arm64:bl 0)))
            ;; External function - emit marker for linker
            (t
             (emit (list :call-fn name)))))

        ;; Save result to x8 before restoring (x8 is not in our save set)
        (emit (arm64:mov :x8 :x0))

        ;; Deallocate extra args stack space
        (when (> extra-args-space 0)
          (emit (arm64:add :sp :sp extra-args-space :imm t)))

        ;; Restore caller-saved registers
        (emit (arm64:ldr :x15 :sp :offset 48))
        (emit (arm64:ldp :x13 :x14 :sp :offset 32))
        (emit (arm64:ldp :x11 :x12 :sp :offset 16))
        (emit (arm64:ldp :x9 :x10 :sp :offset 0))
        (emit (arm64:add :sp :sp 64 :imm t))

        ;; Reset extra args count for next call
        (setf *extra-args-allocated* 0)

        ;; Now move result to destination
        (let ((rd (dest-reg dest)))
          (emit (arm64:mov rd :x8))
          (store-if-spilled dest))))

    (funcall (dest fn nargs)
      (declare (ignore nargs))
      ;; Move closure/function pointer to x24 if not already there
      (let ((fn-reg (vreg->reg-or-temp fn)))
        (unless (eq fn-reg :x24)
          (emit (arm64:mov :x24 fn-reg))))
      ;; Call through closure - BLR x24
      ;; x24 holds closure pointer per habu calling convention
      (emit (arm64:blr :x24))
      ;; Move result from x0 to dest if needed
      (let ((rd (dest-reg dest)))
        (emit (arm64:mov rd :x0))
        (store-if-spilled dest)))

    ;; === List Operations ===
    (cons (dest car-vreg cdr-vreg)
      ;; Inline heap allocation: x28 = alloc ptr, x27 = heap base
      ;; Store car at x28+0, cdr at x28+8
      (let ((rcar (vreg->reg-or-temp car-vreg +spill-temp2+))
            (rcdr (vreg->reg-or-temp cdr-vreg +spill-temp+)))
        (emit (arm64:str rcar :x28 :offset 0))
        (emit (arm64:str rcdr :x28 :offset 8)))
      ;; Result = x28 (already tagged with 0 = cons tag)
      (let ((rd (dest-reg dest)))
        (emit (arm64:mov rd :x28))
        (store-if-spilled dest))
      ;; Bump allocator: x28 += 16
      (emit (arm64:add :x28 :x28 16 :imm t)))

    (car (dest cell)
      ;; Untag pointer: clear low 4 bits (tag mask = -16)
      (let ((rd (dest-reg dest))
            (rc (vreg->reg-or-temp cell)))
        (emit (arm64:and* :x19 rc -16 :imm t))
        ;; Load car from offset 0
        (emit (arm64:ldr rd :x19 :offset 0))
        (store-if-spilled dest)))

    (cdr (dest cell)
      ;; Untag pointer: clear low 4 bits (tag mask = -16)
      (let ((rd (dest-reg dest))
            (rc (vreg->reg-or-temp cell)))
        (emit (arm64:and* :x19 rc -16 :imm t))
        ;; Load cdr from offset 8
        (emit (arm64:ldr rd :x19 :offset 8))
        (store-if-spilled dest)))

    (list (dest elems)
      (declare (ignore elems))
      ;; TODO: build list
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    ;; === Type Predicates ===
    ;; All predicates return t-value (3) for true, nil-value (0) for false
    (null (dest value)
      ;; nil = 0, so if value == 0 then t (3) else nil (0)
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        (emit (arm64:cmp rv #.+nil-value+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; Convert 1 -> 3 (t-value), 0 -> 0 (nil-value): rd = rd * 3
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))
        (store-if-spilled dest)))

    (consp (dest value)
      ;; Cons: tag == 0 AND value != 0 (to exclude nil which is also 0)
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        ;; Check value != 0
        (emit (arm64:cmp rv #.+nil-value+ :imm t))
        (emit (arm64:cset :x19 #.arm64:+cc-ne+))
        ;; Check tag == 0
        (emit (arm64:and* rd rv #.+tag-mask+ :imm t))
        (emit (arm64:cmp rd #.+tag-cons+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; AND the two conditions
        (emit (arm64:and* rd rd :x19))
        ;; Convert 1 -> 3, 0 -> 0
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))
        (store-if-spilled dest)))

    (symbolp (dest value)
      ;; Symbol: tag == 2
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        (emit (arm64:and* rd rv #.+tag-mask+ :imm t))
        (emit (arm64:cmp rd #.+tag-symbol+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; Convert 1 -> 3, 0 -> 0
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))
        (store-if-spilled dest)))

    (stringp (dest value)
      ;; String: tag == 6
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        (emit (arm64:and* rd rv #.+tag-mask+ :imm t))
        (emit (arm64:cmp rd #.+tag-string+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; Convert 1 -> 3, 0 -> 0
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))
        (store-if-spilled dest)))

    (numberp (dest value)
      ;; Fixnum: bit 0 = 1
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        (emit (arm64:and* rd rv #.+fixnum-bit+ :imm t))
        ;; Result is 1 or 0, convert 1 -> 3
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))
        (store-if-spilled dest)))

    (keywordp (dest value)
      ;; Keyword: tag == 10
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        (emit (arm64:and* rd rv #.+tag-mask+ :imm t))
        (emit (arm64:cmp rd #.+tag-keyword+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; Convert 1 -> 3, 0 -> 0
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))
        (store-if-spilled dest)))

    (functionp (dest value)
      ;; Closure: tag == 8
      (let ((rd (dest-reg dest))
            (rv (vreg->reg-or-temp value)))
        (emit (arm64:and* rd rv #.+tag-mask+ :imm t))
        (emit (arm64:cmp rd #.+tag-closure+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; Convert 1 -> 3, 0 -> 0
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))
        (store-if-spilled dest)))

    ;; === String Operations ===
    (string-length (dest str)
      ;; Return length as tagged fixnum
      ;; Length is stored UNTAGGED at str[0] per +string-length-repr+
      (let ((rd (dest-reg dest))
            (rs (vreg->reg-or-temp str)))
        (emit (arm64:and* :x19 rs -16 :imm t))    ; x19 = untagged string base
        (emit (arm64:ldr :x0 :x19))               ; x0 = UNTAGGED length
        ;; Re-tag as fixnum: (val << 1) | 1
        (emit (arm64:lsl rd :x0 1 :imm t))        ; rd = length << 1
        (emit (arm64:orr rd rd 1 :imm t))         ; rd = tagged fixnum
        (store-if-spilled dest)))

    (string-ref (dest str index)
      ;; Load byte at str + 8 + index
      (let ((rd (dest-reg dest))
            (rs (vreg->reg-or-temp str +spill-temp2+))
            (ri (vreg->reg-or-temp index +spill-temp+)))
        (emit (arm64:and* :x19 rs -16 :imm t))
        (emit (arm64:add :x19 :x19 8 :imm t))
        (emit (arm64:ldrb rd :x19 ri :reg t))
        (store-if-spilled dest)))

    (string-concat (dest left right)
      (declare (ignore left right))
      ;; TODO: implement
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (string-lit (dest string)
      (declare (ignore string))
      ;; TODO: load string literal address
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    ;; === Vector Operations ===
    (make-vector (dest size init)
      ;; Allocate vector with given size
      ;; Size is tagged fixnum, each element is 8 bytes
      ;; Store UNTAGGED length at [heap+0] per +vector-length-repr+ convention
      (declare (ignore init))
      (let ((rs (vreg->reg-or-temp size))
            (rd (dest-reg dest)))
        ;; x0 = tagged size, x1 = untagged for storage and alloc calc
        (emit (arm64:mov :x0 rs))
        (emit (arm64:asr :x1 :x0 1 :imm t))       ; x1 = untagged length
        (emit (arm64:str :x1 :x28 :offset 0))     ; [heap+0] = UNTAGGED length
        ;; Calculate alloc size: 8 + length*8, rounded to 16
        (emit (arm64:lsl :x1 :x1 3 :imm t))       ; x1 = length * 8
        (emit (arm64:add :x1 :x1 8 :imm t))       ; x1 = 8 + data_size
        (emit (arm64:add :x1 :x1 15 :imm t))      ; x1 += 15
        (emit (arm64:and* :x1 :x1 -16 :imm t))    ; x1 &= ~15 (align to 16)
        ;; x0 = tagged pointer, bump heap
        (emit (arm64:mov :x0 :x28))
        (emit (arm64:add :x28 :x28 :x1))
        ;; Tag with vector tag (4)
        (emit (arm64:movz :x1 4))
        (emit (arm64:orr rd :x0 :x1))
        (store-if-spilled dest)))

    (vector-ref (dest vec index)
      ;; Load vec[index]
      ;; vec: tagged vector, index: tagged fixnum
      (let ((rv (vreg->reg-or-temp vec))
            (ri (vreg->reg-or-temp index +spill-temp2+))
            (rd (dest-reg dest)))
        ;; x1 = untagged vector base
        (emit (arm64:and* :x1 rv -16 :imm t))
        ;; x0 = untagged index
        (emit (arm64:asr :x0 ri 1 :imm t))
        ;; x0 = index * 8 + 8 (skip length slot)
        (emit (arm64:lsl :x0 :x0 3 :imm t))
        (emit (arm64:add :x0 :x0 8 :imm t))
        ;; x1 = address of element
        (emit (arm64:add :x1 :x1 :x0))
        ;; Load element
        (emit (arm64:ldr rd :x1 :offset 0))
        (store-if-spilled dest)))

    (vector-set (vec index value)
      ;; Store value at vec[index]
      ;; vec: tagged vector, index: tagged fixnum, value: any tagged value
      (let ((rv (vreg->reg-or-temp vec))
            (ri (vreg->reg-or-temp index +spill-temp2+)))
        (when (is-spilled value)
          (emit (arm64:ldr :x17 :sp :offset (spill-offset value))))
        (let ((rval (if (is-spilled value) :x17 (vreg->reg value))))
          ;; x0 = untagged vector base
          (emit (arm64:and* :x0 rv -16 :imm t))
          ;; x1 = untagged index
          (emit (arm64:asr :x1 ri 1 :imm t))
          ;; x1 = (index + 1) * 8 = offset (skip length slot)
          (emit (arm64:add :x1 :x1 1 :imm t))
          (emit (arm64:lsl :x1 :x1 3 :imm t))
          ;; x0 = address of slot
          (emit (arm64:add :x0 :x0 :x1))
          ;; Store value
          (emit (arm64:str rval :x0 :offset 0)))))

    (vector-length (dest vec)
      ;; Return length as tagged fixnum
      ;; Length is stored UNTAGGED at vec[0] per +vector-length-repr+
      (let ((rv (vreg->reg-or-temp vec))
            (rd (dest-reg dest)))
        (emit (arm64:and* :x19 rv -16 :imm t))    ; x19 = untagged vector base
        (emit (arm64:ldr :x0 :x19))               ; x0 = UNTAGGED length
        ;; Re-tag as fixnum: (val << 1) | 1
        (emit (arm64:lsl rd :x0 1 :imm t))        ; rd = length << 1
        (emit (arm64:orr rd rd 1 :imm t))         ; rd = tagged fixnum
        (store-if-spilled dest)))

    ;; === Symbol Operations ===
    (make-symbol (dest name)
      (declare (ignore name))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (symbol-name (dest sym)
      (let ((rs (vreg->reg-or-temp sym))
            (rd (dest-reg dest)))
        (emit (arm64:and* :x19 rs -16 :imm t))
        (emit (arm64:ldr rd :x19 :offset 8))
        (store-if-spilled dest)))

    (intern (dest str)
      (declare (ignore str))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (symbol-lit (dest name)
      (declare (ignore name))
      ;; For now, emit nil - real symbol literals need intern support
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    ;; === Keyword Operations ===
    (keyword-name (dest kw)
      (let ((rs (vreg->reg-or-temp kw))
            (rd (dest-reg dest)))
        (emit (arm64:and* :x19 rs -16 :imm t))
        (emit (arm64:ldr rd :x19 :offset 8))
        (store-if-spilled dest)))

    (keyword-lit (dest name)
      (declare (ignore name))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    ;; === List Mutations ===
    (setcar (cell value)
      (let ((rc (vreg->reg-or-temp cell))
            (rv (vreg->reg-or-temp value +spill-temp2+)))
        (emit (arm64:and* :x19 rc -16 :imm t))
        (emit (arm64:str rv :x19 :offset 0))))

    (setcdr (cell value)
      (let ((rc (vreg->reg-or-temp cell))
            (rv (vreg->reg-or-temp value +spill-temp2+)))
        (emit (arm64:and* :x19 rc -16 :imm t))
        (emit (arm64:str rv :x19 :offset 8))))

    (nthcdr (dest n lst)
      (declare (ignore n lst))
      ;; TODO: implement nthcdr loop
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (length (dest lst)
      (declare (ignore lst))
      ;; TODO: implement length loop
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    ;; === Type Tag Operations ===
    (get-tag (dest value)
      (let ((rv (vreg->reg-or-temp value))
            (rd (dest-reg dest)))
        (emit (arm64:and* rd rv #.+tag-mask+ :imm t))
        (store-if-spilled dest)))

    (set-tag (dest value tag)
      (let ((rv (vreg->reg-or-temp value))
            (rt (vreg->reg-or-temp tag +spill-temp2+))
            (rd (dest-reg dest)))
        (emit (arm64:and* :x19 rv #.+ptr-mask+ :imm t))
        (emit (arm64:orr rd :x19 rt))
        (store-if-spilled dest)))

    ;; === String Mutations ===
    (make-string (dest len init)
      (declare (ignore len init))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (make-string-from-vector (dest vec)
      ;; Convert vector of char codes to string
      ;; vec: tagged vector pointer, each element is a tagged fixnum char code
      ;; Result: tagged string pointer
      ;; NOTE: Vector length is UNTAGGED at vec[0] per +vector-length-repr+
      ;;       String length is UNTAGGED at str[0] per +string-length-repr+
      (let ((rv (vreg->reg-or-temp vec))
            (rd (dest-reg dest)))
        ;; x1 = untagged vector base
        (emit (arm64:and* :x1 rv -16 :imm t))     ; untag vector (clear low 4 bits)
        ;; x5 = vector length (UNTAGGED at [vec+0])
        (emit (arm64:ldr :x5 :x1 :offset 0))      ; x5 = UNTAGGED length, no untag needed
        ;; Allocate string: store UNTAGGED length at [heap]
        (emit (arm64:str :x5 :x28 :offset 0))     ; [heap] = UNTAGGED length
        ;; x4 = alloc size = (8 + len + 15) & ~15
        (emit (arm64:add :x4 :x5 23 :imm t))
        (emit (arm64:and* :x4 :x4 -16 :imm t))
        ;; x0 = string base, bump heap
        (emit (arm64:mov :x0 :x28))
        (emit (arm64:add :x28 :x28 :x4))
        ;; x2 = string data = x0 + 8
        (emit (arm64:add :x2 :x0 8 :imm t))
        ;; x3 = loop counter = 0
        (emit (arm64:movz :x3 0))
        ;; Loop: copy chars from vector to string
        ;; Entry point (offset 0 from here)
        (emit (arm64:cmp :x3 :x5))                ; compare counter with length
        (emit (arm64:b.ge 9))                     ; skip 9 instrs to exit
        ;; Load vec[x3]: address = x1 + 8 + x3*8
        (emit (arm64:lsl :x4 :x3 3 :imm t))       ; x4 = x3 * 8
        (emit (arm64:add :x4 :x4 8 :imm t))       ; x4 = 8 + x3*8
        (emit (arm64:add :x4 :x1 :x4))            ; x4 = vec_base + offset
        (emit (arm64:ldr :x4 :x4 :offset 0))      ; x4 = tagged fixnum
        (emit (arm64:asr :x4 :x4 1 :imm t))       ; x4 = char value (untag fixnum)
        (emit (arm64:strb :x4 :x2 :x3 :reg t))    ; [x2 + x3] = x4 (byte)
        (emit (arm64:add :x3 :x3 1 :imm t))       ; x3++
        (emit (arm64:b -9))                       ; back to cmp
        ;; Tag result with string tag (6)
        (emit (arm64:movz :x4 6))                 ; x4 = string tag
        (emit (arm64:orr rd :x0 :x4))             ; rd = x0 | 6
        (store-if-spilled dest)))

    (string-equal (dest left right)
      (declare (ignore left right))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (string-set (str index value)
      (let ((rs (vreg->reg-or-temp str))
            (ri (vreg->reg-or-temp index +spill-temp2+)))
        ;; Need third temp for value - use x17
        (when (is-spilled value)
          (emit (arm64:ldr :x17 :sp :offset (spill-offset value))))
        (let ((rv (if (is-spilled value) :x17 (vreg->reg value))))
          (emit (arm64:and* :x19 rs -16 :imm t))
          (emit (arm64:add :x19 :x19 8 :imm t))
          (emit (arm64:strb rv :x19 ri :reg t)))))

    ;; === Buffer Operations ===
    (buffer-byte-ref (dest buf index)
      (let ((rb (vreg->reg-or-temp buf))
            (ri (vreg->reg-or-temp index +spill-temp2+))
            (rd (dest-reg dest)))
        (emit (arm64:and* :x19 rb -16 :imm t))
        (emit (arm64:add :x19 :x19 8 :imm t))
        (emit (arm64:ldrb rd :x19 ri :reg t))
        (store-if-spilled dest)))

    (buffer-byte-set (buf index value)
      (let ((rb (vreg->reg-or-temp buf))
            (ri (vreg->reg-or-temp index +spill-temp2+)))
        (when (is-spilled value)
          (emit (arm64:ldr :x17 :sp :offset (spill-offset value))))
        (let ((rv (if (is-spilled value) :x17 (vreg->reg value))))
          (emit (arm64:and* :x19 rb -16 :imm t))
          (emit (arm64:add :x19 :x19 8 :imm t))
          (emit (arm64:strb rv :x19 ri :reg t)))))

    (buffer-to-string (dest buf len)
      (declare (ignore buf len))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    ;; === Symbol Extended ===
    (make-symbol-from-string (dest str)
      (declare (ignore str))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    ;; === File I/O ===
    (read-file (dest path)
      (declare (ignore path))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (write-file (path content)
      (declare (ignore path content)))

    (write-bytes (fd bytes)
      (declare (ignore fd bytes)))

    (println (value)
      (declare (ignore value)))

    (sys-read (dest fd buf count)
      (declare (ignore fd buf count))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (sys-read-byte (dest fd)
      (declare (ignore fd))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (sys-write (dest fd buf count)
      (declare (ignore fd buf count))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (sys-write-char (fd char)
      (declare (ignore fd char)))

    (sys-open (dest path flags mode)
      (declare (ignore path flags mode))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (sys-close (dest fd)
      (declare (ignore fd))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    ;; === System/Low-level ===
    (system (dest cmd)
      (declare (ignore cmd))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (mmap (dest addr len prot flags fd offset)
      (declare (ignore addr len prot flags fd offset))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (mmap-jit (dest len)
      (declare (ignore len))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (munmap (dest addr len)
      (declare (ignore addr len))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (pthread-jit-write-protect (enable)
      (declare (ignore enable)))

    (sys-dcache-flush (addr len)
      (declare (ignore addr len)))

    (sys-icache-invalidate (addr len)
      (declare (ignore addr len)))

    (funcall-ptr (dest ptr args)
      (declare (ignore ptr args))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (mem-set-byte (addr value)
      (let ((ra (vreg->reg-or-temp addr))
            (rv (vreg->reg-or-temp value +spill-temp2+)))
        (emit (arm64:strb rv ra :offset 0))))

    (mem-load-64 (dest addr)
      (let ((ra (vreg->reg-or-temp addr))
            (rd (dest-reg dest)))
        (emit (arm64:ldr rd ra :offset 0))
        (store-if-spilled dest)))

    (mem-load-byte (dest addr)
      (let ((ra (vreg->reg-or-temp addr))
            (rd (dest-reg dest)))
        (emit (arm64:ldrb rd ra 0))  ; immediate offset mode
        (store-if-spilled dest)))

    ;; === Heap/Runtime Access ===
    (get-intern-table (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:ldr rd :x27 :offset 0))
        (store-if-spilled dest)))

    (set-intern-table (value)
      (let ((rv (vreg->reg-or-temp value)))
        (emit (arm64:str rv :x27 :offset 0))))

    (get-keyword-table (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:ldr rd :x27 :offset 128))
        (store-if-spilled dest)))

    (set-keyword-table (value)
      (let ((rv (vreg->reg-or-temp value)))
        (emit (arm64:str rv :x27 :offset 128))))

    (get-lambda-counter (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:ldr rd :x27 :offset 8))
        (store-if-spilled dest)))

    (set-lambda-counter (value)
      (let ((rv (vreg->reg-or-temp value)))
        (emit (arm64:str rv :x27 :offset 8))))

    (get-symbol-counter (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:ldr rd :x27 :offset 48))
        (store-if-spilled dest)))

    (set-symbol-counter (value)
      (let ((rv (vreg->reg-or-temp value)))
        (emit (arm64:str rv :x27 :offset 48))))

    (get-symbol-table (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:ldr rd :x27 :offset 56))
        (store-if-spilled dest)))

    (set-symbol-table (value)
      (let ((rv (vreg->reg-or-temp value)))
        (emit (arm64:str rv :x27 :offset 56))))

    (get-symtab-offset (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:ldr rd :x27 :offset 112))
        (store-if-spilled dest)))

    (get-symtab-count (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:ldr rd :x27 :offset 120))
        (store-if-spilled dest)))

    (get-frame-pointer (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:mov rd :x29))
        (store-if-spilled dest)))

    (get-code-base (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:mov rd :x26))
        (store-if-spilled dest)))

    (set-global-vars (value)
      (let ((rv (vreg->reg-or-temp value)))
        (emit (arm64:str rv :x27 :offset 104))))

    (get-global-vars (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:ldr rd :x27 :offset 104))
        (store-if-spilled dest)))

    (get-cmdline-args (dest)
      (let ((rd (dest-reg dest)))
        (emit (arm64:ldr rd :x27 :offset 72))
        (store-if-spilled dest)))

    ;; === Control Flow Extended ===
    (block-begin (id)
      (declare (ignore id)))

    (block-end (id)
      (declare (ignore id)))

    (return-from (id value)
      (declare (ignore id value))
      ;; TODO: non-local return
      )

    (continue ()
      ;; TODO: continue
      )

    (dolist-init (dest var-offset lst)
      (declare (ignore var-offset lst))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (dolist-next (dest var-offset lst end-label)
      (declare (ignore var-offset lst end-label))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (dotimes-init (dest var-offset count)
      (declare (ignore var-offset count))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (dotimes-next (dest var-offset count end-label)
      (declare (ignore var-offset count end-label))
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    ;; === Functions Extended ===
    (lambda (dest params body captures)
      (declare (ignore params body captures))
      ;; TODO: closure creation
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (lambda-ref (dest name captures)
      (declare (ignore name captures))
      ;; TODO: lambda reference
      (let ((rd (dest-reg dest)))
        (emit (arm64:movz rd 0))
        (store-if-spilled dest)))

    (tail-call (name args)
      (declare (ignore name args))
      ;; TODO: tail call
      )

    ;; === Multiple Values ===
    (values (vals)
      (declare (ignore vals)))

    (mvb (vars expr body)
      (declare (ignore vars expr body)))

    ;; === System ===
    (exit (code)
      ;; Untag fixnum: x0 = value >> 1
      (let ((rc (vreg->reg-or-temp code)))
        (emit (arm64:lsr :x0 rc 1 :imm t)))
      ;; syscall exit
      (emit (arm64:movz :x16 1))
      (emit (arm64:svc 0)))

    (error (message)
      (declare (ignore message))
      ;; TODO: error handling
      )))

;;; Apply fixups for forward branches
(defun apply-fixups ()
  "Patch branch instructions with resolved offsets.
   *code* must be in final order (already nreversed) when called."
  (let ((code-vec (coerce *code* 'vector)))
    (dolist (fixup *fixups*)
      (let* ((branch-offset (first fixup))
             (label (second fixup))
             (type (third fixup))
             (target-offset (gethash label *labels*)))
        (unless target-offset
          (error "Undefined label: ~S" label))
        ;; Calculate relative offset in instructions (bytes / 4)
        (let* ((rel-bytes (- target-offset branch-offset))
               (rel-instrs (ash rel-bytes -2))
               ;; Generate patched instruction
               (patched (ecase type
                          (:b (arm64:b rel-instrs))
                          (:b.eq (arm64:b.eq rel-instrs))
                          (:b.ne (arm64:b.ne rel-instrs))
                          (:bl (arm64:bl rel-instrs)))))
          ;; Patch the 4 bytes at branch-offset
          (loop for i from 0 below 4
                for byte in patched
                do (setf (aref code-vec (+ branch-offset i)) byte)))))
    (setf *code* (coerce code-vec 'list))))

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
   (arm64:str :x26 :sp :offset 64)
   (arm64:add :x20 :sp #x3 :imm t :shift12 t) ;; x20 = sp + 0x3000
   (arm64:add :x20 :x20 #xF80 :imm t)))       ;; x20 = sp + 0x3F80

(defun fn-prologue-with-spills (spill-count)
  "Generate prologue with space for spills. Uses fixed 16KB frame which has
   plenty of room for spills (temp slots at sp+0x40 can hold ~2000 values)."
  (declare (ignore spill-count))  ; Frame already has room
  (fn-fixed-prologue))

(defun fn-fixed-epilogue ()
  "Generate function epilogue for fixed 16KB frame"
  (append
   (arm64:ldr :x26 :sp :offset 64)
   (arm64:ldp :x23 :x24 :sp :offset 48)
   (arm64:ldp :x21 :x22 :sp :offset 32)
   (arm64:ldp :x19 :x20 :sp :offset 16)
   (arm64:ldr :x29 :sp :offset #x3FF0)
   (arm64:ldr :x30 :sp :offset #x3FF8)
   (arm64:add :sp :sp #x4 :imm t :shift12 t) ;; add sp, sp, #0x4000
   (arm64:ret)))

(defun gen-param-stores (params)
  "Generate code to store parameters from x0-x7 to environment (x20).
   Params are stored at [x20 + idx*8] for simple access."
  (let ((code nil)
        (idx 0))
    (dolist (p params)
      (declare (ignore p))
      (let ((reg (arm64:num-to-reg idx)))
        (setf code (append code (arm64:str reg :x20 :offset (* idx 8)))))
      (incf idx))
    code))

(defun codegen-function (name params body-tac alloc)
  "Generate complete function code with prologue/epilogue.
   Returns: (values code-bytes markers) where markers is list of (offset marker-data)."
  (declare (ignore name))
  (reset-codegen)
  (setf *vreg-to-reg* (allocation-result-vreg-to-reg alloc))

  ;; Set up spill slots mapping
  (setf *stack-size* (allocation-result-stack-size alloc))
  (setf *spill-slots* (make-hash-table))
  (let ((slot 0))
    (dolist (vreg (allocation-result-spills alloc))
      (setf (gethash vreg *spill-slots*) slot)
      (incf slot)))

  ;; Emit prologue (includes stack allocation for spills)
  (emit (fn-prologue-with-spills *stack-size*))

  ;; Store parameters from x0-x7 to environment
  (emit (gen-param-stores params))

  ;; Generate body
  (dolist (instr body-tac)
    (codegen-instr instr))

  ;; Reverse to correct order, then apply fixups
  (setf *code* (nreverse *code*))
  (setf *markers* (nreverse *markers*))
  (apply-fixups)
  (values *code* *markers*))
