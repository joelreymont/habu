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
(defvar *vreg-to-reg* nil "Hash table from vreg to physical register")
(defvar *labels* nil "Hash table from label -> byte offset")
(defvar *fixups* nil "List of (offset label type) for forward refs")
(defvar *local-functions* nil "List of function names in current compilation unit")

(defun reset-codegen ()
  (setf *code* nil)
  (setf *labels* (make-hash-table :test 'equal))
  (setf *fixups* nil))

(defun emit (&rest items)
  "Emit bytes to code stream. ARM64 functions return byte lists.
   Special markers (:call-fn name) are emitted as single list items."
  (dolist (item items)
    (cond
      ;; Marker - a list starting with a keyword (like :call-fn)
      ((and (consp item) (keywordp (car item)))
       (push item *code*))
      ;; Regular byte list from ARM64 encoder
      ((listp item) (dolist (byte item) (push byte *code*)))
      ;; Single byte
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
    (lit (dest value)
      (emit (arm64:movz (vreg->reg dest) (logand value #xFFFF))))

    (nil (dest)
      (emit (arm64:movz (vreg->reg dest) 0)))

    (t (dest)
      ;; t = pointer to T symbol, use small constant for now
      (emit (arm64:movz (vreg->reg dest) 3)))

    (move (dest src)
      (let ((rd (vreg->reg dest))
            (rs (vreg->reg src)))
        (unless (eq rd rs)
          (emit (arm64:mov rd rs)))))

    (var (dest offset)
      ;; Load from env (x20) at offset
      (emit (arm64:ldr (vreg->reg dest) :x20 :offset (* offset 8))))

    (setvar (offset src)
      (emit (arm64:str (vreg->reg src) :x20 :offset (* offset 8))))

    (global (dest name)
      (declare (ignore name))
      ;; TODO: global variable lookup
      (emit (arm64:movz (vreg->reg dest) 0)))

    (set-global (name src)
      (declare (ignore name src))
      ;; TODO: global variable store
      )

    ;; === Arithmetic ===
    (add (dest left right)
      (emit (arm64:add (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (sub (dest left right)
      (emit (arm64:sub (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (mul (dest left right)
      ;; Tagged mul: (a<<1) * (b<<1) = a*b<<2, need a*b<<1
      ;; So: mul dest, left, right; asr dest, dest, #1
      (emit (arm64:mul (vreg->reg dest) (vreg->reg left) (vreg->reg right)))
      (emit (arm64:asr (vreg->reg dest) (vreg->reg dest) 1 :imm t)))

    (div (dest left right)
      ;; Tagged div: (a<<1) / (b<<1) = a/b (untagged), need a/b<<1
      ;; So: sdiv dest, left, right; lsl dest, dest, #1
      (emit (arm64:sdiv (vreg->reg dest) (vreg->reg left) (vreg->reg right)))
      (emit (arm64:lsl (vreg->reg dest) (vreg->reg dest) 1 :imm t)))

    (mod (dest left right)
      ;; mod = a - (a / b) * b
      ;; Use x19 as temp
      (emit (arm64:sdiv :x19 (vreg->reg left) (vreg->reg right)))
      (emit (arm64:msub (vreg->reg dest) :x19 (vreg->reg right) (vreg->reg left))))

    (neg (dest value)
      (emit (arm64:neg (vreg->reg dest) (vreg->reg value))))

    ;; === Comparison ===
    (eq (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    (eql (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    (lt (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-lt+)))

    (gt (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-gt+)))

    (le (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-le+)))

    (ge (dest left right)
      (emit (arm64:cmp (vreg->reg left) (vreg->reg right)))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-ge+)))

    (zerop (dest value)
      (emit (arm64:cmp (vreg->reg value) 0 :imm t))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    ;; === Logical ===
    (not (dest value)
      (emit (arm64:cmp (vreg->reg value) 0 :imm t))
      (emit (arm64:cset (vreg->reg dest) #.arm64:+cc-eq+)))

    ;; === Bitwise ===
    (band (dest left right)
      (emit (arm64:and* (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (bor (dest left right)
      (emit (arm64:orr (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (bxor (dest left right)
      (emit (arm64:eor (vreg->reg dest) (vreg->reg left) (vreg->reg right))))

    (bsh (dest value shift)
      ;; Positive = left shift, negative = right shift
      ;; For now, assume left shift
      (emit (arm64:lsl (vreg->reg dest) (vreg->reg value) (vreg->reg shift))))

    (bnot (dest value)
      (emit (arm64:mvn (vreg->reg dest) (vreg->reg value))))

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
      (emit (arm64:cmp (vreg->reg cond) 0 :imm t))
      (let ((target-offset (gethash then-label *labels*)))
        (if target-offset
            (emit (arm64:b.ne (ash (- target-offset (current-offset)) -2)))
            (progn
              (push (list (current-offset) then-label :b.ne) *fixups*)
              (emit (arm64:b.ne 0))))))

    (ifnot (cond else-label)
      (emit (arm64:cmp (vreg->reg cond) 0 :imm t))
      (let ((target-offset (gethash else-label *labels*)))
        (if target-offset
            (emit (arm64:b.eq (ash (- target-offset (current-offset)) -2)))
            (progn
              (push (list (current-offset) else-label :b.eq) *fixups*)
              (emit (arm64:b.eq 0))))))

    (return (value)
      ;; Move result to x0
      (let ((rv (vreg->reg value)))
        (unless (eq rv :x0)
          (emit (arm64:mov :x0 rv))))
      ;; Use fixed epilogue for habu calling convention
      (emit (fn-fixed-epilogue)))

    ;; === Function Calls ===
    (param (dest index)
      ;; Load parameter from x0-x7
      (let ((rd (vreg->reg dest))
            (param-reg (arm64:num-to-reg index)))
        (unless (eq rd param-reg)
          (emit (arm64:mov rd param-reg)))))

    (arg (index src)
      (let ((arg-reg (arm64:num-to-reg index))
            (rs (vreg->reg src)))
        (unless (eq arg-reg rs)
          (emit (arm64:mov arg-reg rs)))))

    (call (dest name nargs)
      (declare (ignore nargs))
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
      ;; Move result from x0 to dest if needed
      (let ((rd (vreg->reg dest)))
        (unless (eq rd :x0)
          (emit (arm64:mov rd :x0)))))

    (funcall (dest fn nargs)
      (declare (ignore nargs))
      ;; Move closure/function pointer to x24 if not already there
      (let ((fn-reg (vreg->reg fn)))
        (unless (eq fn-reg :x24)
          (emit (arm64:mov :x24 fn-reg))))
      ;; Call through closure - BLR x24
      ;; x24 holds closure pointer per habu calling convention
      (emit (arm64:blr :x24))
      ;; Move result from x0 to dest if needed
      (let ((rd (vreg->reg dest)))
        (unless (eq rd :x0)
          (emit (arm64:mov rd :x0)))))

    ;; === List Operations ===
    (cons (dest car-vreg cdr-vreg)
      ;; Inline heap allocation: x28 = alloc ptr, x27 = heap base
      ;; Store car at x28+0, cdr at x28+8
      (emit (arm64:str (vreg->reg car-vreg) :x28 :offset 0))
      (emit (arm64:str (vreg->reg cdr-vreg) :x28 :offset 8))
      ;; Result = x28 (already tagged with 0 = cons tag)
      (emit (arm64:mov (vreg->reg dest) :x28))
      ;; Bump allocator: x28 += 16
      (emit (arm64:add :x28 :x28 16 :imm t)))

    (car (dest cell)
      ;; Untag pointer: clear low 4 bits (tag mask = -16)
      (emit (arm64:and* :x19 (vreg->reg cell) -16 :imm t))
      ;; Load car from offset 0
      (emit (arm64:ldr (vreg->reg dest) :x19 :offset 0)))

    (cdr (dest cell)
      ;; Untag pointer: clear low 4 bits (tag mask = -16)
      (emit (arm64:and* :x19 (vreg->reg cell) -16 :imm t))
      ;; Load cdr from offset 8
      (emit (arm64:ldr (vreg->reg dest) :x19 :offset 8)))

    (list (dest elems)
      (declare (ignore elems))
      ;; TODO: build list
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === Type Predicates ===
    ;; All predicates return t-value (3) for true, nil-value (0) for false
    (null (dest value)
      ;; nil = 0, so if value == 0 then t (3) else nil (0)
      (let ((rd (vreg->reg dest)))
        (emit (arm64:cmp (vreg->reg value) #.+nil-value+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; Convert 1 -> 3 (t-value), 0 -> 0 (nil-value): rd = rd * 3
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))))

    (consp (dest value)
      ;; Cons: tag == 0 AND value != 0 (to exclude nil which is also 0)
      (let ((rd (vreg->reg dest))
            (rv (vreg->reg value)))
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
        (emit (arm64:add rd rd :x19))))

    (symbolp (dest value)
      ;; Symbol: tag == 2
      (let ((rd (vreg->reg dest))
            (rv (vreg->reg value)))
        (emit (arm64:and* rd rv #.+tag-mask+ :imm t))
        (emit (arm64:cmp rd #.+tag-symbol+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; Convert 1 -> 3, 0 -> 0
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))))

    (stringp (dest value)
      ;; String: tag == 6
      (let ((rd (vreg->reg dest))
            (rv (vreg->reg value)))
        (emit (arm64:and* rd rv #.+tag-mask+ :imm t))
        (emit (arm64:cmp rd #.+tag-string+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; Convert 1 -> 3, 0 -> 0
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))))

    (numberp (dest value)
      ;; Fixnum: bit 0 = 1
      (let ((rd (vreg->reg dest))
            (rv (vreg->reg value)))
        (emit (arm64:and* rd rv #.+fixnum-bit+ :imm t))
        ;; Result is 1 or 0, convert 1 -> 3
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))))

    (keywordp (dest value)
      ;; Keyword: tag == 10
      (let ((rd (vreg->reg dest))
            (rv (vreg->reg value)))
        (emit (arm64:and* rd rv #.+tag-mask+ :imm t))
        (emit (arm64:cmp rd #.+tag-keyword+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; Convert 1 -> 3, 0 -> 0
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))))

    (functionp (dest value)
      ;; Closure: tag == 8
      (let ((rd (vreg->reg dest))
            (rv (vreg->reg value)))
        (emit (arm64:and* rd rv #.+tag-mask+ :imm t))
        (emit (arm64:cmp rd #.+tag-closure+ :imm t))
        (emit (arm64:cset rd #.arm64:+cc-eq+))
        ;; Convert 1 -> 3, 0 -> 0
        (emit (arm64:mov :x19 rd))
        (emit (arm64:add rd rd rd))
        (emit (arm64:add rd rd :x19))))

    ;; === String Operations ===
    (string-length (dest str)
      (emit (arm64:and* :x19 (vreg->reg str) -16 :imm t))
      (emit (arm64:ldr (vreg->reg dest) :x19)))

    (string-ref (dest str index)
      ;; Load byte at str + 8 + index
      (emit (arm64:and* :x19 (vreg->reg str) -16 :imm t))
      (emit (arm64:add :x19 :x19 8 :imm t))
      (emit (arm64:ldrb (vreg->reg dest) :x19 (vreg->reg index) :reg t)))

    (string-concat (dest left right)
      (declare (ignore left right))
      ;; TODO: implement
      (emit (arm64:movz (vreg->reg dest) 0)))

    (string-lit (dest string)
      (declare (ignore string))
      ;; TODO: load string literal address
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === Vector Operations ===
    (make-vector (dest size init)
      (declare (ignore size init))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (vector-ref (dest vec index)
      (emit (arm64:and* :x19 (vreg->reg vec) -16 :imm t))
      (emit (arm64:add :x19 :x19 8 :imm t))
      (emit (arm64:ldr (vreg->reg dest) :x19)))

    (vector-set (vec index value)
      (declare (ignore vec index value)))

    (vector-length (dest vec)
      (emit (arm64:and* :x19 (vreg->reg vec) -16 :imm t))
      (emit (arm64:ldr (vreg->reg dest) :x19)))

    ;; === Symbol Operations ===
    (make-symbol (dest name)
      (declare (ignore name))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (symbol-name (dest sym)
      (emit (arm64:and* :x19 (vreg->reg sym) -16 :imm t))
      (emit (arm64:ldr (vreg->reg dest) :x19 :offset 8)))

    (intern (dest str)
      (declare (ignore str))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (symbol-lit (dest name)
      (declare (ignore name))
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === Keyword Operations ===
    (keyword-name (dest kw)
      (emit (arm64:and* :x19 (vreg->reg kw) -16 :imm t))
      (emit (arm64:ldr (vreg->reg dest) :x19 :offset 8)))

    (keyword-lit (dest name)
      (declare (ignore name))
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === List Mutations ===
    (setcar (cell value)
      (emit (arm64:and* :x19 (vreg->reg cell) -16 :imm t))
      (emit (arm64:str (vreg->reg value) :x19 :offset 0)))

    (setcdr (cell value)
      (emit (arm64:and* :x19 (vreg->reg cell) -16 :imm t))
      (emit (arm64:str (vreg->reg value) :x19 :offset 8)))

    (nthcdr (dest n lst)
      (declare (ignore n lst))
      ;; TODO: implement nthcdr loop
      (emit (arm64:movz (vreg->reg dest) 0)))

    (length (dest lst)
      (declare (ignore lst))
      ;; TODO: implement length loop
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === Type Tag Operations ===
    (get-tag (dest value)
      (emit (arm64:and* (vreg->reg dest) (vreg->reg value) #.+tag-mask+ :imm t)))

    (set-tag (dest value tag)
      (emit (arm64:and* :x19 (vreg->reg value) #.+ptr-mask+ :imm t))
      (emit (arm64:orr (vreg->reg dest) :x19 (vreg->reg tag))))

    ;; === String Mutations ===
    (make-string (dest len init)
      (declare (ignore len init))
      ;; TODO: implement
      (emit (arm64:movz (vreg->reg dest) 0)))

    (make-string-from-vector (dest vec)
      (declare (ignore vec))
      ;; TODO: implement
      (emit (arm64:movz (vreg->reg dest) 0)))

    (string-equal (dest left right)
      (declare (ignore left right))
      ;; TODO: implement string comparison
      (emit (arm64:movz (vreg->reg dest) 0)))

    (string-set (str index value)
      (emit (arm64:and* :x19 (vreg->reg str) -16 :imm t))
      (emit (arm64:add :x19 :x19 8 :imm t))
      (emit (arm64:strb (vreg->reg value) :x19 (vreg->reg index) :reg t)))

    ;; === Buffer Operations ===
    (buffer-byte-ref (dest buf index)
      (emit (arm64:and* :x19 (vreg->reg buf) -16 :imm t))
      (emit (arm64:add :x19 :x19 8 :imm t))
      (emit (arm64:ldrb (vreg->reg dest) :x19 (vreg->reg index) :reg t)))

    (buffer-byte-set (buf index value)
      (emit (arm64:and* :x19 (vreg->reg buf) -16 :imm t))
      (emit (arm64:add :x19 :x19 8 :imm t))
      (emit (arm64:strb (vreg->reg value) :x19 (vreg->reg index) :reg t)))

    (buffer-to-string (dest buf len)
      (declare (ignore buf len))
      ;; TODO: implement
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === Symbol Extended ===
    (make-symbol-from-string (dest str)
      (declare (ignore str))
      ;; TODO: implement
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === File I/O ===
    (read-file (dest path)
      (declare (ignore path))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (write-file (path content)
      (declare (ignore path content)))

    (write-bytes (fd bytes)
      (declare (ignore fd bytes)))

    (println (value)
      (declare (ignore value)))

    (sys-read (dest fd buf count)
      (declare (ignore fd buf count))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (sys-read-byte (dest fd)
      (declare (ignore fd))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (sys-write (dest fd buf count)
      (declare (ignore fd buf count))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (sys-write-char (fd char)
      (declare (ignore fd char)))

    (sys-open (dest path flags mode)
      (declare (ignore path flags mode))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (sys-close (dest fd)
      (declare (ignore fd))
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === System/Low-level ===
    (system (dest cmd)
      (declare (ignore cmd))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (mmap (dest addr len prot flags fd offset)
      (declare (ignore addr len prot flags fd offset))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (mmap-jit (dest len)
      (declare (ignore len))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (munmap (dest addr len)
      (declare (ignore addr len))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (pthread-jit-write-protect (enable)
      (declare (ignore enable)))

    (sys-dcache-flush (addr len)
      (declare (ignore addr len)))

    (sys-icache-invalidate (addr len)
      (declare (ignore addr len)))

    (funcall-ptr (dest ptr args)
      (declare (ignore ptr args))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (mem-set-byte (addr value)
      (emit (arm64:strb (vreg->reg value) (vreg->reg addr) :offset 0)))

    (mem-load-64 (dest addr)
      (emit (arm64:ldr (vreg->reg dest) (vreg->reg addr) :offset 0)))

    (mem-load-byte (dest addr)
      (emit (arm64:ldrb (vreg->reg dest) (vreg->reg addr) :offset 0)))

    ;; === Heap/Runtime Access ===
    (get-intern-table (dest)
      (emit (arm64:ldr (vreg->reg dest) :x27 :offset 0)))

    (set-intern-table (value)
      (emit (arm64:str (vreg->reg value) :x27 :offset 0)))

    (get-keyword-table (dest)
      (emit (arm64:ldr (vreg->reg dest) :x27 :offset 128)))

    (set-keyword-table (value)
      (emit (arm64:str (vreg->reg value) :x27 :offset 128)))

    (get-lambda-counter (dest)
      (emit (arm64:ldr (vreg->reg dest) :x27 :offset 8)))

    (set-lambda-counter (value)
      (emit (arm64:str (vreg->reg value) :x27 :offset 8)))

    (get-symbol-counter (dest)
      (emit (arm64:ldr (vreg->reg dest) :x27 :offset 48)))

    (set-symbol-counter (value)
      (emit (arm64:str (vreg->reg value) :x27 :offset 48)))

    (get-symbol-table (dest)
      (emit (arm64:ldr (vreg->reg dest) :x27 :offset 56)))

    (set-symbol-table (value)
      (emit (arm64:str (vreg->reg value) :x27 :offset 56)))

    (get-symtab-offset (dest)
      (emit (arm64:ldr (vreg->reg dest) :x27 :offset 112)))

    (get-symtab-count (dest)
      (emit (arm64:ldr (vreg->reg dest) :x27 :offset 120)))

    (get-frame-pointer (dest)
      (emit (arm64:mov (vreg->reg dest) :x29)))

    (get-code-base (dest)
      (emit (arm64:mov (vreg->reg dest) :x26)))

    (set-global-vars (value)
      (emit (arm64:str (vreg->reg value) :x27 :offset 104)))

    (get-global-vars (dest)
      (emit (arm64:ldr (vreg->reg dest) :x27 :offset 104)))

    (get-cmdline-args (dest)
      (emit (arm64:ldr (vreg->reg dest) :x27 :offset 72)))

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
      (emit (arm64:movz (vreg->reg dest) 0)))

    (dolist-next (dest var-offset lst end-label)
      (declare (ignore var-offset lst end-label))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (dotimes-init (dest var-offset count)
      (declare (ignore var-offset count))
      (emit (arm64:movz (vreg->reg dest) 0)))

    (dotimes-next (dest var-offset count end-label)
      (declare (ignore var-offset count end-label))
      (emit (arm64:movz (vreg->reg dest) 0)))

    ;; === Functions Extended ===
    (lambda (dest params body captures)
      (declare (ignore params body captures))
      ;; TODO: closure creation
      (emit (arm64:movz (vreg->reg dest) 0)))

    (lambda-ref (dest name captures)
      (declare (ignore name captures))
      ;; TODO: lambda reference
      (emit (arm64:movz (vreg->reg dest) 0)))

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
      (emit (arm64:lsr :x0 (vreg->reg code) 1 :imm t))
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
