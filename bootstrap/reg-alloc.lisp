;;; ============================================================
;;; Register Allocation via Nanopasses
;;; ============================================================
;;;
;;; This file implements register allocation as a series of nanopasses
;;; that transform the IR progressively toward efficient machine code.
;;;
;;; ARCHITECTURE OVERVIEW
;;; ====================
;;;
;;; The current Habu codegen uses an "accumulator model":
;;; - All expression results go to x0
;;; - Intermediate values spill to stack temp slots
;;; - Each function allocates a 2KB stack frame regardless of need
;;;
;;; This causes severe performance issues:
;;; - Excessive memory traffic (every subexpression spills)
;;; - Huge stack frames for simple functions
;;; - Function call overhead dominates for small functions
;;;
;;; The new architecture uses a "register model":
;;; - Each value gets a virtual register (v0, v1, v2, ...)
;;; - Linear scan allocates physical registers (x9-x15)
;;; - Only spill when registers are exhausted
;;; - Stack frame sized based on actual needs
;;;
;;; NANOPASS PIPELINE
;;; =================
;;;
;;; Pass 1: ir-to-tac (IR → Three-Address Code)
;;;   Input:  Tree-structured IR: (add (var 0) (mul (lit 2) (var 1)))
;;;   Output: Linear TAC instructions with virtual registers
;;;           ((tac-var v0 0)
;;;            (tac-lit v1 2)
;;;            (tac-var v2 1)
;;;            (tac-binop v3 mul v1 v2)
;;;            (tac-binop v4 add v0 v3))
;;;
;;; Pass 2: compute-liveness (TAC → TAC + Liveness Info)
;;;   Input:  TAC instructions
;;;   Output: TAC with live-in/live-out sets for each instruction
;;;           Computes which virtual registers are live at each point
;;;
;;; Pass 3: compute-intervals (Liveness → Live Intervals)
;;;   Input:  TAC with liveness info
;;;   Output: List of (vreg start-pos end-pos) intervals
;;;
;;; Pass 4: linear-scan (Intervals → Allocation)
;;;   Input:  Live intervals
;;;   Output: Allocation map: vreg → physical-reg or spill-slot
;;;
;;; Pass 5: tac-codegen (TAC + Allocation → ARM64)
;;;   Input:  TAC instructions + allocation map
;;;   Output: ARM64 machine code bytes
;;;
;;; THREE-ADDRESS CODE (TAC) FORMAT
;;; ===============================
;;;
;;; TAC uses explicit virtual registers (v0, v1, ...) and linear control flow.
;;; This is similar to SSA form but without φ-functions (we use explicit moves).
;;;
;;; Instruction formats:
;;;   (tac-lit vreg value)           ; vreg = literal value (tagged)
;;;   (tac-param vreg index)         ; vreg = parameter[index]
;;;   (tac-var vreg offset)          ; vreg = env[offset]
;;;   (tac-setvar offset vreg)       ; env[offset] = vreg
;;;   (tac-binop vreg op vr1 vr2)    ; vreg = vr1 op vr2
;;;   (tac-unop vreg op vr1)         ; vreg = op(vr1)
;;;   (tac-call vreg fn args)        ; vreg = fn(args...), args are vregs
;;;   (tac-if vreg then-lbl else-lbl); branch based on vreg
;;;   (tac-label name)               ; label for jumps
;;;   (tac-goto label)               ; unconditional jump
;;;   (tac-return vreg)              ; return value in vreg
;;;   (tac-move vreg1 vreg2)         ; vreg1 = vreg2 (for merging control flow)
;;;
;;; REGISTER USAGE
;;; ==============
;;;
;;; ARM64 registers:
;;;   x0-x7:   Arguments and return value (caller-saved)
;;;   x9-x15:  Available for allocation (7 registers, caller-saved)
;;;   x19-x22: Callee-saved, can use for values spanning calls
;;;   x20:     Reserved for environment frame base
;;;   x24:     Reserved for closure environment
;;;   x26:     Reserved for code base
;;;   x27:     Reserved for heap base
;;;   x28:     Reserved for heap bump pointer
;;;   x29:     Frame pointer
;;;   x30:     Link register
;;;
;;; Allocation strategy:
;;;   - Values not crossing calls: x9-x15 (7 regs)
;;;   - Values crossing calls: x19, x21, x22 (3 regs, x20 reserved)
;;;   - Spill to stack when exhausted
;;;
;;; EXAMPLE TRANSFORMATION
;;; ======================
;;;
;;; Source: (defun add3 (a b c) (+ a (+ b c)))
;;;
;;; Original IR:
;;;   (add (var 0) (add (var 1) (var 2)))
;;;
;;; After ir-to-tac:
;;;   ((tac-var v0 0)        ; v0 = a
;;;    (tac-var v1 1)        ; v1 = b
;;;    (tac-var v2 2)        ; v2 = c
;;;    (tac-binop v3 add v1 v2)  ; v3 = b + c
;;;    (tac-binop v4 add v0 v3)  ; v4 = a + v3
;;;    (tac-return v4))
;;;
;;; After compute-intervals:
;;;   v0: [0, 4]  ; defined at 0, last used at 4
;;;   v1: [1, 3]  ; defined at 1, last used at 3
;;;   v2: [2, 3]  ; defined at 2, last used at 3
;;;   v3: [3, 4]  ; defined at 3, last used at 4
;;;   v4: [4, 5]  ; defined at 4, last used at 5
;;;
;;; After linear-scan:
;;;   v0 → x9
;;;   v1 → x10
;;;   v2 → x11
;;;   v3 → x10  ; reuses x10 after v1 expires
;;;   v4 → x0   ; return value
;;;
;;; Generated ARM64:
;;;   ldr x9, [x20, #-0]      ; v0 = a
;;;   ldr x10, [x20, #-8]     ; v1 = b
;;;   ldr x11, [x20, #-16]    ; v2 = c
;;;   add x10, x10, x11       ; v3 = b + c
;;;   add x0, x9, x10         ; v4 = a + v3
;;;   ; return (x0 already has result)
;;;
;;; Compare to current codegen (accumulator model):
;;;   ldr x0, [x20, #-0]      ; load a
;;;   str x0, [sp, #0x40]     ; spill a
;;;   ldr x0, [x20, #-8]      ; load b
;;;   str x0, [sp, #0x48]     ; spill b
;;;   ldr x0, [x20, #-16]     ; load c
;;;   ldr x1, [sp, #0x48]     ; restore b
;;;   add x0, x1, x0          ; b + c
;;;   str x0, [sp, #0x48]     ; spill result
;;;   ldr x0, [sp, #0x40]     ; restore a
;;;   ldr x1, [sp, #0x48]     ; restore b+c
;;;   add x0, x0, x1          ; a + (b+c)
;;;
;;; The register-allocated version: 5 instructions
;;; The spill-heavy version: 11 instructions (2.2x more)
;;;
;;; ============================================================

#+sbcl (in-package :habu)

;;; ============================================================
;;; Pass 1: IR to TAC Conversion
;;; ============================================================
;;;
;;; Converts tree-structured IR to linear Three-Address Code.
;;; Each subexpression gets a unique virtual register.

(defun make-vreg-counter ()
  "Create a fresh virtual register counter (mutable cons cell)"
  (cons 0 nil))

(defun next-vreg (counter)
  "Allocate next virtual register number"
  (let ((n (car counter)))
    #+sbcl (setf (car counter) (+ n 1))
    #-sbcl (setcar counter (+ n 1))
    n))

(defun ir-tag-name (sym)
  "Get the symbol name as a string for tag comparison.
   Handles symbols from different packages."
  (if (symbolp sym)
      (symbol-name sym)
      ""))

(defun ir-tag-matches (sym name)
  "Check if symbol matches the given name string (case-insensitive)."
  (string-equal (ir-tag-name sym) name))

(defun ir-tag-member (sym names)
  "Check if symbol matches any of the given name strings."
  (let ((tag (ir-tag-name sym)))
    (labels ((check (lst)
               (if (null lst)
                   nil
                   (if (string-equal tag (car lst))
                       t
                       (check (cdr lst))))))
      (check names))))

(defun ir-to-tac (ir counter)
  "Convert IR tree to TAC instructions.
   Returns (instructions result-vreg) where result-vreg holds the value.

   This is Pass 1 of the register allocation pipeline.
   Input: Tree IR like (add (var 0) (lit 1))
   Output: Linear TAC like ((tac-var v0 0) (tac-lit v1 1) (tac-binop v2 add v0 v1))"
  (cond
    ;; Literal number (already tagged)
    ((numberp ir)
     (let ((vr (next-vreg counter)))
       (list (list (list 'tac-lit vr ir)) vr)))

    ;; (lit value) - literal
    ((and (consp ir) (ir-tag-matches (car ir) "LIT"))
     (let ((vr (next-vreg counter)))
       (list (list (list 'tac-lit vr (cadr ir))) vr)))

    ;; (var offset) - variable reference
    ((and (consp ir) (ir-tag-matches (car ir) "VAR"))
     (let ((vr (next-vreg counter)))
       (list (list (list 'tac-var vr (cadr ir))) vr)))

    ;; Binary operations: add, sub, mul, div, mod
    ((and (consp ir) (ir-tag-member (car ir) '("ADD" "SUB" "MUL" "DIV" "MOD")))
     (let* ((left-result (ir-to-tac (cadr ir) counter))
            (left-instrs (car left-result))
            (left-vr (cadr left-result))
            (right-result (ir-to-tac (caddr ir) counter))
            (right-instrs (car right-result))
            (right-vr (cadr right-result))
            (result-vr (next-vreg counter))
            (op-name (ir-tag-name (car ir))))
       (list (append left-instrs
                     right-instrs
                     (list (list 'tac-binop result-vr
                                 (intern op-name :keyword)  ; :ADD, :SUB, etc.
                                 left-vr right-vr)))
             result-vr)))

    ;; Comparison operations
    ((and (consp ir) (ir-tag-member (car ir) '("CMP-EQ" "CMP-NE" "CMP-LT" "CMP-LE" "CMP-GT" "CMP-GE")))
     (let* ((left-result (ir-to-tac (cadr ir) counter))
            (left-instrs (car left-result))
            (left-vr (cadr left-result))
            (right-result (ir-to-tac (caddr ir) counter))
            (right-instrs (car right-result))
            (right-vr (cadr right-result))
            (result-vr (next-vreg counter)))
       (list (append left-instrs
                     right-instrs
                     (list (list 'tac-cmp result-vr (car ir) left-vr right-vr)))
             result-vr)))

    ;; if-ir: conditional expression
    ((and (consp ir) (ir-tag-matches (car ir) "IF-IR"))
     (let* ((cond-result (ir-to-tac (cadr ir) counter))
            (cond-instrs (car cond-result))
            (cond-vr (cadr cond-result))
            (result-vr (next-vreg counter))
            (then-label (next-vreg counter))
            (else-label (next-vreg counter))
            (end-label (next-vreg counter))
            (then-result (ir-to-tac (caddr ir) counter))
            (then-instrs (car then-result))
            (then-vr (cadr then-result))
            (else-result (ir-to-tac (cadddr ir) counter))
            (else-instrs (car else-result))
            (else-vr (cadr else-result)))
       (list (append cond-instrs
                     (list (list 'tac-if cond-vr then-label else-label))
                     (list (list 'tac-label then-label))
                     then-instrs
                     (list (list 'tac-move result-vr then-vr))
                     (list (list 'tac-goto end-label))
                     (list (list 'tac-label else-label))
                     else-instrs
                     (list (list 'tac-move result-vr else-vr))
                     (list (list 'tac-label end-label)))
             result-vr)))

    ;; let-ir: local bindings
    ((and (consp ir) (ir-tag-matches (car ir) "LET-IR"))
     (let* ((vals (cadr ir))
            (body (caddr ir))
            (offsets (nth 4 ir)))  ; (let-ir vals body count offsets)
       (labels ((convert-bindings (vs os instrs)
                  (if (null vs)
                      instrs
                      (let* ((val-result (ir-to-tac (car vs) counter))
                             (val-instrs (car val-result))
                             (val-vr (cadr val-result))
                             (store-instr (list 'tac-setvar (car os) val-vr)))
                        (convert-bindings (cdr vs) (cdr os)
                                          (append instrs val-instrs (list store-instr)))))))
         (let* ((binding-instrs (convert-bindings vals offsets nil))
                (body-result (ir-to-tac body counter))
                (body-instrs (car body-result))
                (body-vr (cadr body-result)))
           (list (append binding-instrs body-instrs) body-vr)))))

    ;; progn-ir: sequence
    ((and (consp ir) (ir-tag-matches (car ir) "PROGN-IR"))
     (let ((forms (cadr ir)))
       (labels ((convert-forms (fs instrs last-vr)
                  (if (null fs)
                      (list instrs last-vr)
                      (let* ((form-result (ir-to-tac (car fs) counter))
                             (form-instrs (car form-result))
                             (form-vr (cadr form-result)))
                        (convert-forms (cdr fs)
                                       (append instrs form-instrs)
                                       form-vr)))))
         (convert-forms forms nil 0))))

    ;; call-fn: function call
    ((and (consp ir) (ir-tag-matches (car ir) "CALL-FN"))
     (let* ((fn-name (cadr ir))
            (args (cddr ir)))
       (labels ((convert-args (as instrs vrs)
                  (if (null as)
                      (list instrs (reverse vrs))
                      (let* ((arg-result (ir-to-tac (car as) counter))
                             (arg-instrs (car arg-result))
                             (arg-vr (cadr arg-result)))
                        (convert-args (cdr as)
                                      (append instrs arg-instrs)
                                      (cons arg-vr vrs))))))
         (let* ((args-result (convert-args args nil nil))
                (args-instrs (car args-result))
                (arg-vrs (cadr args-result))
                (result-vr (next-vreg counter)))
           (list (append args-instrs
                         (list (list 'tac-call result-vr fn-name arg-vrs)))
                 result-vr)))))

    ;; Default: return nil vreg for unhandled cases
    (t
     (list nil 0))))

;;; ============================================================
;;; Pass 2: Liveness Analysis
;;; ============================================================
;;;
;;; Computes which virtual registers are live at each instruction.
;;; Uses backward dataflow analysis:
;;;   live-in[i] = use[i] ∪ (live-out[i] - def[i])
;;;   live-out[i] = ∪ live-in[successors of i]

(defun tac-def (instr)
  "Return the vreg defined by this instruction (or nil)"
  (case (car instr)
    ((tac-lit tac-param tac-var tac-binop tac-cmp tac-call tac-move)
     (cadr instr))
    (t nil)))

(defun tac-use (instr)
  "Return list of vregs used by this instruction"
  (case (car instr)
    ((tac-lit tac-param tac-var tac-label tac-goto)
     nil)
    ((tac-binop tac-cmp)
     (list (cadddr instr) (nth 4 instr)))  ; op vreg op vr1 vr2
    ((tac-setvar)
     (list (caddr instr)))  ; offset vreg
    ((tac-if)
     (list (cadr instr)))   ; cond-vreg
    ((tac-return)
     (list (cadr instr)))   ; return-vreg
    ((tac-move)
     (list (caddr instr)))  ; dest src
    ((tac-call)
     (cadddr instr))        ; list of arg vregs
    (t nil)))

(defun compute-liveness (tac-instrs)
  "Compute liveness for TAC instructions.
   Returns list of (instr live-in live-out) tuples.

   This is Pass 2 of the register allocation pipeline."
  (let* ((n (length tac-instrs))
         (live-in (make-list n))
         (live-out (make-list n))
         (changed t))
    ;; Build successor map for control flow
    (labels ((find-label-index (label instrs idx)
               (if (null instrs)
                   nil
                   (if (and (eq (caar instrs) 'tac-label)
                            (= (cadar instrs) label))
                       idx
                       (find-label-index label (cdr instrs) (+ idx 1)))))

             (successors (instr idx)
               ;; Return indices of successor instructions
               (case (car instr)
                 ((tac-goto)
                  (let ((target (find-label-index (cadr instr) tac-instrs 0)))
                    (if target (list target) nil)))
                 ((tac-if)
                  (let ((then-idx (find-label-index (caddr instr) tac-instrs 0))
                        (else-idx (find-label-index (cadddr instr) tac-instrs 0)))
                    (append (if then-idx (list then-idx) nil)
                            (if else-idx (list else-idx) nil))))
                 ((tac-return)
                  nil)  ; no successor
                 (t
                  (if (< (+ idx 1) n) (list (+ idx 1)) nil))))

             (set-union (a b)
               (if (null a) b
                   (if (member (car a) b)
                       (set-union (cdr a) b)
                       (cons (car a) (set-union (cdr a) b)))))

             (set-diff (a b)
               (if (null a) nil
                   (if (member (car a) b)
                       (set-diff (cdr a) b)
                       (cons (car a) (set-diff (cdr a) b)))))

             (nth-set (lst idx val)
               ;; Destructively set nth element
               (if (= idx 0)
                   #+sbcl (setf (car lst) val)
                   #-sbcl (setcar lst val)
                   (nth-set (cdr lst) (- idx 1) val)))

             (iterate ()
               ;; One pass of backward dataflow
               (setq changed nil)
               (let ((idx (- n 1)))
                 (labels ((process-instr (instrs)
                            (when instrs
                              (let* ((instr (car instrs))
                                     (succs (successors instr idx))
                                     (new-out (let ((result nil))
                                                (dolist (s succs)
                                                  (setq result (set-union result (nth s live-in))))
                                                result))
                                     (def (tac-def instr))
                                     (use (tac-use instr))
                                     (new-in (set-union use (set-diff new-out (if def (list def) nil)))))
                                (unless (equal new-in (nth idx live-in))
                                  (setq changed t)
                                  (nth-set live-in idx new-in))
                                (nth-set live-out idx new-out)
                                (setq idx (- idx 1))
                                (process-instr (cdr instrs))))))
                   (process-instr (reverse tac-instrs))))))

      ;; Iterate until fixed point
      (loop while changed do (iterate))

      ;; Return annotated instructions
      (let ((result nil)
            (idx 0))
        (dolist (instr tac-instrs)
          (setq result (cons (list instr (nth idx live-in) (nth idx live-out)) result))
          (setq idx (+ idx 1)))
        (reverse result)))))

;;; ============================================================
;;; Pass 3: Compute Live Intervals
;;; ============================================================
;;;
;;; Converts liveness info to live intervals: (vreg start end)

(defun compute-intervals (annotated-tac)
  "Compute live intervals from annotated TAC.
   Returns alist: ((vreg start end) ...)

   This is Pass 3 of the register allocation pipeline."
  (let ((intervals nil)
        (pos 0))
    (labels ((update-interval (vr p)
               (let ((entry (assoc vr intervals)))
                 (if entry
                     ;; Extend end position
                     #+sbcl (setf (caddr entry) p)
                     #-sbcl (setcar (cddr entry) p)
                     ;; New interval
                     (setq intervals (cons (list vr p p) intervals))))))
      (dolist (annotated annotated-tac)
        (let ((instr (car annotated))
              (live-in (cadr annotated)))
          ;; Record definition
          (let ((def (tac-def instr)))
            (when def (update-interval def pos)))
          ;; Record uses
          (dolist (vr live-in)
            (update-interval vr pos))
          (setq pos (+ pos 1))))
      intervals)))

;;; ============================================================
;;; Pass 4: Linear Scan Register Allocation
;;; ============================================================
;;;
;;; Allocates physical registers using linear scan algorithm.
;;; Returns allocation map: ((vreg . reg-or-spill) ...)

(defun allocatable-regs ()
  "Registers available for allocation.
   x9-x15: 7 caller-saved temporaries"
  '(9 10 11 12 13 14 15))

(defun callee-saved-regs ()
  "Callee-saved registers for values spanning calls.
   x19, x21, x22 (x20 reserved for env base)"
  '(19 21 22))

(defun linear-scan (intervals)
  "Perform linear scan register allocation.
   Returns allocation: ((vreg . physical-reg-or-spill) ...)
   where spill is (:spill slot-number)

   This is Pass 4 of the register allocation pipeline."
  (let* ((sorted (sort-intervals-by-start intervals))
         (active nil)           ; currently active: ((vreg end . reg) ...)
         (allocation nil)       ; result
         (free-regs (copy-list (allocatable-regs)))
         (spill-slot 0))

    (labels ((expire-old (pos)
               ;; Remove intervals ending before pos, free their registers
               (let ((still-active nil))
                 (dolist (a active)
                   (if (< (cadr a) pos)
                       ;; Expired - return register to pool
                       (setq free-regs (cons (cddr a) free-regs))
                       ;; Still active
                       (setq still-active (cons a still-active))))
                 (setq active still-active)))

             (allocate-one (interval)
               (let* ((vr (car interval))
                      (start (cadr interval))
                      (end (caddr interval)))
                 (expire-old start)
                 (if free-regs
                     ;; Allocate register
                     (let ((reg (car free-regs)))
                       (setq free-regs (cdr free-regs))
                       (setq active (cons (cons vr (cons end reg)) active))
                       (setq allocation (cons (cons vr reg) allocation)))
                     ;; Spill - no free registers
                     (let ((slot spill-slot))
                       (setq spill-slot (+ spill-slot 1))
                       (setq allocation (cons (cons vr (list :spill slot)) allocation)))))))

      (dolist (interval sorted)
        (allocate-one interval))

      allocation)))

(defun sort-intervals-by-start (intervals)
  "Sort intervals by start position (ascending)"
  (labels ((insert-sorted (item sorted)
             (if (null sorted)
                 (list item)
                 (if (<= (cadr item) (cadar sorted))
                     (cons item sorted)
                     (cons (car sorted)
                           (insert-sorted item (cdr sorted)))))))
    (let ((result nil))
      (dolist (i intervals)
        (setq result (insert-sorted i result)))
      result)))

;;; ============================================================
;;; Pass 5: TAC Code Generation
;;; ============================================================
;;;
;;; Generates ARM64 machine code from TAC + allocation.
;;; Uses arm64 package intrinsics for instruction encoding.

(defun vreg-to-reg (vreg allocation)
  "Look up physical register for vreg. Returns reg number or (:spill slot)."
  (let ((entry (assoc vreg allocation)))
    (if entry (cdr entry) 0)))

(defun emit-load-vreg (vreg allocation dest-reg)
  "Emit code to load vreg into dest-reg.
   If vreg is in a register, emit MOV. If spilled, emit LDR from stack."
  (let ((loc (vreg-to-reg vreg allocation)))
    (if (and (consp loc) (eq (car loc) :spill))
        ;; Spilled: load from stack
        (arm64:ldr dest-reg 31 :offset (* (cadr loc) 8))
        ;; In register: move if different
        (if (= loc dest-reg)
            nil  ; Already in dest
            (arm64:mov dest-reg loc)))))

(defun emit-store-vreg (vreg allocation src-reg)
  "Emit code to store src-reg to vreg's location.
   If vreg is in a register, emit MOV. If spilled, emit STR to stack."
  (let ((loc (vreg-to-reg vreg allocation)))
    (if (and (consp loc) (eq (car loc) :spill))
        ;; Spilled: store to stack
        (arm64:str src-reg 31 :offset (* (cadr loc) 8))
        ;; In register: move if different
        (if (= loc src-reg)
            nil  ; Already there
            (arm64:mov loc src-reg)))))

(defun tac-codegen-instr (instr allocation)
  "Generate ARM64 code for a single TAC instruction.
   Returns list of instruction bytes (each instruction is 4 bytes)."
  (let ((op (car instr)))
    (case op
      ;; tac-lit: load literal into vreg
      ((tac-lit)
       (let* ((vreg (cadr instr))
              (value (caddr instr))
              (dest (vreg-to-reg vreg allocation)))
         (if (and (consp dest) (eq (car dest) :spill))
             ;; Spilled: load to x0, then store
             (append (arm64:movz 0 (logand value #xFFFF))
                     (arm64:str 0 31 :offset (* (cadr dest) 8)))
             ;; In register
             (arm64:movz dest (logand value #xFFFF)))))

      ;; tac-var: load from environment
      ((tac-var)
       (let* ((vreg (cadr instr))
              (offset (caddr instr))
              (dest (vreg-to-reg vreg allocation)))
         (if (and (consp dest) (eq (car dest) :spill))
             ;; Spilled: load to x0, then store
             (append (arm64:ldr 0 20 :offset (* offset -8))
                     (arm64:str 0 31 :offset (* (cadr dest) 8)))
             ;; In register
             (arm64:ldr dest 20 :offset (* offset -8)))))

      ;; tac-setvar: store to environment
      ((tac-setvar)
       (let* ((offset (cadr instr))
              (vreg (caddr instr))
              (src (vreg-to-reg vreg allocation)))
         (if (and (consp src) (eq (car src) :spill))
             ;; Spilled: load to x0, then store to env
             (append (arm64:ldr 0 31 :offset (* (cadr src) 8))
                     (arm64:str 0 20 :offset (* offset -8)))
             ;; In register
             (arm64:str src 20 :offset (* offset -8)))))

      ;; tac-binop: binary operation
      ((tac-binop)
       (let* ((dest-vreg (cadr instr))
              (binop (caddr instr))
              (left-vreg (cadddr instr))
              (right-vreg (nth 4 instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (left (vreg-to-reg left-vreg allocation))
              (right (vreg-to-reg right-vreg allocation))
              ;; Use x0, x1 as temporaries if needed
              (left-reg (if (and (consp left) (eq (car left) :spill)) 0 left))
              (right-reg (if (and (consp right) (eq (car right) :spill)) 1 right))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) 0 dest)))
         (append
          ;; Load spilled operands
          (when (and (consp left) (eq (car left) :spill))
            (arm64:ldr 0 31 :offset (* (cadr left) 8)))
          (when (and (consp right) (eq (car right) :spill))
            (arm64:ldr 1 31 :offset (* (cadr right) 8)))
          ;; Perform operation
          (case binop
            ((:ADD) (arm64:add dest-reg left-reg right-reg))
            ((:SUB) (arm64:sub dest-reg left-reg right-reg))
            ((:MUL) (arm64:mul dest-reg left-reg right-reg))
            ((:DIV) (arm64:sdiv dest-reg left-reg right-reg))
            ((:MOD) (arm64:sdiv dest-reg left-reg right-reg))  ; TODO: proper mod
            (t (arm64:mov dest-reg 0)))  ; Unknown op
          ;; Store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str 0 31 :offset (* (cadr dest) 8))))))

      ;; tac-cmp: comparison (result is 0 or non-zero)
      ((tac-cmp)
       (let* ((dest-vreg (cadr instr))
              (cmp-op (caddr instr))
              (left-vreg (cadddr instr))
              (right-vreg (nth 4 instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (left (vreg-to-reg left-vreg allocation))
              (right (vreg-to-reg right-vreg allocation))
              (left-reg (if (and (consp left) (eq (car left) :spill)) 0 left))
              (right-reg (if (and (consp right) (eq (car right) :spill)) 1 right))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) 0 dest)))
         (append
          ;; Load spilled operands
          (when (and (consp left) (eq (car left) :spill))
            (arm64:ldr 0 31 :offset (* (cadr left) 8)))
          (when (and (consp right) (eq (car right) :spill))
            (arm64:ldr 1 31 :offset (* (cadr right) 8)))
          ;; Compare
          (arm64:cmp left-reg right-reg)
          ;; Set result based on condition
          (case cmp-op
            ((cmp-eq) (arm64:cset dest-reg arm64:+eq+))
            ((cmp-ne) (arm64:cset dest-reg arm64:+ne+))
            ((cmp-lt) (arm64:cset dest-reg arm64:+lt+))
            ((cmp-le) (arm64:cset dest-reg arm64:+le+))
            ((cmp-gt) (arm64:cset dest-reg arm64:+gt+))
            ((cmp-ge) (arm64:cset dest-reg arm64:+ge+))
            (t (arm64:movz dest-reg 0)))
          ;; Store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str 0 31 :offset (* (cadr dest) 8))))))

      ;; tac-move: copy between vregs
      ((tac-move)
       (let* ((dest-vreg (cadr instr))
              (src-vreg (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (src (vreg-to-reg src-vreg allocation)))
         (cond
           ;; Both spilled
           ((and (consp dest) (consp src))
            (append (arm64:ldr 0 31 :offset (* (cadr src) 8))
                    (arm64:str 0 31 :offset (* (cadr dest) 8))))
           ;; Src spilled
           ((consp src)
            (arm64:ldr dest 31 :offset (* (cadr src) 8)))
           ;; Dest spilled
           ((consp dest)
            (arm64:str src 31 :offset (* (cadr dest) 8)))
           ;; Both in registers
           ((= dest src) nil)
           (t (arm64:mov dest src)))))

      ;; tac-return: move result to x0
      ((tac-return)
       (let* ((vreg (cadr instr))
              (src (vreg-to-reg vreg allocation)))
         (if (and (consp src) (eq (car src) :spill))
             (arm64:ldr 0 31 :offset (* (cadr src) 8))
             (if (= src 0)
                 nil  ; Already in x0
                 (arm64:mov 0 src)))))

      ;; tac-label: no code, just record position
      ((tac-label) nil)

      ;; tac-goto: unconditional branch (resolved later)
      ((tac-goto)
       (list :branch-marker (cadr instr)))

      ;; tac-if: conditional branch
      ((tac-if)
       (let* ((cond-vreg (cadr instr))
              (then-label (caddr instr))
              (else-label (cadddr instr))
              (cond-loc (vreg-to-reg cond-vreg allocation))
              (cond-reg (if (and (consp cond-loc) (eq (car cond-loc) :spill)) 0 cond-loc)))
         (append
          ;; Load condition if spilled
          (when (and (consp cond-loc) (eq (car cond-loc) :spill))
            (arm64:ldr 0 31 :offset (* (cadr cond-loc) 8)))
          ;; Compare with nil (0x06)
          (arm64:cmp cond-reg #x06 :imm t)
          ;; Branch markers (resolved in second pass)
          (list :branch-ne-marker then-label)
          (list :branch-marker else-label))))

      ;; tac-call: function call (placeholder - needs more work)
      ((tac-call)
       ;; For now, just return nil - full call support needs ABI handling
       nil)

      ;; Default
      (t nil))))

(defun tac-codegen (tac-instrs allocation)
  "Generate ARM64 code from TAC with register allocation.
   Returns list of ARM64 instruction bytes.

   This is Pass 5 of the register allocation pipeline."
  (let ((code nil))
    ;; Generate code for each instruction
    (dolist (instr tac-instrs)
      (unless (eq (car instr) 'tac-label)
        ;; Generate code (skip labels - they're just markers)
        (let ((bytes (tac-codegen-instr instr allocation)))
          (when bytes
            (setq code (append code bytes))))))
    ;; Filter out branch markers (not yet resolved)
    (remove-if (lambda (x) (and (consp x)
                                 (or (eq (car x) :branch-marker)
                                     (eq (car x) :branch-ne-marker))))
               code)))

;;; ============================================================
;;; Top-Level Interface
;;; ============================================================

(defun allocate-registers-for-function (fn)
  "Apply full register allocation pipeline to a compiled function.
   fn has structure: (name params body-ir param-base)
   Returns: (name params body-ir param-base allocation tac)"
  (let* ((body-ir (caddr fn))
         (counter (make-vreg-counter))
         ;; Pass 1: IR to TAC
         (tac-result (ir-to-tac body-ir counter))
         (tac-instrs (car tac-result))
         (result-vr (cadr tac-result))
         ;; Add return instruction
         (full-tac (append tac-instrs (list (list 'tac-return result-vr))))
         ;; Pass 2: Liveness analysis
         (annotated (compute-liveness full-tac))
         ;; Pass 3: Compute intervals
         (intervals (compute-intervals annotated))
         ;; Pass 4: Linear scan allocation
         (allocation (linear-scan intervals)))
    (list (car fn) (cadr fn) (caddr fn) (cadddr fn) allocation full-tac)))

;;; Register as optimization pass
(register-optimization 'register-allocation #'allocate-registers-for-function)
