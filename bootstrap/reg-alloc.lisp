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
    ;; Literal number - raw value, tagging applied in tac-codegen
    ((numberp ir)
     (let ((vr (next-vreg counter)))
       (list (list (list 'tac-lit vr ir)) vr)))

    ;; (lit value) - literal, raw value, tagging applied in tac-codegen
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
    ;; IR format: (CALL-FN fn-name (arg1 arg2 ...))
    ;; Use caddr (not cddr) to get the args list directly
    ((and (consp ir) (ir-tag-matches (car ir) "CALL-FN"))
     (let* ((fn-name (cadr ir))
            (args (caddr ir)))
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

    ;; nil-ir: nil literal (0x06 in tagged representation)
    ((and (consp ir) (ir-tag-matches (car ir) "NIL-IR"))
     (let ((vr (next-vreg counter)))
       (list (list (list 'tac-nil vr)) vr)))

    ;; sym-lit: symbol literal
    ((and (consp ir) (ir-tag-matches (car ir) "SYM-LIT"))
     (let ((vr (next-vreg counter)))
       (list (list (list 'tac-sym vr (cadr ir))) vr)))

    ;; cons-ir: cons operation (car cdr)
    ((and (consp ir) (ir-tag-matches (car ir) "CONS-IR"))
     (let* ((car-result (ir-to-tac (cadr ir) counter))
            (car-instrs (car car-result))
            (car-vr (cadr car-result))
            (cdr-result (ir-to-tac (caddr ir) counter))
            (cdr-instrs (car cdr-result))
            (cdr-vr (cadr cdr-result))
            (result-vr (next-vreg counter)))
       (list (append car-instrs
                     cdr-instrs
                     (list (list 'tac-cons result-vr car-vr cdr-vr)))
             result-vr)))

    ;; car-ir: car operation (unary)
    ((and (consp ir) (ir-tag-matches (car ir) "CAR-IR"))
     (let* ((arg-result (ir-to-tac (cadr ir) counter))
            (arg-instrs (car arg-result))
            (arg-vr (cadr arg-result))
            (result-vr (next-vreg counter)))
       (list (append arg-instrs
                     (list (list 'tac-car result-vr arg-vr)))
             result-vr)))

    ;; cdr-ir: cdr operation (unary)
    ((and (consp ir) (ir-tag-matches (car ir) "CDR-IR"))
     (let* ((arg-result (ir-to-tac (cadr ir) counter))
            (arg-instrs (car arg-result))
            (arg-vr (cadr arg-result))
            (result-vr (next-vreg counter)))
       (list (append arg-instrs
                     (list (list 'tac-cdr result-vr arg-vr)))
             result-vr)))

    ;; setq-ir: variable assignment (setq-ir offset value)
    ((and (consp ir) (ir-tag-matches (car ir) "SETQ-IR"))
     (let* ((offset (cadr ir))
            (val-result (ir-to-tac (caddr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       ;; setq returns the assigned value
       (list (append val-instrs
                     (list (list 'tac-setvar offset val-vr)))
             val-vr)))

    ;; while-ir: while loop (while-ir cond body)
    ((and (consp ir) (ir-tag-matches (car ir) "WHILE-IR"))
     (let* ((loop-label (next-vreg counter))
            (end-label (next-vreg counter))
            (cond-result (ir-to-tac (cadr ir) counter))
            (cond-instrs (car cond-result))
            (cond-vr (cadr cond-result))
            (body-result (ir-to-tac (caddr ir) counter))
            (body-instrs (car body-result))
            (result-vr (next-vreg counter)))
       ;; while returns nil
       (list (append (list (list 'tac-label loop-label))
                     cond-instrs
                     (list (list 'tac-if-not cond-vr end-label))
                     body-instrs
                     (list (list 'tac-goto loop-label))
                     (list (list 'tac-label end-label))
                     (list (list 'tac-nil result-vr)))
             result-vr)))

    ;; str-lit: string literal
    ((and (consp ir) (ir-tag-matches (car ir) "STR-LIT"))
     (let ((vr (next-vreg counter)))
       (list (list (list 'tac-str vr (cadr ir))) vr)))

    ;; setcar-ir: mutate car of cons cell
    ((and (consp ir) (ir-tag-matches (car ir) "SETCAR-IR"))
     (let* ((cons-result (ir-to-tac (cadr ir) counter))
            (cons-instrs (car cons-result))
            (cons-vr (cadr cons-result))
            (val-result (ir-to-tac (caddr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       ;; setcar returns the value
       (list (append cons-instrs
                     val-instrs
                     (list (list 'tac-setcar cons-vr val-vr)))
             val-vr)))

    ;; setcdr-ir: mutate cdr of cons cell
    ((and (consp ir) (ir-tag-matches (car ir) "SETCDR-IR"))
     (let* ((cons-result (ir-to-tac (cadr ir) counter))
            (cons-instrs (car cons-result))
            (cons-vr (cadr cons-result))
            (val-result (ir-to-tac (caddr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       (list (append cons-instrs
                     val-instrs
                     (list (list 'tac-setcdr cons-vr val-vr)))
             val-vr)))

    ;; make-vector-ir: allocate vector
    ((and (consp ir) (ir-tag-matches (car ir) "MAKE-VECTOR-IR"))
     (let* ((size-result (ir-to-tac (cadr ir) counter))
            (size-instrs (car size-result))
            (size-vr (cadr size-result))
            (result-vr (next-vreg counter)))
       (list (append size-instrs
                     (list (list 'tac-make-vector result-vr size-vr)))
             result-vr)))

    ;; vector-ref-ir: read vector element
    ((and (consp ir) (ir-tag-matches (car ir) "VECTOR-REF-IR"))
     (let* ((vec-result (ir-to-tac (cadr ir) counter))
            (vec-instrs (car vec-result))
            (vec-vr (cadr vec-result))
            (idx-result (ir-to-tac (caddr ir) counter))
            (idx-instrs (car idx-result))
            (idx-vr (cadr idx-result))
            (result-vr (next-vreg counter)))
       (list (append vec-instrs
                     idx-instrs
                     (list (list 'tac-vector-ref result-vr vec-vr idx-vr)))
             result-vr)))

    ;; vector-set-ir: write vector element
    ((and (consp ir) (ir-tag-matches (car ir) "VECTOR-SET-IR"))
     (let* ((vec-result (ir-to-tac (cadr ir) counter))
            (vec-instrs (car vec-result))
            (vec-vr (cadr vec-result))
            (idx-result (ir-to-tac (caddr ir) counter))
            (idx-instrs (car idx-result))
            (idx-vr (cadr idx-result))
            (val-result (ir-to-tac (cadddr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       ;; vector-set returns the value
       (list (append vec-instrs
                     idx-instrs
                     val-instrs
                     (list (list 'tac-vector-set vec-vr idx-vr val-vr)))
             val-vr)))

    ;; vector-length-ir: get vector length
    ((and (consp ir) (ir-tag-matches (car ir) "VECTOR-LENGTH-IR"))
     (let* ((vec-result (ir-to-tac (cadr ir) counter))
            (vec-instrs (car vec-result))
            (vec-vr (cadr vec-result))
            (result-vr (next-vreg counter)))
       (list (append vec-instrs
                     (list (list 'tac-vector-length result-vr vec-vr)))
             result-vr)))

    ;; string-length-ir: get string length
    ((and (consp ir) (ir-tag-matches (car ir) "STRING-LENGTH-IR"))
     (let* ((str-result (ir-to-tac (cadr ir) counter))
            (str-instrs (car str-result))
            (str-vr (cadr str-result))
            (result-vr (next-vreg counter)))
       (list (append str-instrs
                     (list (list 'tac-string-length result-vr str-vr)))
             result-vr)))

    ;; string-ref-ir: read string character
    ((and (consp ir) (ir-tag-matches (car ir) "STRING-REF-IR"))
     (let* ((str-result (ir-to-tac (cadr ir) counter))
            (str-instrs (car str-result))
            (str-vr (cadr str-result))
            (idx-result (ir-to-tac (caddr ir) counter))
            (idx-instrs (car idx-result))
            (idx-vr (cadr idx-result))
            (result-vr (next-vreg counter)))
       (list (append str-instrs
                     idx-instrs
                     (list (list 'tac-string-ref result-vr str-vr idx-vr)))
             result-vr)))

    ;; make-string-from-vector-ir: create string from char vector
    ((and (consp ir) (ir-tag-matches (car ir) "MAKE-STRING-FROM-VECTOR-IR"))
     (let* ((vec-result (ir-to-tac (cadr ir) counter))
            (vec-instrs (car vec-result))
            (vec-vr (cadr vec-result))
            (result-vr (next-vreg counter)))
       (list (append vec-instrs
                     (list (list 'tac-make-string result-vr vec-vr)))
             result-vr)))

    ;; loop-ir: TCO loop (loop-ir body marker)
    ((and (consp ir) (ir-tag-matches (car ir) "LOOP-IR"))
     (let* ((loop-label (next-vreg counter))
            (body-result (ir-to-tac (cadr ir) counter))
            (body-instrs (car body-result))
            (body-vr (cadr body-result)))
       ;; Store loop label for continue-ir to reference
       ;; The marker (caddr ir) identifies this loop
       (list (append (list (list 'tac-loop-start loop-label (caddr ir)))
                     (list (list 'tac-label loop-label))
                     body-instrs)
             body-vr)))

    ;; continue-ir: jump back to loop start
    ((and (consp ir) (ir-tag-matches (car ir) "CONTINUE-IR"))
     ;; The marker identifies which loop to continue
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-continue (cadr ir))
                   (list 'tac-nil result-vr))  ; unreachable but needed
             result-vr)))

    ;; get-tag: extract tag bits from value
    ((and (consp ir) (ir-tag-matches (car ir) "GET-TAG"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result))
            (result-vr (next-vreg counter)))
       (list (append val-instrs
                     (list (list 'tac-get-tag result-vr val-vr)))
             result-vr)))

    ;; funcall-ir: call through function value
    ;; IR format: (FUNCALL-IR fn-expr (arg1 arg2 ...))
    ;; Use caddr (not cddr) to get the args list directly
    ((and (consp ir) (ir-tag-matches (car ir) "FUNCALL-IR"))
     (let* ((fn-result (ir-to-tac (cadr ir) counter))
            (fn-instrs (car fn-result))
            (fn-vr (cadr fn-result))
            (args (caddr ir)))
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
           (list (append fn-instrs
                         args-instrs
                         (list (list 'tac-funcall result-vr fn-vr arg-vrs)))
                 result-vr)))))

    ;; lambda-ir: create closure
    ((and (consp ir) (ir-tag-matches (car ir) "LAMBDA-IR"))
     ;; (lambda-ir params body-ir free-vars free-offsets)
     ;; For now, emit as opaque closure creation
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-make-closure result-vr ir)) result-vr)))

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
    ;; Instructions that define a result vreg (vreg is second element)
    ((tac-lit tac-param tac-var tac-binop tac-cmp tac-call tac-move
      tac-nil tac-cons tac-car tac-cdr tac-sym tac-str
      tac-make-vector tac-vector-ref tac-vector-length
      tac-string-length tac-string-ref tac-make-string
      tac-get-tag tac-funcall tac-make-closure)
     (cadr instr))
    (t nil)))

(defun tac-use (instr)
  "Return list of vregs used by this instruction"
  (case (car instr)
    ;; Instructions with no vreg uses
    ((tac-lit tac-param tac-var tac-label tac-goto tac-nil tac-sym tac-str
      tac-loop-start tac-continue tac-make-closure)
     nil)
    ;; Binary operations: (tac-binop dest op vr1 vr2)
    ((tac-binop tac-cmp)
     (list (cadddr instr) (nth 4 instr)))
    ;; setvar: (tac-setvar offset vreg)
    ((tac-setvar)
     (list (caddr instr)))
    ;; Conditionals: (tac-if cond-vreg then else)
    ((tac-if tac-if-not)
     (list (cadr instr)))
    ;; Return: (tac-return vreg)
    ((tac-return)
     (list (cadr instr)))
    ;; Unary ops: (tac-X dest src)
    ((tac-move tac-car tac-cdr tac-vector-length tac-string-length
      tac-make-vector tac-make-string tac-get-tag)
     (list (caddr instr)))
    ;; Cons: (tac-cons dest car cdr)
    ((tac-cons)
     (list (caddr instr) (cadddr instr)))
    ;; Mutation: (tac-setcar cons-vr val-vr), (tac-setcdr cons-vr val-vr)
    ((tac-setcar tac-setcdr)
     (list (cadr instr) (caddr instr)))
    ;; Vector ops: (tac-vector-ref dest vec idx), (tac-string-ref dest str idx)
    ((tac-vector-ref tac-string-ref)
     (list (caddr instr) (cadddr instr)))
    ;; Vector set: (tac-vector-set vec idx val) - no dest, returns val
    ((tac-vector-set)
     (list (cadr instr) (caddr instr) (cadddr instr)))
    ;; Call: (tac-call dest fn args)
    ((tac-call)
     (cadddr instr))
    ;; Funcall: (tac-funcall dest fn-vr args)
    ((tac-funcall)
     (cons (caddr instr) (cadddr instr)))
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
  '(:x9 :x10 :x11 :x12 :x13 :x14 :x15))

(defun callee-saved-regs ()
  "Callee-saved registers for values spanning calls.
   x19, x21, x22 (x20 reserved for env base)"
  '(:x19 :x21 :x22))

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
  "Look up physical register for vreg. Returns reg keyword (:x9, etc.) or (:spill slot)."
  (let ((entry (assoc vreg allocation)))
    (if entry (cdr entry) :x0)))

(defun emit-load-vreg (vreg allocation dest-reg)
  "Emit code to load vreg into dest-reg.
   If vreg is in a register, emit MOV. If spilled, emit LDR from stack."
  (let ((loc (vreg-to-reg vreg allocation)))
    (if (and (consp loc) (eq (car loc) :spill))
        ;; Spilled: load from stack
        (arm64:ldr dest-reg :sp :offset (* (cadr loc) 8))
        ;; In register: move if different
        (if (eq loc dest-reg)
            nil  ; Already in dest
            (arm64:mov dest-reg loc)))))

(defun emit-store-vreg (vreg allocation src-reg)
  "Emit code to store src-reg to vreg's location.
   If vreg is in a register, emit MOV. If spilled, emit STR to stack."
  (let ((loc (vreg-to-reg vreg allocation)))
    (if (and (consp loc) (eq (car loc) :spill))
        ;; Spilled: store to stack
        (arm64:str src-reg :sp :offset (* (cadr loc) 8))
        ;; In register: move if different
        (if (eq loc src-reg)
            nil  ; Already there
            (arm64:mov loc src-reg)))))

(defun tac-codegen-instr (instr allocation)
  "Generate ARM64 code for a single TAC instruction.
   Returns list of instruction bytes (each instruction is 4 bytes)."
  (let ((op (car instr)))
    (case op
      ;; tac-lit: load literal into vreg
      ;; Value must be tagged as fixnum (value << 4, tag 0)
      ((tac-lit)
       (let* ((vreg (cadr instr))
              (value (caddr instr))
              (tagged (ash value 4))  ; Fixnum tagging: value << 4
              (dest (vreg-to-reg vreg allocation)))
         (if (and (consp dest) (eq (car dest) :spill))
             ;; Spilled: load to x0, then store
             (append (arm64:movz :x0 (logand tagged #xFFFF))
                     (arm64:str :x0 :sp :offset (* (cadr dest) 8)))
             ;; In register
             (arm64:movz dest (logand tagged #xFFFF)))))

      ;; tac-var: load from environment
      ;; Environment is at x20, params at negative offsets from x20
      ;; Use LDUR for negative offsets (unscaled signed 9-bit)
      ((tac-var)
       (let* ((vreg (cadr instr))
              (offset (caddr instr))
              (dest (vreg-to-reg vreg allocation))
              (byte-off (* offset -8)))
         (if (and (consp dest) (eq (car dest) :spill))
             ;; Spilled: load to x0, then store
             (append (arm64:ldur :x0 :env :offset byte-off)
                     (arm64:str :x0 :sp :offset (* (cadr dest) 8)))
             ;; In register
             (arm64:ldur dest :env :offset byte-off))))

      ;; tac-setvar: store to environment
      ;; Use STUR for negative offsets
      ((tac-setvar)
       (let* ((offset (cadr instr))
              (vreg (caddr instr))
              (src (vreg-to-reg vreg allocation))
              (byte-off (* offset -8)))
         (if (and (consp src) (eq (car src) :spill))
             ;; Spilled: load to x0, then store to env
             (append (arm64:ldr :x0 :sp :offset (* (cadr src) 8))
                     (arm64:stur :x0 :env :offset byte-off))
             ;; In register
             (arm64:stur src :env :offset byte-off))))

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
              (left-reg (if (and (consp left) (eq (car left) :spill)) :x0 left))
              (right-reg (if (and (consp right) (eq (car right) :spill)) :x1 right))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          ;; Load spilled operands
          (when (and (consp left) (eq (car left) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr left) 8)))
          (when (and (consp right) (eq (car right) :spill))
            (arm64:ldr :x1 :sp :offset (* (cadr right) 8)))
          ;; Perform operation
          ;; Note: For tagged fixnums, ADD/SUB work directly since tags cancel out.
          ;; MUL needs to untag one operand: (a<<4)*(b>>4) = (a*b)<<4
          ;; DIV needs to untag both, divide, retag: ((a>>4)/(b>>4))<<4
          (case binop
            ((:ADD) (arm64:add dest-reg left-reg right-reg))
            ((:SUB) (arm64:sub dest-reg left-reg right-reg))
            ((:MUL)
             ;; Untag right operand, then multiply
             (append (arm64:lsr right-reg right-reg 4 :imm t)
                     (arm64:mul dest-reg left-reg right-reg)))
            ((:DIV)
             ;; Untag both, divide, retag result
             (append (arm64:lsr left-reg left-reg 4 :imm t)
                     (arm64:lsr right-reg right-reg 4 :imm t)
                     (arm64:sdiv dest-reg left-reg right-reg)
                     (arm64:lsl dest-reg dest-reg 4 :imm t)))
            ((:MOD)
             ;; a mod b = a - (a/b)*b, all untagged, retag at end
             ;; Use x8 as scratch (not allocatable)
             (append (arm64:lsr left-reg left-reg 4 :imm t)    ; untag a
                     (arm64:lsr right-reg right-reg 4 :imm t)  ; untag b
                     (arm64:sdiv :x8 left-reg right-reg)         ; x8 = a/b
                     (arm64:mul :x8 :x8 right-reg)                 ; x8 = (a/b)*b
                     (arm64:sub dest-reg left-reg :x8)           ; dest = a - (a/b)*b
                     (arm64:lsl dest-reg dest-reg 4 :imm t)))  ; retag
            (t (arm64:mov dest-reg :x0)))  ; Unknown op
          ;; Store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (* (cadr dest) 8))))))

      ;; tac-cmp: comparison (result is 0 or non-zero)
      ((tac-cmp)
       (let* ((dest-vreg (cadr instr))
              (cmp-op (caddr instr))
              (left-vreg (cadddr instr))
              (right-vreg (nth 4 instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (left (vreg-to-reg left-vreg allocation))
              (right (vreg-to-reg right-vreg allocation))
              (left-reg (if (and (consp left) (eq (car left) :spill)) :x0 left))
              (right-reg (if (and (consp right) (eq (car right) :spill)) :x1 right))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          ;; Load spilled operands
          (when (and (consp left) (eq (car left) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr left) 8)))
          (when (and (consp right) (eq (car right) :spill))
            (arm64:ldr :x1 :sp :offset (* (cadr right) 8)))
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
            (arm64:str :x0 :sp :offset (* (cadr dest) 8))))))

      ;; tac-move: copy between vregs
      ((tac-move)
       (let* ((dest-vreg (cadr instr))
              (src-vreg (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (src (vreg-to-reg src-vreg allocation)))
         (cond
           ;; Both spilled
           ((and (consp dest) (consp src))
            (append (arm64:ldr :x0 :sp :offset (* (cadr src) 8))
                    (arm64:str :x0 :sp :offset (* (cadr dest) 8))))
           ;; Src spilled
           ((consp src)
            (arm64:ldr dest :sp :offset (* (cadr src) 8)))
           ;; Dest spilled
           ((consp dest)
            (arm64:str src :sp :offset (* (cadr dest) 8)))
           ;; Both in registers
           ((eq dest src) nil)
           (t (arm64:mov dest src)))))

      ;; tac-return: move result to x0
      ((tac-return)
       (let* ((vreg (cadr instr))
              (src (vreg-to-reg vreg allocation)))
         (if (and (consp src) (eq (car src) :spill))
             (arm64:ldr :x0 :sp :offset (* (cadr src) 8))
             (if (eq src :x0)
                 nil  ; Already in x0
                 (arm64:mov :x0 src)))))

      ;; tac-label: no code, just record position
      ((tac-label) nil)

      ;; tac-goto: unconditional branch (resolved later)
      ((tac-goto)
       (list (list :branch-marker (cadr instr))))

      ;; tac-if: conditional branch
      ((tac-if)
       (let* ((cond-vreg (cadr instr))
              (then-label (caddr instr))
              (else-label (cadddr instr))
              (cond-loc (vreg-to-reg cond-vreg allocation))
              (cond-reg (if (and (consp cond-loc) (eq (car cond-loc) :spill)) :x0 cond-loc)))
         (append
          ;; Load condition if spilled
          (when (and (consp cond-loc) (eq (car cond-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr cond-loc) 8)))
          ;; Compare with nil (0x06)
          (arm64:cmp cond-reg #x06 :imm t)
          ;; Branch markers (resolved in second pass)
          ;; Use (list (list ...)) so append doesn't flatten the marker
          (list (list :branch-ne-marker then-label))
          (list (list :branch-marker else-label)))))

      ;; tac-call: function call with ABI handling
      ;; Format: (tac-call dest-vreg fn-name (arg-vregs...))
      ((tac-call)
       (let* ((dest-vreg (cadr instr))
              (fn-name (caddr instr))
              (arg-vregs (cadddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              ;; Save caller-saved registers x9-x15 that are in use
              ;; (simplified: save all allocatable regs for now)
              (save-regs '(:x9 :x10 :x11 :x12 :x13 :x14 :x15))
              ;; Generate saves to stack at offsets 16+ (0,8 for x30,x20)
              (save-code nil)
              (save-offset 16))
         ;; Save caller-saved registers
         (dolist (reg save-regs)
           (setq save-code (append save-code
                                   (arm64:str reg :sp :offset save-offset)))
           (setq save-offset (+ save-offset 8)))
         ;; Move args to x0-x7
         (let ((arg-code nil)
               (arg-idx 0)
               (arg-regs '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7)))
           (dolist (arg-vr arg-vregs)
             (when (< arg-idx 8)
               (let ((arg-loc (vreg-to-reg arg-vr allocation))
                     (arg-reg (nth arg-idx arg-regs)))
                 (if (and (consp arg-loc) (eq (car arg-loc) :spill))
                     ;; Load from spill slot to arg register
                     (setq arg-code (append arg-code
                                            (arm64:ldr arg-reg :sp :offset (* (cadr arg-loc) 8))))
                     ;; Move from allocated reg to arg register
                     (unless (eq arg-loc arg-reg)
                       (setq arg-code (append arg-code (arm64:mov arg-reg arg-loc))))))
               (setq arg-idx (+ arg-idx 1))))
           ;; Generate call marker (resolved by resolve-calls)
           (let ((call-marker (list (list :call-fn fn-name)))
                 ;; Restore caller-saved registers
                 (restore-code nil)
                 (restore-offset 16))
             (dolist (reg save-regs)
               (setq restore-code (append restore-code
                                          (arm64:ldr reg :sp :offset restore-offset)))
               (setq restore-offset (+ restore-offset 8)))
             ;; Move result from x0 to dest
             (let ((result-code
                     (if (and (consp dest) (eq (car dest) :spill))
                         (arm64:str :x0 :sp :offset (* (cadr dest) 8))
                         (if (eq dest :x0)
                             nil
                             (arm64:mov dest :x0)))))
               (append save-code arg-code call-marker restore-code result-code))))))

      ;; tac-nil: load nil (0x06) into vreg
      ((tac-nil)
       (let* ((vreg (cadr instr))
              (dest (vreg-to-reg vreg allocation)))
         (if (and (consp dest) (eq (car dest) :spill))
             (append (arm64:movz :x0 6)
                     (arm64:str :x0 :sp :offset (* (cadr dest) 8)))
             (arm64:movz dest 6))))

      ;; tac-cons: allocate cons cell (needs heap allocation)
      ((tac-cons)
       (let* ((dest-vreg (cadr instr))
              (car-vreg (caddr instr))
              (cdr-vreg (cadddr instr))
              (car-loc (vreg-to-reg car-vreg allocation))
              (cdr-loc (vreg-to-reg cdr-vreg allocation))
              (dest (vreg-to-reg dest-vreg allocation))
              (car-reg (if (and (consp car-loc) (eq (car car-loc) :spill)) :x0 car-loc))
              (cdr-reg (if (and (consp cdr-loc) (eq (car cdr-loc) :spill)) :x1 cdr-loc))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x2 dest)))
         (append
          ;; Load spilled car/cdr
          (when (and (consp car-loc) (eq (car car-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr car-loc) 8)))
          (when (and (consp cdr-loc) (eq (car cdr-loc) :spill))
            (arm64:ldr :x1 :sp :offset (* (cadr cdr-loc) 8)))
          ;; Store car at [x28]
          (arm64:str car-reg :heap :offset 0)
          ;; Store cdr at [x28+8]
          (arm64:str cdr-reg :heap :offset 8)
          ;; Result = x28 | 1 (cons tag)
          (arm64:orr dest-reg :heap 1 :imm t)
          ;; Bump heap pointer
          (arm64:add :heap :heap 16 :imm t)
          ;; Store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x2 :sp :offset (* (cadr dest) 8))))))

      ;; tac-car: extract car from cons cell
      ((tac-car)
       (let* ((dest-vreg (cadr instr))
              (src-vreg (caddr instr))
              (src-loc (vreg-to-reg src-vreg allocation))
              (dest (vreg-to-reg dest-vreg allocation))
              (src-reg (if (and (consp src-loc) (eq (car src-loc) :spill)) :x0 src-loc))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          ;; Load if spilled
          (when (and (consp src-loc) (eq (car src-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr src-loc) 8)))
          ;; Clear tag bits to get base address
          (arm64:and* dest-reg src-reg -16 :imm t)
          ;; Load car from [base]
          (arm64:ldr dest-reg dest-reg :offset 0)
          ;; Store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (* (cadr dest) 8))))))

      ;; tac-cdr: extract cdr from cons cell
      ((tac-cdr)
       (let* ((dest-vreg (cadr instr))
              (src-vreg (caddr instr))
              (src-loc (vreg-to-reg src-vreg allocation))
              (dest (vreg-to-reg dest-vreg allocation))
              (src-reg (if (and (consp src-loc) (eq (car src-loc) :spill)) :x0 src-loc))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          ;; Load if spilled
          (when (and (consp src-loc) (eq (car src-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr src-loc) 8)))
          ;; Clear tag bits to get base address
          (arm64:and* dest-reg src-reg -16 :imm t)
          ;; Load cdr from [base+8]
          (arm64:ldr dest-reg dest-reg :offset 8)
          ;; Store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (* (cadr dest) 8))))))

      ;; tac-if-not: conditional branch on nil
      ((tac-if-not)
       (let* ((cond-vreg (cadr instr))
              (target-label (caddr instr))
              (cond-loc (vreg-to-reg cond-vreg allocation))
              (cond-reg (if (and (consp cond-loc) (eq (car cond-loc) :spill)) :x0 cond-loc)))
         (append
          ;; Load condition if spilled
          (when (and (consp cond-loc) (eq (car cond-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr cond-loc) 8)))
          ;; Compare with nil (0x06)
          (arm64:cmp cond-reg #x06 :imm t)
          ;; Branch if equal to nil
          ;; Use nested list so append doesn't flatten the marker
          (list (list :branch-eq-marker target-label)))))

      ;; tac-setcar: mutate car of cons
      ((tac-setcar)
       (let* ((cons-vreg (cadr instr))
              (val-vreg (caddr instr))
              (cons-loc (vreg-to-reg cons-vreg allocation))
              (val-loc (vreg-to-reg val-vreg allocation))
              (cons-reg (if (and (consp cons-loc) (eq (car cons-loc) :spill)) :x0 cons-loc))
              (val-reg (if (and (consp val-loc) (eq (car val-loc) :spill)) :x1 val-loc)))
         (append
          ;; Load spilled values
          (when (and (consp cons-loc) (eq (car cons-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr cons-loc) 8)))
          (when (and (consp val-loc) (eq (car val-loc) :spill))
            (arm64:ldr :x1 :sp :offset (* (cadr val-loc) 8)))
          ;; Clear tag to get base address into x2
          (arm64:and* :x2 cons-reg -16 :imm t)
          ;; Store value at [base]
          (arm64:str val-reg :x2 :offset 0))))

      ;; tac-setcdr: mutate cdr of cons
      ((tac-setcdr)
       (let* ((cons-vreg (cadr instr))
              (val-vreg (caddr instr))
              (cons-loc (vreg-to-reg cons-vreg allocation))
              (val-loc (vreg-to-reg val-vreg allocation))
              (cons-reg (if (and (consp cons-loc) (eq (car cons-loc) :spill)) :x0 cons-loc))
              (val-reg (if (and (consp val-loc) (eq (car val-loc) :spill)) :x1 val-loc)))
         (append
          (when (and (consp cons-loc) (eq (car cons-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr cons-loc) 8)))
          (when (and (consp val-loc) (eq (car val-loc) :spill))
            (arm64:ldr :x1 :sp :offset (* (cadr val-loc) 8)))
          ;; Clear tag to get base address into x2
          (arm64:and* :x2 cons-reg -16 :imm t)
          ;; Store value at [base+8]
          (arm64:str val-reg :x2 :offset 8))))

      ;; tac-get-tag: extract tag bits (value & 0xF)
      ((tac-get-tag)
       (let* ((dest-vreg (cadr instr))
              (src-vreg (caddr instr))
              (src-loc (vreg-to-reg src-vreg allocation))
              (dest (vreg-to-reg dest-vreg allocation))
              (src-reg (if (and (consp src-loc) (eq (car src-loc) :spill)) :x0 src-loc))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (when (and (consp src-loc) (eq (car src-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr src-loc) 8)))
          ;; AND with 0xF to get tag
          (arm64:and* dest-reg src-reg #xF :imm t)
          ;; Shift left 4 to make it a tagged fixnum
          (arm64:lsl dest-reg dest-reg 4 :imm t)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (* (cadr dest) 8))))))

      ;; tac-vector-length: get vector length from header
      ((tac-vector-length)
       (let* ((dest-vreg (cadr instr))
              (vec-vreg (caddr instr))
              (vec-loc (vreg-to-reg vec-vreg allocation))
              (dest (vreg-to-reg dest-vreg allocation))
              (vec-reg (if (and (consp vec-loc) (eq (car vec-loc) :spill)) :x0 vec-loc))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (when (and (consp vec-loc) (eq (car vec-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr vec-loc) 8)))
          ;; Clear tag to get base address
          (arm64:and* dest-reg vec-reg -16 :imm t)
          ;; Load length from header (first word)
          (arm64:ldr dest-reg dest-reg :offset 0)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (* (cadr dest) 8))))))

      ;; tac-string-length: same as vector-length (strings have length header)
      ((tac-string-length)
       (let* ((dest-vreg (cadr instr))
              (str-vreg (caddr instr))
              (str-loc (vreg-to-reg str-vreg allocation))
              (dest (vreg-to-reg dest-vreg allocation))
              (str-reg (if (and (consp str-loc) (eq (car str-loc) :spill)) :x0 str-loc))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (when (and (consp str-loc) (eq (car str-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr str-loc) 8)))
          ;; Clear tag to get base address
          (arm64:and* dest-reg str-reg -16 :imm t)
          ;; Load length from header
          (arm64:ldr dest-reg dest-reg :offset 0)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (* (cadr dest) 8))))))

      ;; tac-vector-ref: load vector element
      ((tac-vector-ref)
       (let* ((dest-vreg (cadr instr))
              (vec-vreg (caddr instr))
              (idx-vreg (cadddr instr))
              (vec-loc (vreg-to-reg vec-vreg allocation))
              (idx-loc (vreg-to-reg idx-vreg allocation))
              (dest (vreg-to-reg dest-vreg allocation))
              (vec-reg (if (and (consp vec-loc) (eq (car vec-loc) :spill)) :x0 vec-loc))
              (idx-reg (if (and (consp idx-loc) (eq (car idx-loc) :spill)) :x1 idx-loc))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (when (and (consp vec-loc) (eq (car vec-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr vec-loc) 8)))
          (when (and (consp idx-loc) (eq (car idx-loc) :spill))
            (arm64:ldr :x1 :sp :offset (* (cadr idx-loc) 8)))
          ;; Clear vector tag to get base
          (arm64:and* :x2 vec-reg -16 :imm t)
          ;; Index is tagged fixnum, use as byte offset (already * 16, need * 8)
          ;; Actually: idx >> 4 gives index, * 8 for word offset, + 8 for header
          ;; Simpler: idx >> 1 gives byte offset, + 8 for header
          (arm64:lsr :x3 idx-reg 1 :imm t)
          (arm64:add :x2 :x2 :x3)
          ;; Load from [base + 8] (skip header)
          (arm64:ldr dest-reg :x2 :offset 8)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (* (cadr dest) 8))))))

      ;; tac-vector-set: store vector element
      ((tac-vector-set)
       (let* ((vec-vreg (cadr instr))
              (idx-vreg (caddr instr))
              (val-vreg (cadddr instr))
              (vec-loc (vreg-to-reg vec-vreg allocation))
              (idx-loc (vreg-to-reg idx-vreg allocation))
              (val-loc (vreg-to-reg val-vreg allocation))
              (vec-reg (if (and (consp vec-loc) (eq (car vec-loc) :spill)) :x0 vec-loc))
              (idx-reg (if (and (consp idx-loc) (eq (car idx-loc) :spill)) :x1 idx-loc))
              (val-reg (if (and (consp val-loc) (eq (car val-loc) :spill)) :x2 val-loc)))
         (append
          (when (and (consp vec-loc) (eq (car vec-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr vec-loc) 8)))
          (when (and (consp idx-loc) (eq (car idx-loc) :spill))
            (arm64:ldr :x1 :sp :offset (* (cadr idx-loc) 8)))
          (when (and (consp val-loc) (eq (car val-loc) :spill))
            (arm64:ldr :x2 :sp :offset (* (cadr val-loc) 8)))
          ;; Clear vector tag
          (arm64:and* :x3 vec-reg -16 :imm t)
          ;; Compute element address
          (arm64:lsr :x4 idx-reg 1 :imm t)
          (arm64:add :x3 :x3 :x4)
          ;; Store at [base + 8]
          (arm64:str val-reg :x3 :offset 8))))

      ;; tac-string-ref: load string character (byte)
      ((tac-string-ref)
       (let* ((dest-vreg (cadr instr))
              (str-vreg (caddr instr))
              (idx-vreg (cadddr instr))
              (str-loc (vreg-to-reg str-vreg allocation))
              (idx-loc (vreg-to-reg idx-vreg allocation))
              (dest (vreg-to-reg dest-vreg allocation))
              (str-reg (if (and (consp str-loc) (eq (car str-loc) :spill)) :x0 str-loc))
              (idx-reg (if (and (consp idx-loc) (eq (car idx-loc) :spill)) :x1 idx-loc))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (when (and (consp str-loc) (eq (car str-loc) :spill))
            (arm64:ldr :x0 :sp :offset (* (cadr str-loc) 8)))
          (when (and (consp idx-loc) (eq (car idx-loc) :spill))
            (arm64:ldr :x1 :sp :offset (* (cadr idx-loc) 8)))
          ;; Clear string tag
          (arm64:and* :x2 str-reg -16 :imm t)
          ;; Index >> 4 gives actual index, + 8 for header
          (arm64:lsr :x3 idx-reg 4 :imm t)
          (arm64:add :x2 :x2 :x3)
          ;; Load byte
          (arm64:ldrb dest-reg :x2 8)
          ;; Tag as fixnum
          (arm64:lsl dest-reg dest-reg 4 :imm t)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (* (cadr dest) 8))))))

      ;; tac-loop-start: marker for loop, no code generated
      ((tac-loop-start) nil)

      ;; tac-continue: jump back to loop - need loop label tracking
      ;; For now, emit as unresolved marker
      ((tac-continue)
       (list :continue-marker (cadr instr)))

      ;; tac-funcall: call through closure - complex, delegate to runtime helper
      ;; Format: (tac-funcall dest fn-vr arg-vrs)
      ((tac-funcall)
       ;; For now, emit as marker for later resolution
       ;; Full implementation needs closure unpacking
       (list :funcall-marker instr))

      ;; tac-make-closure: create closure object - complex
      ((tac-make-closure)
       ;; Emit as marker for later resolution
       (list :make-closure-marker instr))

      ;; tac-make-vector, tac-make-string: heap allocation - emit markers
      ((tac-make-vector tac-make-string)
       (list :heap-alloc-marker instr))

      ;; Default
      (t nil))))

(defun count-bytes-in-code (code)
  "Count bytes in code, skipping markers and labels"
  (let ((count 0))
    (dolist (item code)
      (cond
        ;; Markers are not instructions
        ((and (consp item)
              (member (car item) '(:branch-marker :branch-ne-marker :branch-eq-marker :label-marker)))
         nil)
        ;; Regular byte
        ((numberp item)
         (setq count (+ count 1)))))
    count))

(defun tac-codegen (tac-instrs allocation)
  "Generate ARM64 code from TAC with register allocation.
   Returns list of ARM64 instruction bytes.

   This is Pass 5 of the register allocation pipeline.
   Uses two-pass approach:
   Pass 1: Generate code with markers, track label positions
   Pass 2: Resolve branch markers to actual branch instructions"
  ;; Pass 1: Generate code with markers
  (let ((code-with-markers nil)
        (label-positions nil)  ; alist of (label . byte-position)
        (current-pos 0))
    ;; Generate code for each instruction, tracking positions
    (dolist (instr tac-instrs)
      (if (eq (car instr) 'tac-label)
          ;; Record label position
          (let ((label (cadr instr)))
            (setq label-positions (cons (cons label current-pos) label-positions))
            (setq code-with-markers (append code-with-markers
                                            (list (list :label-marker label)))))
          ;; Generate code for instruction
          (let ((bytes (tac-codegen-instr instr allocation)))
            (when bytes
              (setq code-with-markers (append code-with-markers bytes))
              ;; Update position: count actual bytes AND branch markers (4 bytes each)
              (dolist (b bytes)
                (cond
                  ((numberp b)
                   (setq current-pos (+ current-pos 1)))
                  ;; Branch markers will become 4-byte instructions
                  ((and (consp b) (member (car b) '(:branch-marker :branch-ne-marker :branch-eq-marker)))
                   (setq current-pos (+ current-pos 4)))))))))

    ;; Pass 2: Resolve branch markers to actual instructions
    (let ((resolved nil)
          (pos 0))
      (dolist (item code-with-markers)
        (cond
          ;; Skip label markers in output
          ((and (consp item) (eq (car item) :label-marker))
           nil)

          ;; Resolve unconditional branch
          ((and (consp item) (eq (car item) :branch-marker))
           (let* ((target-label (cadr item))
                  (target-pos (cdr (assoc target-label label-positions)))
                  ;; Offset in instructions (4 bytes each), from NEXT instruction
                  (offset (if target-pos
                              (ash (- target-pos (+ pos 4)) -2)
                              0)))
             (setq resolved (append resolved (arm64:b offset)))
             (setq pos (+ pos 4))))

          ;; Resolve conditional branch (branch if not equal)
          ((and (consp item) (eq (car item) :branch-ne-marker))
           (let* ((target-label (cadr item))
                  (target-pos (cdr (assoc target-label label-positions)))
                  (offset (if target-pos
                              (ash (- target-pos (+ pos 4)) -2)
                              0)))
             (setq resolved (append resolved (arm64:b.ne offset)))
             (setq pos (+ pos 4))))

          ;; Resolve conditional branch (branch if equal)
          ((and (consp item) (eq (car item) :branch-eq-marker))
           (let* ((target-label (cadr item))
                  (target-pos (cdr (assoc target-label label-positions)))
                  (offset (if target-pos
                              (ash (- target-pos (+ pos 4)) -2)
                              0)))
             (setq resolved (append resolved (arm64:b.eq offset)))
             (setq pos (+ pos 4))))

          ;; Function call marker - pass through for resolve-calls
          ((and (consp item) (eq (car item) :call-fn))
           (setq resolved (append resolved (list item)))
           (setq pos (+ pos 4)))  ; BL is 4 bytes

          ;; Regular byte - keep it
          ((numberp item)
           (setq resolved (append resolved (list item)))
           (setq pos (+ pos 1)))

          ;; Unknown - skip
          (t nil)))
      resolved)))

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

;;; ============================================================
;;; Register-Allocated Code Generation
;;; ============================================================
;;;
;;; These functions provide an alternate code generation path using
;;; the register allocator instead of the accumulator-based codegen.

#+sbcl
(defun reg-alloc-prologue ()
  "Generate function prologue for register-allocated functions.
   Saves x30 (link register) and sets up minimal frame.
   Uses smaller frame than accumulator codegen (256 bytes vs 2KB)."
  (append
   ;; sub sp, sp, #256 - allocate frame (enough for 32 spill slots)
   (arm64:sub :sp :sp #x100 :imm t)
   ;; str x30, [sp, #0] - save link register
   (arm64:str :lr :sp :offset 0)
   ;; str x20, [sp, #8] - save x20 (env base)
   (arm64:str :env :sp :offset 8)
   ;; add x20, sp, #128 - set env base in middle of frame
   (arm64:add :env :sp #x80 :imm t)))

#+sbcl
(defun reg-alloc-epilogue ()
  "Generate function epilogue for register-allocated functions."
  (append
   ;; ldr x30, [sp, #0] - restore link register
   (arm64:ldr :lr :sp :offset 0)
   ;; ldr x20, [sp, #8] - restore x20
   (arm64:ldr :env :sp :offset 8)
   ;; add sp, sp, #256 - deallocate frame
   (arm64:add :sp :sp #x100 :imm t)
   ;; ret
   (arm64:ret)))

#+sbcl
(defun reg-alloc-gen-param-stores (params base-offset)
  "Generate code to store parameters from x0-x7 to stack.
   For register-allocated code, params are stored at [x20 - offset*8].
   Uses STUR for negative offsets (unscaled signed 9-bit)."
  (labels ((gen-stores (ps idx acc)
             (if (null ps)
                 acc
                 (let* ((off (* (+ base-offset idx) -8))
                        (reg-kw (arm64:num-to-reg idx))
                        ;; Use STUR for negative offsets, STR for positive
                        (store (if (< off 0)
                                   (arm64:stur reg-kw :env :offset off)
                                   (arm64:str reg-kw :env :offset off))))
                   (gen-stores (cdr ps) (+ idx 1) (append acc store))))))
    (gen-stores params 0 nil)))

#+sbcl
(defun has-unresolved-markers (code)
  "Check if code contains any unresolved markers.
   These indicate IR that can't be fully compiled by reg-alloc yet.
   Note: :call-fn markers would need resolution by the main flatten machinery,
   so we fall back to regular codegen for any function with calls."
  (labels ((check (items)
             (cond
               ((null items) nil)
               ((and (consp (car items))
                     (member (caar items)
                             '(:call-fn           ; function calls need resolution
                               :funcall-marker    ; closure calls
                               :make-closure-marker
                               :heap-alloc-marker
                               :continue-marker)))
                t)
               (t (check (cdr items))))))
    (check code)))

#+sbcl
(defun codegen-fn-reg-alloc (fn)
  "Generate code for a function using register allocation.
   Accepts two formats:
   - Defun: (name params body-ir param-base) - param-base is a number
   - Lambda: (name params body-ir free-vars) - free-vars is a list or nil
   Returns list of ARM64 instruction bytes, or nil if IR not fully supported."
  (let* ((params (cadr fn))
         (body-ir (caddr fn))
         (fourth (cadddr fn))
         ;; Detect format: number = param-base (defun), list = free-vars (lambda)
         (param-base (if (numberp fourth)
                         fourth
                         (if fourth (length fourth) 0)))
         ;; Apply register allocation pipeline
         (counter (make-vreg-counter))
         (tac-result (ir-to-tac body-ir counter))
         (tac-instrs (car tac-result))
         (result-vr (cadr tac-result)))
    ;; Check if IR converted successfully
    (if (null tac-instrs)
        ;; Fall back to regular codegen for unsupported IR
        nil
        (let* (;; Add return instruction
               (full-tac (append tac-instrs (list (list 'tac-return result-vr))))
               ;; Liveness analysis
               (annotated (compute-liveness full-tac))
               ;; Compute intervals
               (intervals (compute-intervals annotated))
               ;; Linear scan allocation
               (allocation (linear-scan intervals))
               ;; Generate prologue
               (prologue-code (reg-alloc-prologue))
               ;; Generate param stores
               (param-code (reg-alloc-gen-param-stores params param-base))
               ;; Generate body code with allocation
               (body-code (tac-codegen full-tac allocation))
               ;; Generate epilogue
               (epilogue-code (reg-alloc-epilogue))
               ;; Combine all code
               (all-code (append prologue-code param-code body-code epilogue-code)))
          ;; Check for unresolved markers - if present, fall back to regular codegen
          (if (has-unresolved-markers all-code)
              nil
              all-code)))))

#+sbcl
(defun compile-expr-reg-alloc (ir)
  "Compile a single expression using register allocation.
   Returns list of ARM64 instruction bytes, or nil if IR not supported.
   Useful for testing the register allocator on simple expressions."
  (let* ((counter (make-vreg-counter))
         (tac-result (ir-to-tac ir counter))
         (tac-instrs (car tac-result))
         (result-vr (cadr tac-result)))
    (if (null tac-instrs)
        nil
        (let* ((full-tac (append tac-instrs (list (list 'tac-return result-vr))))
               (annotated (compute-liveness full-tac))
               (intervals (compute-intervals annotated))
               (allocation (linear-scan intervals)))
          (tac-codegen full-tac allocation)))))

;;; ============================================================
;;; Debug Tools for Register Allocator
;;; ============================================================
;;;
;;; These tools help visualize and debug the register allocation pipeline.
;;; All are SBCL-only (used during development, not in native binaries).

#+sbcl
(defun print-tac (tac-instrs &optional (stream t))
  "Pretty-print TAC instructions with line numbers."
  (format stream "~%TAC Instructions:~%")
  (let ((i 0))
    (dolist (instr tac-instrs)
      (format stream "  ~3D: ~S~%" i instr)
      (incf i))))

#+sbcl
(defun print-liveness (annotated &optional (stream t))
  "Print liveness analysis results."
  (format stream "~%Liveness Analysis:~%")
  (format stream "  ~4A  ~30A ~20A ~20A~%" "IDX" "INSTRUCTION" "LIVE-IN" "LIVE-OUT")
  (format stream "  ~4,,,'-A  ~30,,,'-A ~20,,,'-A ~20,,,'-A~%" "" "" "" "")
  (let ((i 0))
    (dolist (entry annotated)
      (format stream "  ~4D  ~30S ~20S ~20S~%"
              i (car entry) (cadr entry) (caddr entry))
      (incf i))))

#+sbcl
(defun print-intervals (intervals &optional (stream t))
  "Print live intervals with ASCII visualization."
  (format stream "~%Live Intervals:~%")
  (let ((max-end (reduce #'max intervals :key #'caddr :initial-value 0)))
    (dolist (interval (sort (copy-list intervals) #'< :key #'cadr))
      (let* ((vreg (car interval))
             (start (cadr interval))
             (end (caddr interval))
             (bar (make-string (+ max-end 2) :initial-element #\Space)))
        (loop for i from start to end do
          (setf (char bar i) (if (= i start) #\[ (if (= i end) #\] #\-))))
        (format stream "  v~2D: ~A [~D-~D]~%" vreg bar start end)))))

#+sbcl
(defun print-allocation (allocation &optional (stream t))
  "Print register allocation results."
  (format stream "~%Register Allocation:~%")
  (dolist (entry (sort (copy-list allocation) #'<
                       :key (lambda (e) (if (numberp (car e)) (car e) 0))))
    (let ((vreg (car entry))
          (loc (cdr entry)))
      (if (and (consp loc) (eq (car loc) :spill))
          (format stream "  v~D -> spill[~D]~%" vreg (cadr loc))
          (format stream "  v~D -> x~D~%" vreg loc)))))

#+sbcl
(defun reg-alloc-debug (ir &optional (stream t))
  "Run register allocation pipeline with full debug output.

   Usage: (reg-alloc-debug '(add (lit 1) (mul (lit 2) (lit 3))))

   Shows all pipeline stages: IR -> TAC -> Liveness -> Intervals -> Allocation"
  (format stream "~%========================================~%")
  (format stream "Register Allocation Debug~%")
  (format stream "========================================~%")

  (format stream "~%Input IR: ~S~%" ir)

  ;; Pass 1: IR to TAC
  (let* ((counter (make-vreg-counter))
         (tac-result (ir-to-tac ir counter))
         (tac-instrs (car tac-result))
         (result-vr (cadr tac-result)))

    (if (null tac-instrs)
        (progn
          (format stream "~%WARNING: ir-to-tac returned nil!~%")
          (format stream "This IR type may not be supported yet.~%")
          nil)
        (progn
          ;; Add return instruction
          (let ((full-tac (append tac-instrs
                                  (list (list 'tac-return result-vr)))))

            (format stream "~%--- Pass 1: IR to TAC ---")
            (print-tac full-tac stream)
            (format stream "Result vreg: v~D~%" result-vr)

            ;; Pass 2: Liveness analysis
            (let ((annotated (compute-liveness full-tac)))
              (format stream "~%--- Pass 2: Liveness Analysis ---")
              (print-liveness annotated stream)

              ;; Pass 3: Compute intervals
              (let ((intervals (compute-intervals annotated)))
                (format stream "~%--- Pass 3: Live Intervals ---")
                (print-intervals intervals stream)

                ;; Pass 4: Linear scan
                (let ((allocation (linear-scan intervals)))
                  (format stream "~%--- Pass 4: Register Allocation ---")
                  (print-allocation allocation stream)

                  (format stream "~%========================================~%")

                  ;; Return the results for further inspection
                  (list :tac full-tac
                        :liveness annotated
                        :intervals intervals
                        :allocation allocation)))))))))

#+sbcl
(defun check-ir-coverage (ir)
  "Check which IR types are used but not handled by ir-to-tac.
   Returns list of unhandled IR tags found in the input."
  (let ((unhandled nil)
        (handled '("LIT" "VAR" "ADD" "SUB" "MUL" "DIV" "MOD"
                   "CMP-EQ" "CMP-NE" "CMP-LT" "CMP-LE" "CMP-GT" "CMP-GE"
                   "IF-IR" "LET-IR" "PROGN-IR" "CALL-FN"
                   ;; Extended IR types now handled:
                   "NIL-IR" "CONS-IR" "CAR-IR" "CDR-IR" "SYM-LIT" "STR-LIT"
                   "SETQ-IR" "WHILE-IR" "SETCAR-IR" "SETCDR-IR"
                   "MAKE-VECTOR-IR" "VECTOR-REF-IR" "VECTOR-SET-IR" "VECTOR-LENGTH-IR"
                   "STRING-LENGTH-IR" "STRING-REF-IR" "MAKE-STRING-FROM-VECTOR-IR"
                   "LOOP-IR" "CONTINUE-IR" "GET-TAG" "FUNCALL-IR" "LAMBDA-IR")))
    (labels ((check-node (node)
               (when (consp node)
                 (let ((tag (ir-tag-name (car node))))
                   (unless (or (string= tag "")
                               (member tag handled :test #'string-equal)
                               (member tag unhandled :test #'string-equal))
                     (push tag unhandled)))
                 ;; Recurse into children
                 (dolist (child (cdr node))
                   (check-node child)))))
      (check-node ir)
      (reverse unhandled))))

#+sbcl
(defun test-reg-alloc ()
  "Run some test cases through the register allocator."
  (format t "~%=== Test 1: Simple add ===~%")
  (reg-alloc-debug '(add (lit 16) (lit 32)))

  (format t "~%=== Test 2: Nested operations ===~%")
  (reg-alloc-debug '(add (lit 16) (mul (lit 32) (lit 48))))

  (format t "~%=== Test 3: Variable reference ===~%")
  (reg-alloc-debug '(add (var 0) (var 1)))

  (format t "~%=== Test 4: If expression ===~%")
  (reg-alloc-debug '(if-ir (cmp-eq (var 0) (lit 0))
                           (lit 16)
                           (lit 32)))

  (format t "~%Done with register allocator tests.~%")
  t)

;;; ============================================================
;;; Diagnostic Tools for Register Allocator
;;; ============================================================

#+sbcl
(defun disassemble-bytes (bytes &optional (stream t))
  "Disassemble ARM64 bytes to readable instructions.
   BYTES is a list of bytes in big-endian order (as emitted by codegen)."
  (format stream "~%ARM64 Disassembly (~D bytes):~%" (length bytes))
  (format stream "~4A  ~8A    ~A~%" "OFF" "HEX" "INSTRUCTION")
  (format stream "~4,,,'-A  ~8,,,'-A    ~,,,'-A~%" "" "" "")
  (let ((i 0))
    (loop while (< i (- (length bytes) 3)) do
      (let* ((b0 (nth i bytes))
             (b1 (nth (+ i 1) bytes))
             (b2 (nth (+ i 2) bytes))
             (b3 (nth (+ i 3) bytes))
             ;; ARM64 is little-endian; our bytes are big-endian
             (instr (logior (ash b3 24) (ash b2 16) (ash b1 8) b0)))
        (format stream "~4X  ~8,'0X    ~A~%"
                i instr (decode-arm64-instr instr i))
        (incf i 4)))))

#+sbcl
(defun disasm (fn)
  "Disassemble FN to *standard-output*.
   FN: extended function designator - function, symbol naming a function,
       or lambda expression.
   Returns NIL.

   Extensions: also accepts pathname/string for binary files."
  (cond
    ;; Function object - not yet supported in Habu
    ((functionp fn)
     (format t "~%; Function objects not yet supported~%"))
    ;; Symbol - look up function definition
    ((symbolp fn)
     (let ((def (get fn 'function-definition)))
       (if def
           (let ((code (compile-program (list def) nil)))
             (disassemble-bytes code t))
           (format t "~%; No definition found for ~S~%" fn))))
    ;; Lambda expression - compile and disassemble
    ((and (listp fn) (eq (car fn) 'lambda))
     (let ((code (compile-program (list fn) nil)))
       (disassemble-bytes code t)))
    ;; Extension: pathname or string - read binary file
    ((or (pathnamep fn) (stringp fn))
     (let ((path (if (stringp fn) (pathname fn) fn)))
       (with-open-file (f path :element-type '(unsigned-byte 8))
         (let* ((size (file-length f))
                (bytes (make-array size :element-type '(unsigned-byte 8))))
           (read-sequence bytes f)
           ;; Skip Mach-O header to __text section
           (let ((code-start #x328)
                 (result nil))
             (loop for i from code-start below size
                   do (push (aref bytes i) result))
             (disassemble-bytes (nreverse result) t))))))
    (t
     (error "Cannot disassemble ~S" fn)))
  nil)

#+sbcl
(defun reg-name (n)
  "Format register name like lldb does."
  (case n
    (31 "sp")
    (30 "lr")
    (29 "fp")
    (t (format nil "x~D" n))))

#+sbcl
(defun decode-arm64-instr (instr &optional offset)
  "Decode ARM64 instruction to readable string matching lldb format.
   Handles all instructions used by Habu codegen."
  (declare (ignorable offset))
  (let* ((rd (logand instr #x1F))
         (rn (logand (ash instr -5) #x1F))
         (rm (logand (ash instr -16) #x1F)))
    (cond
      ;; === Data Movement ===

      ;; MOVZ: 110100101 hw imm16 Rd
      ((= (logand instr #xFF800000) #xD2800000)
       (let* ((hw (logand (ash instr -21) #x3))
              (imm16 (logand (ash instr -5) #xFFFF))
              (shift (* hw 16)))
         (if (zerop shift)
             (format nil "mov    ~A, #0x~X" (reg-name rd) imm16)
             (format nil "movz   ~A, #0x~X, lsl #~D" (reg-name rd) imm16 shift))))

      ;; MOVK: 111100101 hw imm16 Rd
      ((= (logand instr #xFF800000) #xF2800000)
       (let* ((hw (logand (ash instr -21) #x3))
              (imm16 (logand (ash instr -5) #xFFFF))
              (shift (* hw 16)))
         (format nil "movk   ~A, #0x~X, lsl #~D" (reg-name rd) imm16 shift)))

      ;; MOV (register, ORR with XZR): 10101010000 Rm 000000 11111 Rd
      ((= (logand instr #xFFE0FFE0) #xAA0003E0)
       (format nil "mov    ~A, ~A" (reg-name rd) (reg-name rm)))

      ;; === Arithmetic ===

      ;; ADD immediate: 1001000100 sh imm12 Rn Rd
      ((= (logand instr #xFF000000) #x91000000)
       (let ((imm12 (logand (ash instr -10) #xFFF))
             (sh (logbitp 22 instr)))
         (if sh
             (format nil "add    ~A, ~A, #0x~X, lsl #12" (reg-name rd) (reg-name rn) imm12)
             (format nil "add    ~A, ~A, #0x~X" (reg-name rd) (reg-name rn) imm12))))

      ;; SUB immediate: 1101000100 sh imm12 Rn Rd
      ((= (logand instr #xFF000000) #xD1000000)
       (let ((imm12 (logand (ash instr -10) #xFFF))
             (sh (logbitp 22 instr)))
         (if sh
             (format nil "sub    ~A, ~A, #0x~X, lsl #12" (reg-name rd) (reg-name rn) imm12)
             (format nil "sub    ~A, ~A, #0x~X" (reg-name rd) (reg-name rn) imm12))))

      ;; ADD register: 10001011000 Rm 000000 Rn Rd
      ((= (logand instr #xFF200000) #x8B000000)
       (format nil "add    ~A, ~A, ~A" (reg-name rd) (reg-name rn) (reg-name rm)))

      ;; SUB register: 11001011000 Rm 000000 Rn Rd
      ((= (logand instr #xFF200000) #xCB000000)
       (format nil "sub    ~A, ~A, ~A" (reg-name rd) (reg-name rn) (reg-name rm)))

      ;; MUL: 10011011000 Rm 011111 Rn Rd (MADD with Ra=XZR)
      ((= (logand instr #xFFE0FC00) #x9B007C00)
       (format nil "mul    ~A, ~A, ~A" (reg-name rd) (reg-name rn) (reg-name rm)))

      ;; SDIV: 10011010110 Rm 000011 Rn Rd
      ((= (logand instr #xFFE0FC00) #x9AC00C00)
       (format nil "sdiv   ~A, ~A, ~A" (reg-name rd) (reg-name rn) (reg-name rm)))

      ;; SUBS register: 11101011000 Rm 000000 Rn Rd
      ((= (logand instr #xFF200000) #xEB000000)
       (if (= rd 31)
           (format nil "cmp    ~A, ~A" (reg-name rn) (reg-name rm))
           (format nil "subs   ~A, ~A, ~A" (reg-name rd) (reg-name rn) (reg-name rm))))

      ;; SUBS immediate: 11110001 imm12 Rn Rd
      ((= (logand instr #xFF000000) #xF1000000)
       (let ((imm12 (logand (ash instr -10) #xFFF)))
         (if (= rd 31)
             (format nil "cmp    ~A, #0x~X" (reg-name rn) imm12)
             (format nil "subs   ~A, ~A, #0x~X" (reg-name rd) (reg-name rn) imm12))))

      ;; === Bitwise ===

      ;; AND register: 10001010000 Rm 000000 Rn Rd
      ((= (logand instr #xFF200000) #x8A000000)
       (format nil "and    ~A, ~A, ~A" (reg-name rd) (reg-name rn) (reg-name rm)))

      ;; AND immediate: 1001001000 N immr imms Rn Rd
      ((= (logand instr #xFF800000) #x92400000)
       (format nil "and    ~A, ~A, #<imm>" (reg-name rd) (reg-name rn)))

      ;; ORR register: 10101010000 Rm 000000 Rn Rd
      ((= (logand instr #xFF200000) #xAA000000)
       (format nil "orr    ~A, ~A, ~A" (reg-name rd) (reg-name rn) (reg-name rm)))

      ;; ORR immediate: 1011001000 N immr imms Rn Rd
      ((= (logand instr #xFF800000) #xB2400000)
       (format nil "orr    ~A, ~A, #<imm>" (reg-name rd) (reg-name rn)))

      ;; EOR register: 11001010000 Rm 000000 Rn Rd
      ((= (logand instr #xFF200000) #xCA000000)
       (format nil "eor    ~A, ~A, ~A" (reg-name rd) (reg-name rn) (reg-name rm)))

      ;; LSL immediate (UBFM): 1101001101 immr imms Rn Rd
      ((= (logand instr #xFFC00000) #xD3400000)
       (let ((imms (logand (ash instr -10) #x3F)))
         (format nil "lsl    ~A, ~A, #~D" (reg-name rd) (reg-name rn) (- 63 imms))))

      ;; LSR immediate (UBFM with imms=63): 1101001101 immr 111111 Rn Rd
      ((= (logand instr #xFFC0FC00) #xD340FC00)
       (let ((immr (logand (ash instr -16) #x3F)))
         (format nil "lsr    ~A, ~A, #~D" (reg-name rd) (reg-name rn) immr)))

      ;; ASR immediate (SBFM with imms=63): 1001001101 immr 111111 Rn Rd
      ((= (logand instr #xFFC0FC00) #x9340FC00)
       (let ((immr (logand (ash instr -16) #x3F)))
         (format nil "asr    ~A, ~A, #~D" (reg-name rd) (reg-name rn) immr)))

      ;; === Memory ===

      ;; LDR unsigned offset: 11111001010 imm12 Rn Rt
      ((= (logand instr #xFFC00000) #xF9400000)
       (let ((imm12 (logand (ash instr -10) #xFFF)))
         (format nil "ldr    ~A, [~A, #0x~X]" (reg-name rd) (reg-name rn) (* imm12 8))))

      ;; STR unsigned offset: 11111001000 imm12 Rn Rt
      ((= (logand instr #xFFC00000) #xF9000000)
       (let ((imm12 (logand (ash instr -10) #xFFF)))
         (format nil "str    ~A, [~A, #0x~X]" (reg-name rd) (reg-name rn) (* imm12 8))))

      ;; LDUR: 11111000010 imm9 00 Rn Rt
      ((= (logand instr #xFFE00C00) #xF8400000)
       (let ((imm9 (logand (ash instr -12) #x1FF)))
         (let ((signed-imm (if (logbitp 8 imm9) (- imm9 512) imm9)))
           (format nil "ldur   ~A, [~A, #~D]" (reg-name rd) (reg-name rn) signed-imm))))

      ;; STUR: 11111000000 imm9 00 Rn Rt
      ((= (logand instr #xFFE00C00) #xF8000000)
       (let ((imm9 (logand (ash instr -12) #x1FF)))
         (let ((signed-imm (if (logbitp 8 imm9) (- imm9 512) imm9)))
           (format nil "stur   ~A, [~A, #~D]" (reg-name rd) (reg-name rn) signed-imm))))

      ;; LDP: 1010100101 imm7 Rt2 Rn Rt
      ((= (logand instr #xFFC00000) #xA9400000)
       (let* ((imm7 (logand (ash instr -15) #x7F))
              (rt2 (logand (ash instr -10) #x1F))
              (signed-imm (if (logbitp 6 imm7) (- imm7 128) imm7)))
         (format nil "ldp    ~A, ~A, [~A, #~D]" (reg-name rd) (reg-name rt2) (reg-name rn) (* signed-imm 8))))

      ;; STP: 1010100100 imm7 Rt2 Rn Rt
      ((= (logand instr #xFFC00000) #xA9000000)
       (let* ((imm7 (logand (ash instr -15) #x7F))
              (rt2 (logand (ash instr -10) #x1F))
              (signed-imm (if (logbitp 6 imm7) (- imm7 128) imm7)))
         (format nil "stp    ~A, ~A, [~A, #~D]" (reg-name rd) (reg-name rt2) (reg-name rn) (* signed-imm 8))))

      ;; LDRB unsigned offset: 0011100101 imm12 Rn Rt
      ((= (logand instr #xFFC00000) #x39400000)
       (let ((imm12 (logand (ash instr -10) #xFFF)))
         (format nil "ldrb   w~D, [~A, #0x~X]" rd (reg-name rn) imm12)))

      ;; STRB unsigned offset: 0011100100 imm12 Rn Rt
      ((= (logand instr #xFFC00000) #x39000000)
       (let ((imm12 (logand (ash instr -10) #xFFF)))
         (format nil "strb   w~D, [~A, #0x~X]" rd (reg-name rn) imm12)))

      ;; LDRB register: 00111000011 Rm opt S 10 Rn Rt
      ((= (logand instr #xFFE00C00) #x38600800)
       (format nil "ldrb   w~D, [~A, ~A]" rd (reg-name rn) (reg-name rm)))

      ;; STRB register: 00111000001 Rm opt S 10 Rn Rt
      ((= (logand instr #xFFE00C00) #x38200800)
       (format nil "strb   w~D, [~A, ~A]" rd (reg-name rn) (reg-name rm)))

      ;; LDR register: 11111000011 Rm opt S 10 Rn Rt
      ((= (logand instr #xFFE00C00) #xF8600800)
       (format nil "ldr    ~A, [~A, ~A]" (reg-name rd) (reg-name rn) (reg-name rm)))

      ;; === Conditional ===

      ;; CSET: 1001101010011111 cond 0111100000 Rd
      ((= (logand instr #xFFFF0FE0) #x9A9F07E0)
       (let* ((cond-inv (logand (ash instr -12) #xF))
              (cond-name (case (logxor cond-inv 1)
                           (0 "eq") (1 "ne") (2 "hs") (3 "lo")
                           (10 "ge") (11 "lt") (12 "gt") (13 "le")
                           (t "??"))))
         (format nil "cset   ~A, ~A" (reg-name rd) cond-name)))

      ;; === Branch ===

      ;; B: 000101 imm26
      ((= (logand instr #xFC000000) #x14000000)
       (let* ((imm26 (logand instr #x03FFFFFF))
              (signed (if (logbitp 25 imm26) (- imm26 #x4000000) imm26)))
         (if offset
             (format nil "b      0x~X" (+ offset (* signed 4)))
             (format nil "b      #~D" (* signed 4)))))

      ;; BL: 100101 imm26
      ((= (logand instr #xFC000000) #x94000000)
       (let* ((imm26 (logand instr #x03FFFFFF))
              (signed (if (logbitp 25 imm26) (- imm26 #x4000000) imm26)))
         (if offset
             (format nil "bl     0x~X" (+ offset (* signed 4)))
             (format nil "bl     #~D" (* signed 4)))))

      ;; BLR: 1101011000111111000000 Rn 00000
      ((= (logand instr #xFFFFFC1F) #xD63F0000)
       (format nil "blr    ~A" (reg-name rn)))

      ;; BR: 1101011000011111000000 Rn 00000
      ((= (logand instr #xFFFFFC1F) #xD61F0000)
       (format nil "br     ~A" (reg-name rn)))

      ;; RET: 1101011001011111000000 Rn 00000 (typically Rn=30)
      ((= (logand instr #xFFFFFC1F) #xD65F0000)
       (if (= rn 30)
           "ret"
           (format nil "ret    ~A" (reg-name rn))))

      ;; CBZ: 10110100 imm19 Rt
      ((= (logand instr #xFF000000) #xB4000000)
       (let* ((imm19 (logand (ash instr -5) #x7FFFF))
              (signed (if (logbitp 18 imm19) (- imm19 #x80000) imm19)))
         (if offset
             (format nil "cbz    ~A, 0x~X" (reg-name rd) (+ offset (* signed 4)))
             (format nil "cbz    ~A, #~D" (reg-name rd) (* signed 4)))))

      ;; CBNZ: 10110101 imm19 Rt
      ((= (logand instr #xFF000000) #xB5000000)
       (let* ((imm19 (logand (ash instr -5) #x7FFFF))
              (signed (if (logbitp 18 imm19) (- imm19 #x80000) imm19)))
         (if offset
             (format nil "cbnz   ~A, 0x~X" (reg-name rd) (+ offset (* signed 4)))
             (format nil "cbnz   ~A, #~D" (reg-name rd) (* signed 4)))))

      ;; B.cond: 01010100 imm19 0 cond
      ((= (logand instr #xFF000010) #x54000000)
       (let* ((imm19 (logand (ash instr -5) #x7FFFF))
              (cond-code (logand instr #xF))
              (signed (if (logbitp 18 imm19) (- imm19 #x80000) imm19))
              (cond-name (case cond-code
                           (0 "eq") (1 "ne") (2 "hs") (3 "lo")
                           (8 "hi") (9 "ls") (10 "ge") (11 "lt")
                           (12 "gt") (13 "le") (t "??"))))
         (if offset
             (format nil "b.~A   0x~X" cond-name (+ offset (* signed 4)))
             (format nil "b.~A   #~D" cond-name (* signed 4)))))

      ;; === System ===

      ;; SVC: 11010100000 imm16 00001
      ((= (logand instr #xFFE0001F) #xD4000001)
       (let ((imm16 (logand (ash instr -5) #xFFFF)))
         (format nil "svc    #0x~X" imm16)))

      ;; NOP: 11010101000000110010000000011111
      ((= instr #xD503201F)
       "nop")

      ;; Default: unknown instruction
      (t (format nil ".word  0x~8,'0X" instr)))))

#+sbcl
(defun reg-alloc-trace-fn (fn &optional (stream t))
  "Full trace of register allocation for a function.
   Shows each pass and the generated code."
  (format stream "~%========================================~%")
  (format stream "Register Allocator Trace~%")
  (format stream "========================================~%")
  (format stream "~%Function: ~S~%" (car fn))
  (format stream "Params: ~S~%" (cadr fn))
  (format stream "Body IR: ~S~%" (caddr fn))
  (format stream "Param-base: ~S~%" (cadddr fn))

  (let* ((params (cadr fn))
         (body-ir (caddr fn))
         (param-base (cadddr fn))
         (counter (make-vreg-counter)))

    ;; Pass 1: IR to TAC
    (format stream "~%--- Pass 1: IR to TAC ---~%")
    (let* ((tac-result (ir-to-tac body-ir counter))
           (tac-instrs (car tac-result))
           (result-vr (cadr tac-result)))
      (if (null tac-instrs)
          (progn
            (format stream "IR conversion failed - unsupported IR~%")
            (return-from reg-alloc-trace-fn nil))
          (progn
            (format stream "TAC Instructions:~%")
            (let ((i 0))
              (dolist (instr tac-instrs)
                (format stream "  ~3D: ~S~%" i instr)
                (incf i)))
            (format stream "Result vreg: v~D~%" result-vr)))

      ;; Add return
      (let ((full-tac (append tac-instrs (list (list 'tac-return result-vr)))))

        ;; Pass 2: Liveness
        (format stream "~%--- Pass 2: Liveness Analysis ---~%")
        (let ((annotated (compute-liveness full-tac)))
          (print-liveness annotated stream)

          ;; Pass 3: Intervals
          (format stream "~%--- Pass 3: Live Intervals ---~%")
          (let ((intervals (compute-intervals annotated)))
            (print-intervals intervals stream)

            ;; Pass 4: Allocation
            (format stream "~%--- Pass 4: Register Allocation ---~%")
            (let ((allocation (linear-scan intervals)))
              (print-allocation allocation stream)

              ;; Generate code
              (format stream "~%--- Pass 5: Code Generation ---~%")
              (let* ((prologue-code (reg-alloc-prologue))
                     (param-code (reg-alloc-gen-param-stores params param-base))
                     (body-code (tac-codegen full-tac allocation))
                     (epilogue-code (reg-alloc-epilogue))
                     (all-code (append prologue-code param-code body-code epilogue-code)))

                ;; Check for unresolved markers
                (format stream "~%Checking for unresolved markers...~%")
                (if (has-unresolved-markers all-code)
                    (progn
                      (format stream "Found unresolved markers - would fall back to accumulator codegen~%")
                      (format stream "Markers found: ~S~%"
                              (remove-if-not (lambda (x)
                                               (and (consp x)
                                                    (member (car x) '(:call-fn :funcall-marker
                                                                      :make-closure-marker
                                                                      :heap-alloc-marker
                                                                      :continue-marker))))
                                             all-code)))
                    (format stream "No unresolved markers - code is complete~%"))

                ;; Disassemble
                (format stream "~%--- Generated Code ---~%")
                (disassemble-bytes all-code stream)

                ;; Return the code
                all-code))))))))

#+sbcl
(defun compare-codegen (fn &optional (stream t))
  "Compare register-allocated vs accumulator codegen for a function.
   Useful for finding discrepancies."
  (format stream "~%========================================~%")
  (format stream "Codegen Comparison~%")
  (format stream "========================================~%")
  (format stream "~%Function: ~S~%" fn)

  ;; Generate with reg-alloc
  (format stream "~%--- Register Allocator ---~%")
  (let ((reg-code (codegen-fn-reg-alloc fn)))
    (if reg-code
        (progn
          (format stream "Generated ~D bytes~%" (length reg-code))
          (disassemble-bytes reg-code stream))
        (format stream "Fell back to accumulator (returned nil)~%")))

  ;; Generate with accumulator (force by binding *use-register-allocation* nil)
  (format stream "~%--- Accumulator Codegen ---~%")
  (let* ((*use-register-allocation* nil)
         (acc-code (codegen-fn fn nil nil)))
    (format stream "Generated ~D bytes~%" (length acc-code))
    (disassemble-bytes acc-code stream)))

#+sbcl
(defun test-fn-execution (source &optional (stream t))
  "Compile a source string and test execution.
   Returns exit code or error info."
  (format stream "~%Testing: ~S~%" source)
  (let ((tmp-path "/tmp/habu_reg_alloc_test"))
    ;; Compile with reg-alloc
    (format stream "~%Compiling with *use-register-allocation* = t~%")
    (let ((*use-register-allocation* t))
      (deliver source tmp-path))
    ;; Run and capture exit code
    (let ((exit-code (nth-value 2 (uiop:run-program tmp-path :ignore-error-status t))))
      (format stream "Exit code: ~D~%" exit-code)
      (cond
        ((= exit-code 132) (format stream "  SIGILL - Illegal instruction~%"))
        ((= exit-code 139) (format stream "  SIGSEGV - Segmentation fault~%"))
        ((= exit-code 138) (format stream "  SIGBUS - Bus error~%"))
        ((= exit-code 137) (format stream "  SIGKILL - Killed (codesign?)~%")))
      exit-code)))
