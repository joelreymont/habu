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
    (setcar counter (+ n 1))
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

;;; TCO loop tracking for continue-ir resolution
(defvar *tco-loop-label* nil
  "Current loop label for TCO. Set by loop-ir, used by continue-ir.")
(defvar *tco-loop-marker* nil
  "Current loop marker for TCO. Set by loop-ir, used by continue-ir.")

;;; Block/return-from tracking
(defvar *block-labels* nil
  "Alist of (block-id . end-label) for return-from resolution.")
(defvar *block-results* nil
  "Alist of (block-id . result-vreg) for return-from resolution.")

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

    ;; Binary operations: add, sub, mul, div, mod, bsh, band, bor, bxor
    ;; Note: some forms use -IR suffix (MOD-IR, DIV-IR) depending on compiler path
    ((and (consp ir) (ir-tag-member (car ir) '("ADD" "SUB" "MUL" "DIV" "MOD" "BSH" "BAND" "BOR" "BXOR"
                                               "MOD-IR" "DIV-IR" "ADD-IR" "SUB-IR" "MUL-IR")))
     (let* ((left-result (ir-to-tac (cadr ir) counter))
            (left-instrs (car left-result))
            (left-vr (cadr left-result))
            (right-result (ir-to-tac (caddr ir) counter))
            (right-instrs (car right-result))
            (right-vr (cadr right-result))
            (result-vr (next-vreg counter))
            (raw-name (ir-tag-name (car ir)))
            ;; Strip -IR suffix if present (e.g., "MOD-IR" -> "MOD")
            (op-name (if (and (> (length raw-name) 3)
                              (string= (subseq raw-name (- (length raw-name) 3)) "-IR"))
                         (subseq raw-name 0 (- (length raw-name) 3))
                         raw-name)))
       (list (append left-instrs
                     right-instrs
                     (list (list 'tac-binop result-vr
                                 (intern op-name :keyword)  ; :ADD, :SUB, :MOD, etc.
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
            (result-vr (next-vreg counter))
            (op-name (ir-tag-name (car ir))))
       (list (append left-instrs
                     right-instrs
                     (list (list 'tac-cmp result-vr (intern op-name :keyword) left-vr right-vr)))
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

    ;; buffer-to-string-ir: convert raw byte buffer to string
    ((and (consp ir) (ir-tag-matches (car ir) "BUFFER-TO-STRING-IR"))
     (let* ((buf-result (ir-to-tac (cadr ir) counter))
            (buf-instrs (car buf-result))
            (buf-vr (cadr buf-result))
            (len-result (ir-to-tac (caddr ir) counter))
            (len-instrs (car len-result))
            (len-vr (cadr len-result))
            (result-vr (next-vreg counter)))
       (list (append buf-instrs len-instrs
                     (list (list 'tac-buffer-to-string result-vr buf-vr len-vr)))
             result-vr)))

    ;; loop-ir: TCO loop (loop-ir body marker)
    ((and (consp ir) (ir-tag-matches (car ir) "LOOP-IR"))
     (let* ((loop-label (next-vreg counter))
            (marker (caddr ir)))
       ;; Set current loop info for continue-ir to reference
       (setf *tco-loop-label* loop-label)
       (setf *tco-loop-marker* marker)
       (let* ((body-result (ir-to-tac (cadr ir) counter))
              (body-instrs (car body-result))
              (body-vr (cadr body-result)))
         ;; Emit loop-start with marker and label
         (list (append (list (list 'tac-loop-start loop-label marker))
                       (list (list 'tac-label loop-label))
                       body-instrs)
               body-vr))))

    ;; continue-ir: jump back to loop start after updating params
    ;; Format: (continue-ir (new-arg1 new-arg2 ...))
    ((and (consp ir) (ir-tag-matches (car ir) "CONTINUE-IR"))
     (let* ((new-args (cadr ir))
            (result-vr (next-vreg counter)))
       ;; Evaluate all new arg values to temp vregs first (avoid overwriting params mid-eval)
       ;; NOTE: Must append instructions in correct order - arg-instrs AFTER acc-instrs
       (labels ((eval-args (args acc-instrs acc-vrs)
                  (if (null args)
                      (list acc-instrs (reverse acc-vrs))
                      (let* ((arg-result (ir-to-tac (car args) counter))
                             (arg-instrs (car arg-result))
                             (arg-vr (cadr arg-result)))
                        (eval-args (cdr args)
                                   (append acc-instrs arg-instrs)
                                   (cons arg-vr acc-vrs)))))
                (gen-setvars (vrs idx)
                  ;; Store each vreg to param slot (offset = idx)
                  (if (null vrs)
                      nil
                      (cons (list 'tac-setvar idx (car vrs))
                            (gen-setvars (cdr vrs) (+ idx 1))))))
         (let* ((eval-result (eval-args new-args nil nil))
                (all-instrs (car eval-result))
                (arg-vrs (cadr eval-result))
                (setvar-instrs (gen-setvars arg-vrs 0)))
           ;; Emit: arg evals, setvars, continue (with marker from *tco-loop-marker*)
           (list (append all-instrs
                         setvar-instrs
                         (list (list 'tac-continue *tco-loop-marker*))
                         (list (list 'tac-nil result-vr)))  ; unreachable but needed
                 result-vr)))))

    ;; dotimes-ir: counted iteration loop
    ;; Format: (dotimes-ir var count-ir body-ir result-ir compile-env)
    ;; The loop var is at slot (length compile-env) in the extended env
    ((and (consp ir) (ir-tag-matches (car ir) "DOTIMES-IR"))
     (let* ((count-ir (caddr ir))
            (body-ir (cadddr ir))
            (result-ir (nth 4 ir))
            (compile-env (nth 5 ir))
            (loop-var-slot (length compile-env))
            ;; Generate labels
            (loop-label (next-vreg counter))
            (end-label (next-vreg counter))
            ;; Evaluate count
            (count-result (ir-to-tac count-ir counter))
            (count-instrs (car count-result))
            (count-vr (cadr count-result))
            ;; Create counter vreg initialized to 0
            (counter-vr (next-vreg counter))
            (result-vr (next-vreg counter))
            ;; Vreg for literal 1 (for increment)
            (one-vr (next-vreg counter)))
       ;; Compile body and result
       (let* ((body-result (ir-to-tac body-ir counter))
              (body-instrs (car body-result))
              (result-result (ir-to-tac result-ir counter))
              (result-instrs (car result-result))
              (final-vr (cadr result-result))
              ;; Temp vreg for incremented counter
              (inc-vr (next-vreg counter))
              ;; Temp vreg for comparison
              (cmp-vr (next-vreg counter)))
         (list (append
                ;; Evaluate count
                count-instrs
                ;; Load literal 1 for increment
                (list (list 'tac-lit one-vr 1))
                ;; Initialize counter to 0 and store in loop var slot
                (list (list 'tac-lit counter-vr 0))
                (list (list 'tac-setvar loop-var-slot counter-vr))
                ;; Loop start
                (list (list 'tac-label loop-label))
                ;; Load current counter from slot
                (list (list 'tac-var counter-vr loop-var-slot))
                ;; Compare counter < count (returns tagged t/nil)
                (list (list 'tac-cmp cmp-vr :CMP-LT counter-vr count-vr))
                ;; Branch to end if counter NOT < count (i.e., counter >= count)
                (list (list 'tac-if-not cmp-vr end-label))
                ;; Execute body
                body-instrs
                ;; Load counter, increment, store back
                (list (list 'tac-var counter-vr loop-var-slot))
                (list (list 'tac-binop inc-vr :add counter-vr one-vr))
                (list (list 'tac-setvar loop-var-slot inc-vr))
                ;; Branch back to loop start (unconditional)
                (list (list 'tac-goto loop-label))
                ;; End label
                (list (list 'tac-label end-label))
                ;; Evaluate result
                result-instrs
                ;; Move final result to result-vr
                (list (list 'tac-move result-vr final-vr)))
               result-vr))))

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

    ;; get-global-vars-ir: load global vars table from [x27 + 104]
    ((and (consp ir) (ir-tag-matches (car ir) "GET-GLOBAL-VARS-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-global-vars result-vr)) result-vr)))

    ;; set-global-vars-ir: store global vars table to [x27 + 104]
    ((and (consp ir) (ir-tag-matches (car ir) "SET-GLOBAL-VARS-IR"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       (list (append val-instrs
                     (list (list 'tac-set-global-vars val-vr)))
             val-vr)))

    ;; get-cmdline-args-ir: get command line args
    ((and (consp ir) (ir-tag-matches (car ir) "GET-CMDLINE-ARGS-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-cmdline-args result-vr)) result-vr)))

    ;; sys-exit-ir: exit with code
    ((and (consp ir) (ir-tag-matches (car ir) "SYS-EXIT-IR"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       (list (append val-instrs
                     (list (list 'tac-sys-exit val-vr)))
             val-vr)))

    ;; sys-open-ir: open(path, flags, mode)
    ((and (consp ir) (ir-tag-matches (car ir) "SYS-OPEN-IR"))
     (let* ((path-result (ir-to-tac (cadr ir) counter))
            (path-instrs (car path-result))
            (path-vr (cadr path-result))
            (flags-result (ir-to-tac (caddr ir) counter))
            (flags-instrs (car flags-result))
            (flags-vr (cadr flags-result))
            (mode-result (ir-to-tac (cadddr ir) counter))
            (mode-instrs (car mode-result))
            (mode-vr (cadr mode-result))
            (result-vr (next-vreg counter)))
       (list (append path-instrs flags-instrs mode-instrs
                     (list (list 'tac-sys-open result-vr path-vr flags-vr mode-vr)))
             result-vr)))

    ;; sys-read-ir: read(fd, buf, len)
    ((and (consp ir) (ir-tag-matches (car ir) "SYS-READ-IR"))
     (let* ((fd-result (ir-to-tac (cadr ir) counter))
            (fd-instrs (car fd-result))
            (fd-vr (cadr fd-result))
            (buf-result (ir-to-tac (caddr ir) counter))
            (buf-instrs (car buf-result))
            (buf-vr (cadr buf-result))
            (len-result (ir-to-tac (cadddr ir) counter))
            (len-instrs (car len-result))
            (len-vr (cadr len-result))
            (result-vr (next-vreg counter)))
       (list (append fd-instrs buf-instrs len-instrs
                     (list (list 'tac-sys-read result-vr fd-vr buf-vr len-vr)))
             result-vr)))

    ;; sys-write-ir: write(fd, buf, len)
    ((and (consp ir) (ir-tag-matches (car ir) "SYS-WRITE-IR"))
     (let* ((fd-result (ir-to-tac (cadr ir) counter))
            (fd-instrs (car fd-result))
            (fd-vr (cadr fd-result))
            (buf-result (ir-to-tac (caddr ir) counter))
            (buf-instrs (car buf-result))
            (buf-vr (cadr buf-result))
            (len-result (ir-to-tac (cadddr ir) counter))
            (len-instrs (car len-result))
            (len-vr (cadr len-result))
            (result-vr (next-vreg counter)))
       (list (append fd-instrs buf-instrs len-instrs
                     (list (list 'tac-sys-write result-vr fd-vr buf-vr len-vr)))
             result-vr)))

    ;; sys-close-ir: close(fd)
    ((and (consp ir) (ir-tag-matches (car ir) "SYS-CLOSE-IR"))
     (let* ((fd-result (ir-to-tac (cadr ir) counter))
            (fd-instrs (car fd-result))
            (fd-vr (cadr fd-result))
            (result-vr (next-vreg counter)))
       (list (append fd-instrs
                     (list (list 'tac-sys-close result-vr fd-vr)))
             result-vr)))

    ;; buffer-byte-set-ir: set byte in buffer
    ((and (consp ir) (ir-tag-matches (car ir) "BUFFER-BYTE-SET-IR"))
     (let* ((buf-result (ir-to-tac (cadr ir) counter))
            (buf-instrs (car buf-result))
            (buf-vr (cadr buf-result))
            (idx-result (ir-to-tac (caddr ir) counter))
            (idx-instrs (car idx-result))
            (idx-vr (cadr idx-result))
            (val-result (ir-to-tac (cadddr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result))
            (result-vr (next-vreg counter)))
       (list (append buf-instrs idx-instrs val-instrs
                     (list (list 'tac-buffer-byte-set result-vr buf-vr idx-vr val-vr)))
             result-vr)))

    ;; buffer-byte-ref-ir: get byte from buffer at index
    ((and (consp ir) (ir-tag-matches (car ir) "BUFFER-BYTE-REF-IR"))
     (let* ((buf-result (ir-to-tac (cadr ir) counter))
            (buf-instrs (car buf-result))
            (buf-vr (cadr buf-result))
            (idx-result (ir-to-tac (caddr ir) counter))
            (idx-instrs (car idx-result))
            (idx-vr (cadr idx-result))
            (result-vr (next-vreg counter)))
       (list (append buf-instrs idx-instrs
                     (list (list 'tac-buffer-byte-ref result-vr buf-vr idx-vr)))
             result-vr)))

    ;; mem-set-byte-ir: set byte at pointer + offset
    ((and (consp ir) (ir-tag-matches (car ir) "MEM-SET-BYTE-IR"))
     (let* ((ptr-result (ir-to-tac (cadr ir) counter))
            (ptr-instrs (car ptr-result))
            (ptr-vr (cadr ptr-result))
            (off-result (ir-to-tac (caddr ir) counter))
            (off-instrs (car off-result))
            (off-vr (cadr off-result))
            (val-result (ir-to-tac (cadddr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result))
            (result-vr (next-vreg counter)))
       (list (append ptr-instrs off-instrs val-instrs
                     (list (list 'tac-mem-set-byte result-vr ptr-vr off-vr val-vr)))
             result-vr)))

    ;; mem-load-64-ir: load 64-bit from pointer + offset
    ((and (consp ir) (ir-tag-matches (car ir) "MEM-LOAD-64-IR"))
     (let* ((ptr-result (ir-to-tac (cadr ir) counter))
            (ptr-instrs (car ptr-result))
            (ptr-vr (cadr ptr-result))
            (off-result (ir-to-tac (caddr ir) counter))
            (off-instrs (car off-result))
            (off-vr (cadr off-result))
            (result-vr (next-vreg counter)))
       (list (append ptr-instrs off-instrs
                     (list (list 'tac-mem-load-64 result-vr ptr-vr off-vr)))
             result-vr)))

    ;; mem-load-byte-ir: load single byte from pointer + offset
    ((and (consp ir) (ir-tag-matches (car ir) "MEM-LOAD-BYTE-IR"))
     (let* ((ptr-result (ir-to-tac (cadr ir) counter))
            (ptr-instrs (car ptr-result))
            (ptr-vr (cadr ptr-result))
            (off-result (ir-to-tac (caddr ir) counter))
            (off-instrs (car off-result))
            (off-vr (cadr off-result))
            (result-vr (next-vreg counter)))
       (list (append ptr-instrs off-instrs
                     (list (list 'tac-mem-load-byte result-vr ptr-vr off-vr)))
             result-vr)))

    ;; bnot-ir: boolean not
    ((and (consp ir) (ir-tag-matches (car ir) "BNOT-IR"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result))
            (result-vr (next-vreg counter)))
       (list (append val-instrs
                     (list (list 'tac-bnot result-vr val-vr)))
             result-vr)))

    ;; mvn-ir: bitwise NOT (ARM64 MVN instruction)
    ((and (consp ir) (ir-tag-matches (car ir) "MVN-IR"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result))
            (result-vr (next-vreg counter)))
       (list (append val-instrs
                     (list (list 'tac-mvn result-vr val-vr)))
             result-vr)))

    ;; lambda-ref: reference to lifted lambda
    ;; IR: (lambda-ref name free-offsets)
    ;; free-offsets is list of env offsets for captured variables
    ((and (consp ir) (ir-tag-matches (car ir) "LAMBDA-REF"))
     (let ((result-vr (next-vreg counter))
           (lambda-name (cadr ir))
           (free-offsets (caddr ir)))
       (list (list (list 'tac-lambda-ref result-vr lambda-name free-offsets)) result-vr)))

    ;; symbol-name-ir: get symbol's name as string
    ((and (consp ir) (ir-tag-matches (car ir) "SYMBOL-NAME-IR"))
     (let* ((sym-result (ir-to-tac (cadr ir) counter))
            (sym-instrs (car sym-result))
            (sym-vr (cadr sym-result))
            (result-vr (next-vreg counter)))
       (list (append sym-instrs
                     (list (list 'tac-symbol-name result-vr sym-vr)))
             result-vr)))

    ;; make-symbol-ir: create symbol from string
    ((and (consp ir) (ir-tag-matches (car ir) "MAKE-SYMBOL-IR"))
     (let* ((name-result (ir-to-tac (cadr ir) counter))
            (name-instrs (car name-result))
            (name-vr (cadr name-result))
            (result-vr (next-vreg counter)))
       (list (append name-instrs
                     (list (list 'tac-make-symbol result-vr name-vr)))
             result-vr)))

    ;; make-symbol-from-string-ir: same as make-symbol-ir
    ((and (consp ir) (ir-tag-matches (car ir) "MAKE-SYMBOL-FROM-STRING-IR"))
     (let* ((name-result (ir-to-tac (cadr ir) counter))
            (name-instrs (car name-result))
            (name-vr (cadr name-result))
            (result-vr (next-vreg counter)))
       (list (append name-instrs
                     (list (list 'tac-make-symbol result-vr name-vr)))
             result-vr)))

    ;; string-concat-ir: concatenate two strings
    ((and (consp ir) (ir-tag-matches (car ir) "STRING-CONCAT-IR"))
     (let* ((s1-result (ir-to-tac (cadr ir) counter))
            (s1-instrs (car s1-result))
            (s1-vr (cadr s1-result))
            (s2-result (ir-to-tac (caddr ir) counter))
            (s2-instrs (car s2-result))
            (s2-vr (cadr s2-result))
            (result-vr (next-vreg counter)))
       (list (append s1-instrs s2-instrs
                     (list (list 'tac-string-concat result-vr s1-vr s2-vr)))
             result-vr)))

    ;; string-equal-ir: compare two strings
    ((and (consp ir) (ir-tag-matches (car ir) "STRING-EQUAL-IR"))
     (let* ((s1-result (ir-to-tac (cadr ir) counter))
            (s1-instrs (car s1-result))
            (s1-vr (cadr s1-result))
            (s2-result (ir-to-tac (caddr ir) counter))
            (s2-instrs (car s2-result))
            (s2-vr (cadr s2-result))
            (result-vr (next-vreg counter)))
       (list (append s1-instrs s2-instrs
                     (list (list 'tac-string-equal result-vr s1-vr s2-vr)))
             result-vr)))

    ;; get-intern-table-ir: load intern table from [x27 + 0]
    ((and (consp ir) (ir-tag-matches (car ir) "GET-INTERN-TABLE-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-intern-table result-vr)) result-vr)))

    ;; set-intern-table-ir: store intern table to [x27 + 0]
    ((and (consp ir) (ir-tag-matches (car ir) "SET-INTERN-TABLE-IR"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       (list (append val-instrs
                     (list (list 'tac-set-intern-table val-vr)))
             val-vr)))

    ;; get-lambda-counter-ir: load lambda counter from [x27 + 8]
    ((and (consp ir) (ir-tag-matches (car ir) "GET-LAMBDA-COUNTER-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-lambda-counter result-vr)) result-vr)))

    ;; set-lambda-counter-ir: store lambda counter to [x27 + 8]
    ((and (consp ir) (ir-tag-matches (car ir) "SET-LAMBDA-COUNTER-IR"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       (list (append val-instrs
                     (list (list 'tac-set-lambda-counter val-vr)))
             val-vr)))

    ;; get-frame-pointer-ir: get x29 as raw pointer for stack walking
    ((and (consp ir) (ir-tag-matches (car ir) "GET-FRAME-POINTER-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-frame-pointer result-vr)) result-vr)))

    ;; get-code-base-ir: get x26 as raw pointer for symbol table access
    ((and (consp ir) (ir-tag-matches (car ir) "GET-CODE-BASE-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-code-base result-vr)) result-vr)))

    ;; get-symtab-offset-ir: load symtab offset from [x27 + 112]
    ((and (consp ir) (ir-tag-matches (car ir) "GET-SYMTAB-OFFSET-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-symtab-offset result-vr)) result-vr)))

    ;; get-symtab-count-ir: load symtab count from [x27 + 120]
    ((and (consp ir) (ir-tag-matches (car ir) "GET-SYMTAB-COUNT-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-symtab-count result-vr)) result-vr)))

    ;; get-symbol-counter-ir: load symbol counter from [x27 + 48]
    ((and (consp ir) (ir-tag-matches (car ir) "GET-SYMBOL-COUNTER-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-symbol-counter result-vr)) result-vr)))

    ;; set-symbol-counter-ir: store symbol counter to [x27 + 48]
    ((and (consp ir) (ir-tag-matches (car ir) "SET-SYMBOL-COUNTER-IR"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       (list (append val-instrs
                     (list (list 'tac-set-symbol-counter val-vr)))
             val-vr)))

    ;; get-symbol-table-sym-ir: load symbol table from [x27 + 56]
    ((and (consp ir) (ir-tag-matches (car ir) "GET-SYMBOL-TABLE-SYM-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-symbol-table result-vr)) result-vr)))

    ;; set-symbol-table-sym-ir: store symbol table to [x27 + 56]
    ((and (consp ir) (ir-tag-matches (car ir) "SET-SYMBOL-TABLE-SYM-IR"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       (list (append val-instrs
                     (list (list 'tac-set-symbol-table val-vr)))
             val-vr)))

    ;; get-packages-ir: load packages alist from [x27 + 80]
    ((and (consp ir) (ir-tag-matches (car ir) "GET-PACKAGES-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-packages result-vr)) result-vr)))

    ;; set-packages-ir: store packages alist to [x27 + 80]
    ((and (consp ir) (ir-tag-matches (car ir) "SET-PACKAGES-IR"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       (list (append val-instrs
                     (list (list 'tac-set-packages val-vr)))
             val-vr)))

    ;; get-current-package-ir: load current package name from [x27 + 88]
    ((and (consp ir) (ir-tag-matches (car ir) "GET-CURRENT-PACKAGE-IR"))
     (let ((result-vr (next-vreg counter)))
       (list (list (list 'tac-get-current-package result-vr)) result-vr)))

    ;; set-current-package-ir: store current package name to [x27 + 88]
    ((and (consp ir) (ir-tag-matches (car ir) "SET-CURRENT-PACKAGE-IR"))
     (let* ((val-result (ir-to-tac (cadr ir) counter))
            (val-instrs (car val-result))
            (val-vr (cadr val-result)))
       (list (append val-instrs
                     (list (list 'tac-set-current-package val-vr)))
             val-vr)))

    ;; block-ir: named block with return-from support
    ;; Format: (block-ir (name . block-id) body)
    ((and (consp ir) (ir-tag-matches (car ir) "BLOCK-IR"))
     (let* ((name-info (cadr ir))  ; (name . block-id)
            (block-id (if (consp name-info) (cdr name-info) name-info))
            (body (caddr ir))
            (result-vr (next-vreg counter))
            (end-label (next-vreg counter))
            ;; Store end-label in association list for return-from to reference
            (*block-labels* (cons (cons block-id end-label) *block-labels*))
            (*block-results* (cons (cons block-id result-vr) *block-results*)))
       (let* ((body-result (ir-to-tac body counter))
              (body-instrs (car body-result))
              (body-vr (cadr body-result)))
         (list (append body-instrs
                       (list (list 'tac-move result-vr body-vr))
                       (list (list 'tac-label end-label)))
               result-vr))))

    ;; return-from-ir: early exit from named block
    ;; Format: (return-from-ir (name . block-id) value)
    ((and (consp ir) (ir-tag-matches (car ir) "RETURN-FROM-IR"))
     (let* ((name-info (cadr ir))  ; (name . block-id)
            (block-id (if (consp name-info) (cdr name-info) name-info))
            (value-ir (caddr ir))
            (end-label (cdr (assoc block-id *block-labels*)))
            (result-vr (cdr (assoc block-id *block-results*))))
       (if (and end-label result-vr)
           (let* ((val-result (ir-to-tac value-ir counter))
                  (val-instrs (car val-result))
                  (val-vr (cadr val-result)))
             (list (append val-instrs
                           (list (list 'tac-move result-vr val-vr))
                           (list (list 'tac-goto end-label)))
                   result-vr))
           ;; Fallback if block not found (shouldn't happen)
           (let* ((val-result (ir-to-tac value-ir counter))
                  (val-instrs (car val-result))
                  (val-vr (cadr val-result)))
             (list val-instrs val-vr)))))

    ;; Default: error on unhandled IR
    (t
     (error "ir-to-tac: Unhandled IR form: ~A" ir))))

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
      tac-get-tag tac-funcall tac-make-closure
      tac-get-global-vars tac-get-cmdline-args
      tac-sys-open tac-sys-read tac-sys-write tac-sys-close tac-buffer-to-string
      tac-buffer-byte-set tac-buffer-byte-ref tac-mem-set-byte tac-mem-load-64 tac-mem-load-byte tac-bnot tac-mvn
      ;; New TAC instructions
      tac-lambda-ref tac-symbol-name tac-make-symbol
      tac-string-concat tac-string-equal
      tac-get-intern-table tac-get-lambda-counter tac-get-symbol-counter tac-get-symbol-table
      tac-get-frame-pointer tac-get-code-base tac-get-symtab-offset tac-get-symtab-count
      tac-get-packages tac-get-current-package)
     (cadr instr))
    ;; Instructions that don't define a vreg (control flow, stores, etc.)
    ((tac-return tac-if tac-if-not tac-goto tac-label tac-setvar tac-sys-exit
      tac-loop-start tac-continue tac-setcar tac-setcdr tac-vector-set
      tac-set-global-vars tac-set-intern-table
      tac-set-packages tac-set-current-package)
     nil)
    (t (error "tac-def: Unhandled TAC instruction: ~A" (car instr)))))

(defun tac-use (instr)
  "Return list of vregs used by this instruction"
  (case (car instr)
    ;; Instructions with no vreg uses
    ((tac-lit tac-param tac-var tac-label tac-goto tac-nil tac-sym tac-str
      tac-loop-start tac-continue tac-make-closure
      tac-get-global-vars tac-get-cmdline-args
      ;; New no-use instructions
      tac-lambda-ref tac-get-intern-table tac-get-lambda-counter
      tac-get-symbol-counter tac-get-symbol-table tac-get-frame-pointer
      tac-get-code-base tac-get-symtab-offset tac-get-symtab-count
      tac-get-packages tac-get-current-package)
     nil)
    ;; Binary operations: (tac-binop dest op vr1 vr2)
    ((tac-binop tac-cmp)
     (list (cadddr instr) (nth 4 instr)))
    ;; setvar: (tac-setvar offset vreg) - uses 3rd element
    ((tac-setvar)
     (list (caddr instr)))
    ;; Global/system setters: (tac-set-X vreg) - uses 2nd element
    ((tac-set-global-vars tac-set-intern-table tac-set-lambda-counter
      tac-set-symbol-counter tac-set-symbol-table tac-sys-exit
      tac-set-packages tac-set-current-package)
     (list (cadr instr)))
    ;; Conditionals: (tac-if cond-vreg then else)
    ((tac-if tac-if-not)
     (list (cadr instr)))
    ;; Return: (tac-return vreg)
    ((tac-return)
     (list (cadr instr)))
    ;; Unary ops: (tac-X dest src)
    ((tac-move tac-car tac-cdr tac-vector-length tac-string-length
      tac-make-vector tac-make-string tac-get-tag tac-bnot tac-mvn tac-sys-close
      ;; New unary ops
      tac-symbol-name tac-make-symbol)
     (list (caddr instr)))
    ;; Cons: (tac-cons dest car cdr)
    ((tac-cons tac-mem-load-64 tac-mem-load-byte)
     (list (caddr instr) (cadddr instr)))
    ;; buffer-to-string: (tac-buffer-to-string dest buf len)
    ((tac-buffer-to-string)
     (list (caddr instr) (cadddr instr)))
    ;; Binary string ops: (tac-X dest vr1 vr2)
    ((tac-string-concat tac-string-equal)
     (list (caddr instr) (cadddr instr)))
    ;; Mutation: (tac-setcar cons-vr val-vr), (tac-setcdr cons-vr val-vr)
    ((tac-setcar tac-setcdr)
     (list (cadr instr) (caddr instr)))
    ;; Vector ops: (tac-vector-ref dest vec idx), (tac-string-ref dest str idx), (tac-buffer-byte-ref dest buf idx)
    ((tac-vector-ref tac-string-ref tac-buffer-byte-ref)
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
    ;; sys-open: (tac-sys-open dest path flags mode)
    ((tac-sys-open)
     (list (caddr instr) (cadddr instr) (nth 4 instr)))
    ;; sys-read/write: (tac-sys-read/write dest fd buf len)
    ((tac-sys-read tac-sys-write)
     (list (caddr instr) (cadddr instr) (nth 4 instr)))
    ;; buffer-byte-set, mem-set-byte: (tac-X dest buf/ptr idx/off val)
    ((tac-buffer-byte-set tac-mem-set-byte)
     (list (caddr instr) (cadddr instr) (nth 4 instr)))
    (t (error "tac-use: Unhandled TAC instruction: ~A" (car instr)))))

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
                   (setcar lst val)
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
                     (setcar (cddr entry) p)
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
   x9-x15: 7 caller-saved temporaries
   NOTE: x8 is reserved as scratch for runtime (gc-trigger, MOD, etc.)"
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

;; Spill slots start at sp+0x40 (after saved callee registers at sp+0x10-0x38)
(defparameter +spill-base-offset+ #x40)

(defun spill-offset (slot)
  "Calculate stack offset for spill slot. Spill area starts at sp+0x40."
  (+ +spill-base-offset+ (* slot 8)))

(defun vreg-to-reg (vreg allocation)
  "Look up physical register for vreg. Returns reg keyword (:x9, etc.) or (:spill slot)."
  (let ((entry (assoc vreg allocation)))
    (if entry (cdr entry) :x0)))

(defun emit-load-vreg (vreg allocation dest-reg)
  "Emit code to load vreg into dest-reg.
   If vreg is in a register, emit MOV. If spilled, emit LDR from stack."
  (let ((loc (vreg-to-reg vreg allocation)))
    (if (and (consp loc) (eq (car loc) :spill))
        ;; Spilled: load from stack (spill area starts at sp+0x40)
        (arm64:ldr dest-reg :sp :offset (spill-offset (cadr loc)))
        ;; In register: move if different
        (if (eq loc dest-reg)
            nil  ; Already in dest
            (arm64:mov dest-reg loc)))))

(defun emit-store-vreg (vreg allocation src-reg)
  "Emit code to store src-reg to vreg's location.
   If vreg is in a register, emit MOV. If spilled, emit STR to stack."
  (let ((loc (vreg-to-reg vreg allocation)))
    (if (and (consp loc) (eq (car loc) :spill))
        ;; Spilled: store to stack (spill area starts at sp+0x40)
        (arm64:str src-reg :sp :offset (spill-offset (cadr loc)))
        ;; In register: move if different
        (if (eq loc src-reg)
            nil  ; Already there
            (arm64:mov loc src-reg)))))

(defun gen-str-bytes-code (str start-offset)
  "Generate code to store string bytes at heap offset.
   Stores 8 bytes at a time for efficiency."
  (unless (stringp str)
    (error "gen-str-bytes-code: expected string, got ~S" str))
  (labels
      ;; Convert string to list of char codes
      ((str-to-bytes (s i acc)
         (if (>= i (length s))
             (reverse acc)
             (str-to-bytes s (+ i 1) (cons (char-code (char s i)) acc))))
       ;; Generate stores for chunks of 8 bytes
       ;; Uses x8 as scratch (reserved for runtime, not allocatable)
       (gen-stores (offset bytes acc)
         (if (null bytes)
             acc
             (let* ((chunk (take-bytes bytes 8))
                    (val (bytes-to-u64 chunk))
                    (rest (drop-bytes bytes 8)))
               (gen-stores
                (+ offset 8)
                rest
                (append acc
                        (load-addr :x8 val)
                        (arm64:str :x8 :heap :offset offset)))))))
    (let* ((bytes (str-to-bytes str 0 nil))
           ;; Add null terminator
           (bytes-with-nul (append bytes (list 0))))
      (gen-stores start-offset bytes-with-nul nil))))

(defun tac-codegen-instr (instr allocation)
  "Generate ARM64 code for a single TAC instruction.
   Returns list of instruction bytes (each instruction is 4 bytes)."
  (let ((op (car instr)))
    (case op
      ;; tac-lit: load literal into vreg
      ;; Value must be tagged as fixnum (value << 4, tag 0)
      ;; Uses load-addr to handle constants > 16 bits (movz + movk)
      ((tac-lit)
       (let* ((vreg (cadr instr))
              (value (caddr instr))
              (tagged (ash value 4))  ; Fixnum tagging: value << 4
              (dest (vreg-to-reg vreg allocation)))
         (if (and (consp dest) (eq (car dest) :spill))
             ;; Spilled: load to x0, then store
             (append (load-addr :x0 tagged)
                     (arm64:str :x0 :sp :offset (spill-offset (cadr dest))))
             ;; In register
             (load-addr dest tagged))))

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
                     (arm64:str :x0 :sp :offset (spill-offset (cadr dest))))
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
             (append (arm64:ldr :x0 :sp :offset (spill-offset (cadr src)))
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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr left))))
          (when (and (consp right) (eq (car right) :spill))
            (arm64:ldr :x1 :sp :offset (spill-offset (cadr right))))
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
            ((:BSH)
             ;; Bit shift: positive = left, negative = right
             ;; Untag both operands, branch on sign, retag result
             (append (arm64:asr left-reg left-reg 4 :imm t)    ; untag value
                     (arm64:asr right-reg right-reg 4 :imm t)  ; untag amount
                     (arm64:cmp right-reg 0 :imm t)            ; compare amount to 0
                     (arm64:b.lt 3)                            ; if negative, jump to right shift
                     ;; Positive (left shift)
                     (arm64:lsl dest-reg left-reg right-reg)   ; LSLV
                     (arm64:b 3)                               ; skip right shift
                     ;; Negative (right shift)
                     (arm64:neg right-reg right-reg)           ; negate to get positive amount
                     (arm64:asr dest-reg left-reg right-reg)   ; ASRV
                     ;; Retag result
                     (arm64:lsl dest-reg dest-reg 4 :imm t)))
            ((:BAND)
             ;; Bitwise AND - works directly on tagged values
             (arm64:and* dest-reg left-reg right-reg))
            ((:BOR)
             ;; Bitwise OR - works directly on tagged values
             (arm64:orr dest-reg left-reg right-reg))
            ((:BXOR)
             ;; Bitwise XOR - need to preserve tag bits
             ;; XOR the values, then restore tag (low 4 bits should be 0 for fixnums)
             (arm64:eor dest-reg left-reg right-reg))
            (t (error "tac-codegen-instr: Unknown binop: ~A" binop)))
          ;; Store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-cmp: comparison (result is tagged nil=6 or t=16)
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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr left))))
          (when (and (consp right) (eq (car right) :spill))
            (arm64:ldr :x1 :sp :offset (spill-offset (cadr right))))
          ;; Compare
          (arm64:cmp left-reg right-reg)
          ;; Set result based on condition (0 or 1)
          (case cmp-op
            ((:CMP-EQ) (arm64:cset dest-reg arm64:+eq+))
            ((:CMP-NE) (arm64:cset dest-reg arm64:+ne+))
            ((:CMP-LT) (arm64:cset dest-reg arm64:+lt+))
            ((:CMP-LE) (arm64:cset dest-reg arm64:+le+))
            ((:CMP-GT) (arm64:cset dest-reg arm64:+gt+))
            ((:CMP-GE) (arm64:cset dest-reg arm64:+ge+))
            (t (error "tac-codegen-instr: Unknown comparison op: ~A" cmp-op)))
          ;; Convert 0/1 to tagged nil(6)/t(16):
          ;; neg dest, dest  => -1 (all 1s) or 0
          ;; and dest, dest, #10 => 10 or 0 (use x2 as scratch for mask)
          ;; add dest, dest, #6 => 16 or 6
          (arm64:neg dest-reg dest-reg)
          (arm64:movz :x2 10)
          (arm64:and* dest-reg dest-reg :x2)
          (arm64:add dest-reg dest-reg 6 :imm t)
          ;; Store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-move: copy between vregs
      ((tac-move)
       (let* ((dest-vreg (cadr instr))
              (src-vreg (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (src (vreg-to-reg src-vreg allocation)))
         (cond
           ;; Both spilled
           ((and (consp dest) (consp src))
            (append (arm64:ldr :x0 :sp :offset (spill-offset (cadr src)))
                    (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))
           ;; Src spilled
           ((consp src)
            (arm64:ldr dest :sp :offset (spill-offset (cadr src))))
           ;; Dest spilled
           ((consp dest)
            (arm64:str src :sp :offset (spill-offset (cadr dest))))
           ;; Both in registers
           ((eq dest src) nil)
           (t (arm64:mov dest src)))))

      ;; tac-return: move result to x0
      ((tac-return)
       (let* ((vreg (cadr instr))
              (src (vreg-to-reg vreg allocation)))
         (if (and (consp src) (eq (car src) :spill))
             (arm64:ldr :x0 :sp :offset (spill-offset (cadr src)))
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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr cond-loc))))
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
              ;; Determine which caller-saved registers are actually allocated
              ;; by looking at the allocation map. Only save those that are in use.
              (allocatable '(:x9 :x10 :x11 :x12 :x13 :x14 :x15))
              (used-regs (remove-duplicates
                          (remove-if-not
                           (lambda (r) (member r allocatable))
                           (mapcar #'cdr allocation))))
              ;; Generate saves to stack at offsets 0x3850+ (caller-save area)
              ;; Frame layout (16KB frame):
              ;;   0x10-0x38: saved callee-save regs (x19-x24)
              ;;   0x40-0x3840: temp slots (1792 slots for linear codegen)
              ;;   0x3850-0x38F0: caller-save area (13 slots for x9-x15 + args)
              ;;   0x3F80: env pointer (x20)
              ;;   0x3FF0/0x3FF8: saved fp/lr
              ;; This avoids corrupting temps when saving regs before calls
              (save-code nil)
              (save-offset #x3850)
              (reg-offsets nil))  ; Track which reg is at which offset for restore
         ;; Save only actually-used caller-saved registers
         (dolist (reg used-regs)
           (setq save-code (append save-code
                                   (arm64:str reg :sp :offset save-offset)))
           (setq reg-offsets (cons (cons reg save-offset) reg-offsets))
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
                                            (arm64:ldr arg-reg :sp :offset (spill-offset (cadr arg-loc)))))
                     ;; Move from allocated reg to arg register
                     (unless (eq arg-loc arg-reg)
                       (setq arg-code (append arg-code (arm64:mov arg-reg arg-loc))))))
               (setq arg-idx (+ arg-idx 1))))
           ;; Generate call marker (resolved by resolve-calls)
           (let ((call-marker (list (list :call-fn fn-name)))
                 ;; Restore only the registers we saved
                 (restore-code nil))
             (dolist (reg-off reg-offsets)
               (setq restore-code (append restore-code
                                          (arm64:ldr (car reg-off) :sp :offset (cdr reg-off)))))
            ;; Recompute x20 (env) = sp + 0x3F80 (must match fn-fixed-prologue in codegen.lisp)
            ;; We can't just load from sp+24 - that's the CALLER's x20, not ours
            (setq restore-code (append restore-code
                                        (arm64:add :env :sp #x3 :imm t :shift12 t)   ; x20 = sp + 0x3000
                                        (arm64:add :env :env #xF80 :imm t)))          ; x20 = x20 + 0xF80
             ;; Move result from x0 to dest
             (let ((result-code
                     (if (and (consp dest) (eq (car dest) :spill))
                         (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))
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
                     (arm64:str :x0 :sp :offset (spill-offset (cadr dest))))
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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr car-loc))))
          (when (and (consp cdr-loc) (eq (car cdr-loc) :spill))
            (arm64:ldr :x1 :sp :offset (spill-offset (cadr cdr-loc))))
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
            (arm64:str :x2 :sp :offset (spill-offset (cadr dest)))))))

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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr src-loc))))
          ;; Clear tag bits to get base address
          (arm64:and* dest-reg src-reg -16 :imm t)
          ;; Load car from [base]
          (arm64:ldr dest-reg dest-reg :offset 0)
          ;; Store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr src-loc))))
          ;; Clear tag bits to get base address
          (arm64:and* dest-reg src-reg -16 :imm t)
          ;; Load cdr from [base+8]
          (arm64:ldr dest-reg dest-reg :offset 8)
          ;; Store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-if-not: conditional branch on nil
      ((tac-if-not)
       (let* ((cond-vreg (cadr instr))
              (target-label (caddr instr))
              (cond-loc (vreg-to-reg cond-vreg allocation))
              (cond-reg (if (and (consp cond-loc) (eq (car cond-loc) :spill)) :x0 cond-loc)))
         (append
          ;; Load condition if spilled
          (when (and (consp cond-loc) (eq (car cond-loc) :spill))
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr cond-loc))))
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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr cons-loc))))
          (when (and (consp val-loc) (eq (car val-loc) :spill))
            (arm64:ldr :x1 :sp :offset (spill-offset (cadr val-loc))))
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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr cons-loc))))
          (when (and (consp val-loc) (eq (car val-loc) :spill))
            (arm64:ldr :x1 :sp :offset (spill-offset (cadr val-loc))))
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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr src-loc))))
          ;; AND with 0xF to get tag
          (arm64:and* dest-reg src-reg #xF :imm t)
          ;; Shift left 4 to make it a tagged fixnum
          (arm64:lsl dest-reg dest-reg 4 :imm t)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr vec-loc))))
          ;; Clear tag to get base address
          (arm64:and* dest-reg vec-reg -16 :imm t)
          ;; Load length from header (first word)
          (arm64:ldr dest-reg dest-reg :offset 0)
          ;; Tag as fixnum
          (arm64:lsl dest-reg dest-reg 4 :imm t)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr str-loc))))
          ;; Clear tag to get base address
          (arm64:and* dest-reg str-reg -16 :imm t)
          ;; Load length from header
          (arm64:ldr dest-reg dest-reg :offset 0)
          ;; Tag as fixnum
          (arm64:lsl dest-reg dest-reg 4 :imm t)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr vec-loc))))
          (when (and (consp idx-loc) (eq (car idx-loc) :spill))
            (arm64:ldr :x1 :sp :offset (spill-offset (cadr idx-loc))))
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
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr vec-loc))))
          (when (and (consp idx-loc) (eq (car idx-loc) :spill))
            (arm64:ldr :x1 :sp :offset (spill-offset (cadr idx-loc))))
          (when (and (consp val-loc) (eq (car val-loc) :spill))
            (arm64:ldr :x2 :sp :offset (spill-offset (cadr val-loc))))
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
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr str-loc))))
          (when (and (consp idx-loc) (eq (car idx-loc) :spill))
            (arm64:ldr :x1 :sp :offset (spill-offset (cadr idx-loc))))
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
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-loop-start: marker for loop, no code generated
      ((tac-loop-start) nil)

      ;; tac-continue: jump back to loop - need loop label tracking
      ;; For now, emit as unresolved marker
      ((tac-continue)
       (list :continue-marker (cadr instr)))

      ;; tac-funcall: call through closure
      ;; Format: (tac-funcall dest fn-vr arg-vrs)
      ;; Closure format: [tagged-fn-offset (8), captured-env (8)] with tag 5
      ;; Call convention:
      ;;   - Set x24 to captured env (cons list from closure+8)
      ;;   - Extract fn-offset, add x26, call via BLR
      ((tac-funcall)
       (let* ((dest-vreg (cadr instr))
              (fn-vr (caddr instr))
              (arg-vrs (cadddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (fn-loc (vreg-to-reg fn-vr allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         ;; BUG FIX: Load arguments into x0-x7 FIRST, before loading closure to x9
         ;; This prevents clobbering args that may be allocated to x9
         (append
          (let ((arg-code nil)
                (arg-idx 0))
            (dolist (arg-vr arg-vrs)
              (let* ((arg-loc (vreg-to-reg arg-vr allocation))
                     (arg-reg (nth arg-idx '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7))))
                (when (< arg-idx 8)
                  (setq arg-code
                        (append arg-code
                                (if (and (consp arg-loc) (eq (car arg-loc) :spill))
                                    (arm64:ldr arg-reg :sp :offset (spill-offset (cadr arg-loc)))
                                    (unless (eq arg-loc arg-reg)
                                      (arm64:mov arg-reg arg-loc)))))
                  (setq arg-idx (+ arg-idx 1)))))
            arg-code)
          ;; Now load closure into x9 (after args are safely in x0-x7)
          (if (and (consp fn-loc) (eq (car fn-loc) :spill))
              (arm64:ldr :x9 :sp :offset (spill-offset (cadr fn-loc)))
              (arm64:mov :x9 fn-loc))
          ;; Extract captured env from closure+8, put in x24 for callee
          (arm64:sub :x9 :x9 5 :imm t)       ; remove closure tag
          (arm64:ldr :closure :x9 :offset 8) ; x24 = captured env
          ;; Extract code pointer from closure+0
          (arm64:ldr :x10 :x9 :offset 0)     ; load tagged fn-offset
          (arm64:lsr :x10 :x10 4 :imm t)     ; untag to get raw offset
          (arm64:add :x10 :x26 :x10)         ; add code base to get address
          (arm64:blr :x10)                   ; call through register
          ;; Result is in x0, move to dest
          (unless (eq dest-reg :x0)
            (arm64:mov dest-reg :x0))
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-make-closure: create closure object - complex
      ((tac-make-closure)
       ;; Emit as marker for later resolution (double-wrap so append keeps it as one item)
       (list (list :make-closure-marker instr)))

      ;; tac-str: string literal - allocate on heap inline
      ;; Format: (tac-str dest-vr string-value)
      ;; NOTE: GC trigger uses x8 as scratch and may call GC-COLLECT (clobbers x0-x7)
      ;; We use x19 (callee-saved) to preserve result across GC check
      ((tac-str)
       (let* ((dest-vreg (cadr instr))
              (str (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              ;; Always use x19 as temp, then move to dest after GC check
              (len (length str))
              (total-size (logand (+ len 8 15) (lognot 15))))
         (append
          ;; Store length at x28 (use x8 as scratch - reserved for runtime)
          (load-addr :x8 len)
          (arm64:str :x8 :heap :offset 0)
          ;; Store string bytes at x28+8 (gen-str-bytes-code uses x8)
          (gen-str-bytes-code str 8)
          ;; Get tagged pointer: x28 | 4 -- put in x19 (callee-saved, survives GC)
          (arm64:mov :x19 :heap)
          (arm64:add :x19 :x19 4 :imm t)
          ;; Bump heap
          (arm64:add :heap :heap total-size :imm t)
          ;; GC trigger check (uses x8, may call GC-COLLECT which clobbers x0-x7)
          (gc-trigger-code)
          ;; Now move from x19 to final destination
          (if (and (consp dest) (eq (car dest) :spill))
              ;; Spill slot destination
              (arm64:str :x19 :sp :offset (spill-offset (cadr dest)))
              ;; Register destination
              (arm64:mov dest :x19)))))

      ;; tac-sym: symbol literal - allocate on heap inline
      ;; Format: (tac-sym dest-vr symbol-name-string)
      ;; NOTE: GC trigger uses x8 as scratch and may call GC-COLLECT (clobbers x0-x7)
      ;; We use x19 (callee-saved) to preserve result across GC check
      ((tac-sym)
       (let* ((dest-vreg (cadr instr))
              (name (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              ;; Always use x19 as temp, then move to dest after GC check
              (len (length name))
              (total-size (logand (+ len 8 15) (lognot 15))))
         (append
          ;; Store length at x28 (use x8 as scratch - reserved for runtime)
          (load-addr :x8 len)
          (arm64:str :x8 :heap :offset 0)
          ;; Store symbol name bytes at x28+8 (gen-str-bytes-code uses x8)
          (gen-str-bytes-code name 8)
          ;; Get tagged pointer: x28 | 2 (symbol tag) -- put in x19 (callee-saved, survives GC)
          (arm64:mov :x19 :heap)
          (arm64:add :x19 :x19 2 :imm t)
          ;; Bump heap
          (arm64:add :heap :heap total-size :imm t)
          ;; GC trigger check (uses x8, may call GC-COLLECT which clobbers x0-x7)
          (gc-trigger-code)
          ;; Now move from x19 to final destination
          (if (and (consp dest) (eq (car dest) :spill))
              ;; Spill slot destination
              (arm64:str :x19 :sp :offset (spill-offset (cadr dest)))
              ;; Register destination
              (arm64:mov dest :x19)))))

      ;; tac-make-vector: allocate vector on heap
      ;; Format: (tac-make-vector dest-vreg size-vreg)
      ;; Vector layout: [length (8 bytes)] [data (n * 8 bytes)]
      ((tac-make-vector)
       (let* ((dest-vreg (cadr instr))
              (size-vreg (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (size-loc (vreg-to-reg size-vreg allocation)))
         (append
          ;; Load size to x0
          (if (and (consp size-loc) (eq (car size-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr size-loc)))
              (arm64:mov :x0 size-loc))
          ;; GC pre-check
          (gc-trigger-code)
          ;; x0 = tagged size, store untagged length at [x28+0]
          (arm64:lsr :x1 :x0 4 :imm t)           ; x1 = untagged length
          (arm64:str :x1 :heap :offset 0)        ; [x28+0] = length
          ;; Calculate allocation size: 8 + (x0 >> 1)
          (arm64:lsr :x1 :x0 1 :imm t)           ; x1 = untagged_size * 8
          (arm64:add :x1 :x1 8 :imm t)           ; x1 = 8 + data_size
          ;; Round to 16-byte alignment
          (arm64:add :x1 :x1 15 :imm t)
          (arm64:and* :x1 :x1 -16 :imm t)
          ;; Return tagged pointer, bump heap
          (arm64:mov :x19 :heap)                 ; x19 = base (callee-saved, survives GC)
          (arm64:add :heap :heap :x1)
          ;; Tag with vector tag (0x3)
          (arm64:add :x19 :x19 3 :imm t)
          ;; GC trigger check
          (gc-trigger-code)
          ;; Move result to destination
          (if (and (consp dest) (eq (car dest) :spill))
              (arm64:str :x19 :sp :offset (spill-offset (cadr dest)))
              (arm64:mov dest :x19)))))

      ;; tac-make-string: make string from vector of char codes
      ;; Format: (tac-make-string dest-vreg vec-vreg)
      ((tac-make-string)
       (let* ((dest-vreg (cadr instr))
              (vec-vreg (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (vec-loc (vreg-to-reg vec-vreg allocation)))
         (append
          ;; Load vector to x1
          (if (and (consp vec-loc) (eq (car vec-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr vec-loc)))
              (arm64:mov :x1 vec-loc))
          (arm64:sub :x1 :x1 3 :imm t)           ; untag vector
          ;; x5 = vec length
          (arm64:ldr :x5 :x1 :offset 0)
          ;; GC pre-check
          (gc-trigger-code)
          ;; Allocate string: store length at [x28]
          (arm64:str :x5 :heap :offset 0)
          ;; x4 = alloc size = (8 + len + 15) & ~15
          (arm64:add :x4 :x5 23 :imm t)
          (arm64:and* :x4 :x4 -16 :imm t)
          ;; x19 = string base (callee-saved, survives GC), bump heap
          (arm64:mov :x19 :heap)
          (arm64:add :heap :heap :x4)
          ;; x2 = string data = x19 + 8
          (arm64:add :x2 :x19 8 :imm t)
          ;; x3 = loop counter = 0
          (arm64:movz :x3 0)
          ;; Loop: copy chars from vector to string
          ;; Offsets: cmp=0, b.ge=1, body=2-8, b=9, exit=10
          (arm64:cmp :x3 :x5)                    ; 0: compare counter with length
          (arm64:b.ge 9)                         ; 1: skip 9 instrs to exit (instr 10)
          ;; Load vec[x3]: address = x1 + 8 + x3*8
          (arm64:lsl :x4 :x3 3 :imm t)           ; 2: x4 = x3 * 8
          (arm64:add :x4 :x4 8 :imm t)           ; 3: x4 = 8 + x3*8
          (arm64:add :x4 :x1 :x4)                ; 4: x4 = vec_base + offset
          (arm64:ldr :x4 :x4 :offset 0)          ; 5: x4 = tagged fixnum
          (arm64:lsr :x4 :x4 4 :imm t)           ; 6: x4 = char value
          (arm64:strb :x4 :x2 :x3 :reg t)        ; 7: [x2 + x3] = x4 (byte)
          (arm64:add :x3 :x3 1 :imm t)           ; 8: x3++
          (arm64:b -9)                           ; 9: back to cmp (instr 0)
          ;; Tag result with string tag (4)
          (arm64:add :x19 :x19 4 :imm t)         ; 10: exit point
          ;; Move result to destination
          (if (and (consp dest) (eq (car dest) :spill))
              (arm64:str :x19 :sp :offset (spill-offset (cadr dest)))
              (arm64:mov dest :x19)))))

      ;; tac-get-global-vars: load from [x27 + 104]
      ((tac-get-global-vars)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:ldr dest-reg :gc :offset 104)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-set-global-vars: store to [x27 + 104]
      ((tac-set-global-vars)
       (let* ((val-vreg (cadr instr))
              (val-loc (vreg-to-reg val-vreg allocation))
              (val-reg (if (and (consp val-loc) (eq (car val-loc) :spill)) :x0 val-loc)))
         (append
          (when (and (consp val-loc) (eq (car val-loc) :spill))
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr val-loc))))
          (arm64:str val-reg :gc :offset 104))))

      ;; tac-get-cmdline-args: return nil for now
      ((tac-get-cmdline-args)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:movz dest-reg 6)  ; nil
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-sys-exit: exit via libSystem stub
      ((tac-sys-exit)
       (let* ((val-vreg (cadr instr))
              (val-loc (vreg-to-reg val-vreg allocation))
              (val-reg (if (and (consp val-loc) (eq (car val-loc) :spill)) :x0 val-loc)))
         (append
          (when (and (consp val-loc) (eq (car val-loc) :spill))
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr val-loc))))
          (arm64:lsr :x0 val-reg 4 :imm t)  ; untag
          (list (list :extern-call "_exit")))))

      ;; tac-sys-open: open(path, flags, mode) via libSystem stub
      ((tac-sys-open)
       (let* ((dest-vreg (cadr instr))
              (path-vreg (caddr instr))
              (flags-vreg (cadddr instr))
              (mode-vreg (nth 4 instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (path-loc (vreg-to-reg path-vreg allocation))
              (flags-loc (vreg-to-reg flags-vreg allocation))
              (mode-loc (vreg-to-reg mode-vreg allocation)))
         (append
          ;; Load path string, skip tag and length
          (if (and (consp path-loc) (eq (car path-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr path-loc)))
              (arm64:mov :x0 path-loc))
          (arm64:sub :x0 :x0 4 :imm t)  ; untag string
          (arm64:add :x0 :x0 8 :imm t)  ; skip length
          ;; Load flags
          (if (and (consp flags-loc) (eq (car flags-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr flags-loc)))
              (arm64:mov :x1 flags-loc))
          (arm64:lsr :x1 :x1 4 :imm t)
          ;; Load mode
          (if (and (consp mode-loc) (eq (car mode-loc) :spill))
              (arm64:ldr :x2 :sp :offset (spill-offset (cadr mode-loc)))
              (arm64:mov :x2 mode-loc))
          (arm64:lsr :x2 :x2 4 :imm t)
          ;; call via libSystem stub
          (list (list :extern-call "_open"))
          (arm64:lsl :x0 :x0 4 :imm t)  ; tag result
          (if (and (consp dest) (eq (car dest) :spill))
              (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))
              (unless (eq dest :x0) (arm64:mov dest :x0))))))

      ;; tac-sys-read: read(fd, buf, len) via libSystem stub
      ((tac-sys-read)
       (let* ((dest-vreg (cadr instr))
              (fd-vreg (caddr instr))
              (buf-vreg (cadddr instr))
              (len-vreg (nth 4 instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (fd-loc (vreg-to-reg fd-vreg allocation))
              (buf-loc (vreg-to-reg buf-vreg allocation))
              (len-loc (vreg-to-reg len-vreg allocation)))
         (append
          (if (and (consp fd-loc) (eq (car fd-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr fd-loc)))
              (arm64:mov :x0 fd-loc))
          (arm64:lsr :x0 :x0 4 :imm t)
          (if (and (consp buf-loc) (eq (car buf-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr buf-loc)))
              (arm64:mov :x1 buf-loc))
          (arm64:sub :x1 :x1 3 :imm t)  ; untag vector
          (arm64:add :x1 :x1 8 :imm t)  ; skip length
          (if (and (consp len-loc) (eq (car len-loc) :spill))
              (arm64:ldr :x2 :sp :offset (spill-offset (cadr len-loc)))
              (arm64:mov :x2 len-loc))
          (arm64:lsr :x2 :x2 4 :imm t)
          (list (list :extern-call "_read"))
          (arm64:lsl :x0 :x0 4 :imm t)
          (if (and (consp dest) (eq (car dest) :spill))
              (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))
              (unless (eq dest :x0) (arm64:mov dest :x0))))))

      ;; tac-sys-write: write(fd, buf, len) via libSystem stub
      ;; Buffer can be string (tag 4) or vector (tag 3), so use AND to clear tag bits
      ((tac-sys-write)
       (let* ((dest-vreg (cadr instr))
              (fd-vreg (caddr instr))
              (buf-vreg (cadddr instr))
              (len-vreg (nth 4 instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (fd-loc (vreg-to-reg fd-vreg allocation))
              (buf-loc (vreg-to-reg buf-vreg allocation))
              (len-loc (vreg-to-reg len-vreg allocation)))
         (append
          (if (and (consp fd-loc) (eq (car fd-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr fd-loc)))
              (arm64:mov :x0 fd-loc))
          (arm64:lsr :x0 :x0 4 :imm t)
          (if (and (consp buf-loc) (eq (car buf-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr buf-loc)))
              (arm64:mov :x1 buf-loc))
          (arm64:and* :x1 :x1 -8 :imm t)  ; clear tag bits (works for string tag 4 or vector tag 3)
          (arm64:add :x1 :x1 8 :imm t)
          (if (and (consp len-loc) (eq (car len-loc) :spill))
              (arm64:ldr :x2 :sp :offset (spill-offset (cadr len-loc)))
              (arm64:mov :x2 len-loc))
          (arm64:lsr :x2 :x2 4 :imm t)
          (list (list :extern-call "_write"))
          (arm64:lsl :x0 :x0 4 :imm t)
          (if (and (consp dest) (eq (car dest) :spill))
              (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))
              (unless (eq dest :x0) (arm64:mov dest :x0))))))

      ;; tac-sys-close: close(fd) via libSystem stub
      ((tac-sys-close)
       (let* ((dest-vreg (cadr instr))
              (fd-vreg (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (fd-loc (vreg-to-reg fd-vreg allocation)))
         (append
          (if (and (consp fd-loc) (eq (car fd-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr fd-loc)))
              (arm64:mov :x0 fd-loc))
          (arm64:lsr :x0 :x0 4 :imm t)
          (list (list :extern-call "_close"))
          (arm64:lsl :x0 :x0 4 :imm t)
          (if (and (consp dest) (eq (car dest) :spill))
              (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))
              (unless (eq dest :x0) (arm64:mov dest :x0))))))

      ;; tac-buffer-to-string: convert raw byte buffer to string
      ;; (tac-buffer-to-string dest buf-vr len-vr)
      ((tac-buffer-to-string)
       (let* ((dest-vreg (cadr instr))
              (buf-vreg (caddr instr))
              (len-vreg (cadddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (buf-loc (vreg-to-reg buf-vreg allocation))
              (len-loc (vreg-to-reg len-vreg allocation)))
         ;; Register usage: x0=result, x1=buf_data, x2=str_data, x3=counter, x4=temp, x5=len
         (append
          ;; Load len into x5, untag
          (if (and (consp len-loc) (eq (car len-loc) :spill))
              (arm64:ldr :x5 :sp :offset (spill-offset (cadr len-loc)))
              (arm64:mov :x5 len-loc))
          (arm64:lsr :x5 :x5 4 :imm t)  ; x5 = len >> 4 (untag)
          ;; Load buf into x1, untag, add 8 to skip length header
          (if (and (consp buf-loc) (eq (car buf-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr buf-loc)))
              (arm64:mov :x1 buf-loc))
          (arm64:and* :x1 :x1 -8 :imm t)  ; x1 = buf & ~7 (clear tag)
          (arm64:add :x1 :x1 8 :imm t)    ; x1 = buf + 8 (skip length header)
          ;; GC pre-check BEFORE writing to heap
          (gc-trigger-code)
          ;; Allocate string: store length at [x28]
          (arm64:str :x5 :heap :offset 0)  ; [x28+0] = length
          ;; x4 = alloc size = (8 + len + 15) & ~15 for 16-byte alignment
          (arm64:add :x4 :x5 23 :imm t)    ; x4 = len + 23 (= len + 8 + 15)
          (arm64:and* :x4 :x4 -16 :imm t)  ; x4 = (len + 23) & ~15
          ;; Save string ptr (will be result), bump heap
          (arm64:mov :x0 :heap)            ; x0 = string base (untagged)
          (arm64:add :heap :heap :x4)      ; x28 += alloc_size
          ;; x2 = string data base = x0 + 8
          (arm64:add :x2 :x0 8 :imm t)     ; x2 = string data start
          ;; x3 = loop counter = 0
          (arm64:movz :x3 0)               ; x3 = 0
          ;; Loop: while x3 < x5
          (arm64:cmp :x3 :x5)              ; cmp x3, x5
          (arm64:b.ge 5)                   ; if x3 >= x5, jump to loop_end (+5 instructions)
          ;; Load buf[x3] - raw byte (using register offset mode)
          (arm64:ldrb :x4 :x1 :x3 :reg t)  ; x4 = byte at [x1 + x3]
          ;; Store byte: str_data[x3] = x4 (using register offset mode)
          (arm64:strb :x4 :x2 :x3 :reg t)  ; [x2 + x3] = x4
          ;; x3++
          (arm64:add :x3 :x3 1 :imm t)     ; x3++
          ;; Jump back to loop_start (cmp instruction)
          (arm64:b -5)                     ; back 5 instructions
          ;; loop_end: Tag result with string tag (0x4)
          ;; Note: pointer is 16-byte aligned, so add 4 == orr 4
          (arm64:add :x0 :x0 4 :imm t)     ; x0 += 4 (string tag)
          ;; Move result to destination
          (if (and (consp dest) (eq (car dest) :spill))
              (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))
              (unless (eq dest :x0) (arm64:mov dest :x0))))))

      ;; tac-buffer-byte-set: set byte in buffer
      ((tac-buffer-byte-set)
       (let* ((dest-vreg (cadr instr))
              (buf-vreg (caddr instr))
              (idx-vreg (cadddr instr))
              (val-vreg (nth 4 instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (buf-loc (vreg-to-reg buf-vreg allocation))
              (idx-loc (vreg-to-reg idx-vreg allocation))
              (val-loc (vreg-to-reg val-vreg allocation)))
         (append
          (if (and (consp buf-loc) (eq (car buf-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr buf-loc)))
              (arm64:mov :x0 buf-loc))
          (arm64:sub :x0 :x0 3 :imm t)
          (arm64:add :x0 :x0 8 :imm t)
          (if (and (consp idx-loc) (eq (car idx-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr idx-loc)))
              (arm64:mov :x1 idx-loc))
          (arm64:lsr :x1 :x1 4 :imm t)
          (arm64:add :x0 :x0 :x1)
          (if (and (consp val-loc) (eq (car val-loc) :spill))
              (arm64:ldr :x2 :sp :offset (spill-offset (cadr val-loc)))
              (arm64:mov :x2 val-loc))
          (arm64:lsr :x2 :x2 4 :imm t)
          (arm64:strb :x2 :x0 :offset 0)
          (arm64:movz :x0 6)  ; return nil
          (if (and (consp dest) (eq (car dest) :spill))
              (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))
              (unless (eq dest :x0) (arm64:mov dest :x0))))))

      ;; tac-buffer-byte-ref: get byte from buffer at index
      ;; Format: (tac-buffer-byte-ref dest buf-vr idx-vr)
      ;; Vector layout: [length (8 bytes)][raw bytes...]
      ((tac-buffer-byte-ref)
       (let* ((dest-vreg (cadr instr))
              (buf-vreg (caddr instr))
              (idx-vreg (cadddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (buf-loc (vreg-to-reg buf-vreg allocation))
              (idx-loc (vreg-to-reg idx-vreg allocation)))
         (append
          ;; Load buf into x0, untag vector (sub 3), add 8 to skip length
          (if (and (consp buf-loc) (eq (car buf-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr buf-loc)))
              (arm64:mov :x0 buf-loc))
          (arm64:sub :x0 :x0 3 :imm t)  ; untag vector (tag 3)
          (arm64:add :x0 :x0 8 :imm t)  ; skip length
          ;; Load idx into x1, untag, add to address
          (if (and (consp idx-loc) (eq (car idx-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr idx-loc)))
              (arm64:mov :x1 idx-loc))
          (arm64:lsr :x1 :x1 4 :imm t)  ; untag index
          (arm64:add :x0 :x0 :x1)       ; x0 = address of byte
          ;; Load byte and tag as fixnum
          (arm64:ldrb :x0 :x0 0)        ; load byte (zero-extended)
          (arm64:lsl :x0 :x0 4 :imm t)  ; tag as fixnum
          ;; Store result
          (if (and (consp dest) (eq (car dest) :spill))
              (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))
              (unless (eq dest :x0) (arm64:mov dest :x0))))))

      ;; tac-mem-set-byte: set byte at ptr + offset
      ((tac-mem-set-byte)
       (let* ((dest-vreg (cadr instr))
              (ptr-vreg (caddr instr))
              (off-vreg (cadddr instr))
              (val-vreg (nth 4 instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (ptr-loc (vreg-to-reg ptr-vreg allocation))
              (off-loc (vreg-to-reg off-vreg allocation))
              (val-loc (vreg-to-reg val-vreg allocation)))
         (append
          (if (and (consp ptr-loc) (eq (car ptr-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr ptr-loc)))
              (arm64:mov :x0 ptr-loc))
          (arm64:lsr :x0 :x0 4 :imm t)
          (if (and (consp off-loc) (eq (car off-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr off-loc)))
              (arm64:mov :x1 off-loc))
          (arm64:lsr :x1 :x1 4 :imm t)
          (arm64:add :x0 :x0 :x1)
          (if (and (consp val-loc) (eq (car val-loc) :spill))
              (arm64:ldr :x2 :sp :offset (spill-offset (cadr val-loc)))
              (arm64:mov :x2 val-loc))
          (arm64:lsr :x2 :x2 4 :imm t)
          (arm64:strb :x2 :x0 :offset 0)
          (arm64:movz :x0 6)
          (if (and (consp dest) (eq (car dest) :spill))
              (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))
              (unless (eq dest :x0) (arm64:mov dest :x0))))))

      ;; tac-mem-load-64: load 64-bit from ptr + offset
      ((tac-mem-load-64)
       (let* ((dest-vreg (cadr instr))
              (ptr-vreg (caddr instr))
              (off-vreg (cadddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (ptr-loc (vreg-to-reg ptr-vreg allocation))
              (off-loc (vreg-to-reg off-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (if (and (consp ptr-loc) (eq (car ptr-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr ptr-loc)))
              (arm64:mov :x0 ptr-loc))
          (arm64:lsr :x0 :x0 4 :imm t)
          (if (and (consp off-loc) (eq (car off-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr off-loc)))
              (arm64:mov :x1 off-loc))
          (arm64:lsr :x1 :x1 4 :imm t)
          (arm64:add :x0 :x0 :x1)
          (arm64:ldr dest-reg :x0 :offset 0)
          (arm64:lsl dest-reg dest-reg 4 :imm t)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-mem-load-byte: load single byte from ptr+offset, return as tagged fixnum
      ((tac-mem-load-byte)
       (let* ((dest-vreg (cadr instr))
              (ptr-vreg (caddr instr))
              (off-vreg (cadddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (ptr-loc (vreg-to-reg ptr-vreg allocation))
              (off-loc (vreg-to-reg off-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          ;; Load ptr and untag (ptr is a tagged fixnum)
          (if (and (consp ptr-loc) (eq (car ptr-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr ptr-loc)))
              (arm64:mov :x0 ptr-loc))
          (arm64:lsr :x0 :x0 4 :imm t)  ; untag pointer
          ;; Load offset and untag (offset is a fixnum)
          (if (and (consp off-loc) (eq (car off-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr off-loc)))
              (arm64:mov :x1 off-loc))
          (arm64:lsr :x1 :x1 4 :imm t)  ; untag offset
          (arm64:add :x0 :x0 :x1)       ; ptr + offset
          (arm64:ldrb dest-reg :x0 0)   ; load byte (zero-extended)
          (arm64:lsl dest-reg dest-reg 4 :imm t)  ; tag as fixnum
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-bnot: boolean not
      ((tac-bnot)
       (let* ((dest-vreg (cadr instr))
              (val-vreg (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (val-loc (vreg-to-reg val-vreg allocation))
              (val-reg (if (and (consp val-loc) (eq (car val-loc) :spill)) :x0 val-loc))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (when (and (consp val-loc) (eq (car val-loc) :spill))
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr val-loc))))
          (arm64:cmp val-reg 6 :imm t)  ; compare to nil
          (arm64:cset dest-reg arm64:+eq+)  ; 1 if nil, 0 otherwise
          ;; Convert 0/1 to nil(6)/t(16)
          (arm64:neg dest-reg dest-reg)
          (arm64:movz :x2 10)
          (arm64:and* dest-reg dest-reg :x2)
          (arm64:add dest-reg dest-reg 6 :imm t)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-mvn: bitwise NOT (ARM64 MVN instruction)
      ;; For tagged fixnums: untag, MVN, retag
      ((tac-mvn)
       (let* ((dest-vreg (cadr instr))
              (val-vreg (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (val-loc (vreg-to-reg val-vreg allocation))
              (val-reg (if (and (consp val-loc) (eq (car val-loc) :spill)) :x0 val-loc))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (when (and (consp val-loc) (eq (car val-loc) :spill))
            (arm64:ldr :x0 :sp :offset (spill-offset (cadr val-loc))))
          ;; Untag: ASR 4 (arithmetic shift to preserve sign)
          (arm64:asr :x1 val-reg 4 :imm t)
          ;; MVN: bitwise NOT
          (arm64:mvn dest-reg :x1)
          ;; Retag: LSL 4
          (arm64:lsl dest-reg dest-reg 4 :imm t)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-lambda-ref: create closure for lifted lambda
      ;; TAC format: (tac-lambda-ref dest-vreg lambda-name free-offsets)
      ;; Closure format: [tagged-fn-offset (8), captured-env (8)] with tag 5
      ;; tagged-fn-offset = (fn_addr - x26) << 4
      ((tac-lambda-ref)
       (let* ((dest-vreg (cadr instr))
              (lambda-name (caddr instr))
              (free-offsets (cadddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          ;; GC trigger check BEFORE allocation
          ;; Compare x28 (heap) with from_end at [x27 + 16]
          (arm64:ldr :x9 :gc :offset 16)    ; x9 = from_end
          (arm64:cmp :heap :x9)             ; compare heap ptr with from_end
          (arm64:b.lo 2)                    ; skip GC call if heap < from_end
          (list (list :call-fn 'GC-COLLECT))
          ;; Load lambda address via ADR into x9
          (list (list :lambda-ref-marker :x9 lambda-name))
          ;; Compute offset from code base: x9 = fn_addr - x26
          (arm64:sub :x9 :x9 :x26)
          ;; Tag as fixnum: x9 = offset << 4
          (arm64:lsl :x9 :x9 4 :imm t)
          ;; Store tagged fn-offset at heap+0
          (arm64:str :x9 :heap :offset 0)
          ;; Build captured env or nil at heap+8
          (if (null free-offsets)
              ;; No captures - store nil (6)
              (append
               (arm64:movz :x9 6)
               (arm64:str :x9 :heap :offset 8))
              ;; Has captures - build cons chain
              ;; Process offsets in reverse so first offset ends up as car
              ;; Gen code: start with nil, cons each value onto it
              (labels ((gen-capture-chain (offs)
                         ;; Generate code to build cons chain, result in x9
                         ;; Process from end to start of offs list
                         (if (null offs)
                             ;; Base case: x9 = nil (6)
                             (arm64:movz :x9 6)
                             ;; Recursive: first build rest of chain, then cons current
                             (let* ((rest-code (gen-capture-chain (cdr offs)))
                                    (off (car offs))
                                    (off8 (* off 8)))
                               (append
                                rest-code
                                ;; x9 has rest of list (cdr)
                                ;; Store cdr at heap+8
                                (arm64:str :x9 :heap :offset 8)
                                ;; Load captured value from [env - offset*8]
                                (arm64:sub :x10 :env off8 :imm t)
                                (arm64:ldr :x10 :x10 :offset 0)
                                ;; Store as car at heap+0
                                (arm64:str :x10 :heap :offset 0)
                                ;; Make cons pointer: x9 = heap | 1
                                (arm64:orr :x9 :heap 1 :imm t)
                                ;; Bump heap for this cons
                                (arm64:add :heap :heap 16 :imm t))))))
                ;; Save closure base, build chain, store at closure+8
                (append
                 ;; Save closure base in x11
                 (arm64:mov :x11 :heap)
                 ;; Bump heap past closure (16 bytes)
                 (arm64:add :heap :heap 16 :imm t)
                 ;; Build capture chain (result in x9)
                 ;; gen-capture-chain builds right-to-left, so first offset ends up as car
                 ;; DO NOT reverse - we want (val0 . (val1 . (val2 . nil)))
                 (gen-capture-chain free-offsets)
                 ;; Store captured env at closure+8
                 (arm64:str :x9 :x11 :offset 8))))
          ;; Create closure pointer: dest = heap_base + 5 (closure tag)
          ;; For no-captures case, heap still points to closure start
          ;; For captures case, x11 has closure base
          ;; Note: Use ADD not ORR since 5 is not a valid ORR immediate
          (if (null free-offsets)
              (append
               (arm64:add dest-reg :heap 5 :imm t)
               (arm64:add :heap :heap 16 :imm t))
              ;; With captures, closure base is in x11
              (arm64:add dest-reg :x11 5 :imm t))
          ;; Store if dest is spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-symbol-name: get symbol's name string
      ;; Symbol: [name-ptr | tag 2], name is string ptr at offset 0
      ((tac-symbol-name)
       (let* ((dest-vreg (cadr instr))
              (sym-vreg (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (sym-loc (vreg-to-reg sym-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (if (and (consp sym-loc) (eq (car sym-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr sym-loc)))
              (arm64:mov :x0 sym-loc))
          (arm64:sub :x0 :x0 2 :imm t)  ; untag symbol (tag 2)
          (arm64:ldr dest-reg :x0 :offset 0)  ; load name string ptr
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-make-symbol: create symbol from string name
      ;; Allocates: [name-ptr (8 bytes)] with tag 2
      ((tac-make-symbol)
       (let* ((dest-vreg (cadr instr))
              (name-vreg (caddr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (name-loc (vreg-to-reg name-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (if (and (consp name-loc) (eq (car name-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr name-loc)))
              (arm64:mov :x1 name-loc))
          ;; Store name-ptr at x28 (heap alloc ptr)
          (arm64:str :x1 :heap :offset 0)
          ;; Result = x28 | 2 (symbol tag)
          (arm64:mov dest-reg :heap)
          (arm64:add dest-reg dest-reg 2 :imm t)
          ;; Bump heap by 16 (8-byte aligned, minimum 16)
          (arm64:add :heap :heap 16 :imm t)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-string-concat: concatenate two strings
      ;; This is complex - emit marker for now
      ((tac-string-concat)
       (let* ((dest-vreg (cadr instr))
              (s1-vreg (caddr instr))
              (s2-vreg (cadddr instr))
              (dest (vreg-to-reg dest-vreg allocation)))
         (list (list :string-concat-marker dest s1-vreg s2-vreg))))

      ;; tac-string-equal: compare two strings
      ;; Inline implementation: compare lengths, then byte-by-byte
      ;; String layout: [length (8 bytes)][char data (n bytes)]
      ;; Returns: tagged 16 (t) or 6 (nil)
      ;; Register usage:
      ;;   x0: result
      ;;   x1: str1 base (untagged)
      ;;   x2: str2 base (untagged)
      ;;   x3: len1
      ;;   x4: len2 / loop counter
      ;;   x5: char from str1
      ;;   x6: char from str2
      ((tac-string-equal)
       (let* ((dest-vreg (cadr instr))
              (s1-vreg (caddr instr))
              (s2-vreg (cadddr instr))
              (s1-loc (vreg-to-reg s1-vreg allocation))
              (s2-loc (vreg-to-reg s2-vreg allocation))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          ;; Load s1 into x1
          (if (and (consp s1-loc) (eq (car s1-loc) :spill))
              (arm64:ldr :x1 :sp :offset (spill-offset (cadr s1-loc)))
              (arm64:mov :x1 s1-loc))
          ;; Load s2 into x2
          (if (and (consp s2-loc) (eq (car s2-loc) :spill))
              (arm64:ldr :x2 :sp :offset (spill-offset (cadr s2-loc)))
              (arm64:mov :x2 s2-loc))
          ;; Untag both strings: x1 = str1 & ~0xF, x2 = str2 & ~0xF
          (arm64:and* :x1 :x1 -16 :imm t)
          (arm64:and* :x2 :x2 -16 :imm t)
          ;; Load lengths
          (arm64:ldr :x3 :x1 :offset 0)  ; x3 = len1
          (arm64:ldr :x4 :x2 :offset 0)  ; x4 = len2
          ;; Compare lengths
          (arm64:cmp :x3 :x4)            ; cmp len1, len2
          (arm64:b.ne (ash 56 -2))       ; if len1 != len2, jump to return_false (+14 instrs = 56 bytes)
          ;; Lengths equal, setup for loop
          (arm64:add :x1 :x1 8 :imm t)   ; x1 = str1 data start
          (arm64:add :x2 :x2 8 :imm t)   ; x2 = str2 data start
          (arm64:movz :x4 0)             ; x4 = 0 (loop counter)
          ;; loop_start: (offset here, instruction 10)
          (arm64:cmp :x4 :x3)            ; cmp counter, len
          (arm64:b.ge (ash 28 -2))       ; if counter >= len, jump to return_true (+7 instrs = 28 bytes)
          ;; Load bytes from both strings
          (arm64:ldrb :x5 :x1 :x4 :reg t)  ; x5 = str1[counter]
          (arm64:ldrb :x6 :x2 :x4 :reg t)  ; x6 = str2[counter]
          ;; Compare bytes
          (arm64:cmp :x5 :x6)            ; cmp char1, char2
          (arm64:b.ne (ash 20 -2))       ; if char1 != char2, jump to return_false (+5 instrs = 20 bytes)
          ;; Increment counter
          (arm64:add :x4 :x4 1 :imm t)   ; x4++
          ;; Loop back to cmp at loop_start (7 instructions back)
          (arm64:b (ash -28 -2))         ; back 7 instructions = -28 bytes
          ;; return_true: (instruction 18)
          (arm64:movz dest-reg 16)       ; result = 16 (tagged t)
          (arm64:b (ash 8 -2))           ; skip return_false (+2 instrs = 8 bytes)
          ;; return_false: (instruction 20)
          (arm64:movz dest-reg 6)        ; result = 6 (nil tag)
          ;; end: (instruction 21) - store if spilled
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-get-intern-table: load from [x27 + 0]
      ((tac-get-intern-table)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:ldr dest-reg :gc :offset 0)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-set-intern-table: store to [x27 + 0]
      ((tac-set-intern-table)
       (let* ((val-vreg (cadr instr))
              (val-loc (vreg-to-reg val-vreg allocation)))
         (append
          (if (and (consp val-loc) (eq (car val-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr val-loc)))
              (arm64:mov :x0 val-loc))
          (arm64:str :x0 :gc :offset 0))))

      ;; tac-get-lambda-counter: load from [x27 + 8]
      ((tac-get-lambda-counter)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:ldr dest-reg :gc :offset 8)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-get-frame-pointer: get x29 as tagged fixnum for stack walking
      ;; Must be tagged since mem-load-64 expects tagged pointers
      ((tac-get-frame-pointer)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:mov dest-reg :x29)
          (arm64:lsl dest-reg dest-reg 4 :imm t)  ; tag as fixnum
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-get-code-base: get x26 as tagged fixnum for symbol table access
      ;; Must be tagged since mem-load-64/mem-load-byte expect tagged pointers
      ((tac-get-code-base)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:mov dest-reg :code-base)
          (arm64:lsl dest-reg dest-reg 4 :imm t)  ; tag as fixnum
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-get-symtab-offset: load from [x27 + 112]
      ;; Note: value is ALREADY tagged (pre-shifted << 4) in wrapper storage
      ((tac-get-symtab-offset)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:ldr dest-reg :gc :offset 112)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-get-symtab-count: load from [x27 + 120]
      ((tac-get-symtab-count)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:ldr dest-reg :gc :offset 120)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-set-lambda-counter: store to [x27 + 8]
      ((tac-set-lambda-counter)
       (let* ((val-vreg (cadr instr))
              (val-loc (vreg-to-reg val-vreg allocation)))
         (append
          (if (and (consp val-loc) (eq (car val-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr val-loc)))
              (arm64:mov :x0 val-loc))
          (arm64:str :x0 :gc :offset 8))))

      ;; tac-get-symbol-counter: load from [x27 + 48]
      ((tac-get-symbol-counter)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:ldr dest-reg :gc :offset 48)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-set-symbol-counter: store to [x27 + 48]
      ((tac-set-symbol-counter)
       (let* ((val-vreg (cadr instr))
              (val-loc (vreg-to-reg val-vreg allocation)))
         (append
          (if (and (consp val-loc) (eq (car val-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr val-loc)))
              (arm64:mov :x0 val-loc))
          (arm64:str :x0 :gc :offset 48))))

      ;; tac-get-symbol-table: load from [x27 + 56]
      ((tac-get-symbol-table)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:ldr dest-reg :gc :offset 56)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-set-symbol-table: store to [x27 + 56]
      ((tac-set-symbol-table)
       (let* ((val-vreg (cadr instr))
              (val-loc (vreg-to-reg val-vreg allocation)))
         (append
          (if (and (consp val-loc) (eq (car val-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr val-loc)))
              (arm64:mov :x0 val-loc))
          (arm64:str :x0 :gc :offset 56))))

      ;; tac-get-packages: load from [x27 + 80]
      ((tac-get-packages)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:ldr dest-reg :gc :offset 80)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-set-packages: store to [x27 + 80]
      ((tac-set-packages)
       (let* ((val-vreg (cadr instr))
              (val-loc (vreg-to-reg val-vreg allocation)))
         (append
          (if (and (consp val-loc) (eq (car val-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr val-loc)))
              (arm64:mov :x0 val-loc))
          (arm64:str :x0 :gc :offset 80))))

      ;; tac-get-current-package: load from [x27 + 88]
      ((tac-get-current-package)
       (let* ((dest-vreg (cadr instr))
              (dest (vreg-to-reg dest-vreg allocation))
              (dest-reg (if (and (consp dest) (eq (car dest) :spill)) :x0 dest)))
         (append
          (arm64:ldr dest-reg :gc :offset 88)
          (when (and (consp dest) (eq (car dest) :spill))
            (arm64:str :x0 :sp :offset (spill-offset (cadr dest)))))))

      ;; tac-set-current-package: store to [x27 + 88]
      ((tac-set-current-package)
       (let* ((val-vreg (cadr instr))
              (val-loc (vreg-to-reg val-vreg allocation)))
         (append
          (if (and (consp val-loc) (eq (car val-loc) :spill))
              (arm64:ldr :x0 :sp :offset (spill-offset (cadr val-loc)))
              (arm64:mov :x0 val-loc))
          (arm64:str :x0 :gc :offset 88))))

      ;; Default
      (t (error "tac-codegen-instr: Unhandled TAC instruction: ~A" op)))))

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
        (loop-markers nil)     ; alist of (marker . label) for TCO continue
        (current-pos 0))
    ;; Generate code for each instruction, tracking positions
    (dolist (instr tac-instrs)
      (cond
       ;; tac-loop-start: record marker → label mapping for TCO
       ((eq (car instr) 'tac-loop-start)
        (let ((label (cadr instr))
              (marker (caddr instr)))
          (setq loop-markers (cons (cons marker label) loop-markers))))

       ;; tac-label: record label position
       ((eq (car instr) 'tac-label)
        (let ((label (cadr instr)))
          (setq label-positions (cons (cons label current-pos) label-positions))
          (setq code-with-markers (append code-with-markers
                                          (list (list :label-marker label))))))

       ;; tac-continue: emit branch marker to loop start (resolve later)
       ((eq (car instr) 'tac-continue)
        (let* ((marker (cadr instr))
               (label (cdr (assoc marker loop-markers))))
          (when label
            (setq code-with-markers (append code-with-markers
                                            (list (list :branch-marker label))))
            (setq current-pos (+ current-pos 4)))))

       ;; All other instructions: generate code
       (t
        (let ((bytes (tac-codegen-instr instr allocation)))
          (when bytes
            (setq code-with-markers (append code-with-markers bytes))
            ;; Update position: count actual bytes AND known markers (4 bytes each)
            (dolist (b bytes)
              (cond
                ((numberp b)
                 (setq current-pos (+ current-pos 1)))
                ;; Known markers that will become 4-byte instructions
                ((and (consp b) (member (car b) '(:branch-marker :branch-ne-marker :branch-eq-marker :call-fn :tail-call-fn :lambda-ref-marker :extern-call)))
                 (setq current-pos (+ current-pos 4)))
                ;; Unknown marker - error immediately to prevent silent bugs
                ((and (consp b) (keywordp (car b)))
                 (error "tac-codegen Pass 1: Unknown marker ~S from instruction ~S - needs implementation" b instr)))))))))

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
                  (target-pos (cdr (assoc target-label label-positions))))
             (unless target-pos
               (error "tac-codegen: unresolved branch to label ~S" target-label))
             (let ((offset (ash (- target-pos pos) -2)))
               (setq resolved (append resolved (arm64:b offset)))
               (setq pos (+ pos 4)))))

          ;; Resolve conditional branch (branch if not equal)
          ((and (consp item) (eq (car item) :branch-ne-marker))
           (let* ((target-label (cadr item))
                  (target-pos (cdr (assoc target-label label-positions))))
             (unless target-pos
               (error "tac-codegen: unresolved branch-ne to label ~S" target-label))
             (let ((offset (ash (- target-pos pos) -2)))
               (setq resolved (append resolved (arm64:b.ne offset)))
               (setq pos (+ pos 4)))))

          ;; Resolve conditional branch (branch if equal)
          ((and (consp item) (eq (car item) :branch-eq-marker))
           (let* ((target-label (cadr item))
                  (target-pos (cdr (assoc target-label label-positions))))
             (unless target-pos
               (error "tac-codegen: unresolved branch-eq to label ~S" target-label))
             (let ((offset (ash (- target-pos pos) -2)))
               (setq resolved (append resolved (arm64:b.eq offset)))
               (setq pos (+ pos 4)))))

          ;; Function call marker - pass through for resolve-calls
          ((and (consp item) (eq (car item) :call-fn))
           (setq resolved (append resolved (list item)))
           (setq pos (+ pos 4)))  ; BL is 4 bytes

          ;; Heap allocation marker - pass through for has-unresolved-markers
          ((and (consp item) (eq (car item) :heap-alloc-marker))
           (setq resolved (append resolved (list item))))

          ;; Lambda-ref marker - pass through for link-time resolution
          ;; Format: (:lambda-ref-marker dest-reg lambda-name)
          ;; Will be resolved to ADR instruction by linker
          ((and (consp item) (eq (car item) :lambda-ref-marker))
           (setq resolved (append resolved (list item)))
           (setq pos (+ pos 4)))  ; ADR is 4 bytes

          ;; Tail call marker - pass through for resolve-calls
          ((and (consp item) (eq (car item) :tail-call-fn))
           (setq resolved (append resolved (list item)))
           (setq pos (+ pos 4)))  ; B is 4 bytes

          ;; Extern call marker - pass through for resolve-calls (libSystem stubs)
          ((and (consp item) (eq (car item) :extern-call))
           (setq resolved (append resolved (list item)))
           (setq pos (+ pos 4)))  ; BL is 4 bytes

          ;; Regular byte - keep it
          ((numberp item)
           (setq resolved (append resolved (list item)))
           (setq pos (+ pos 1)))

          ;; Unknown marker - ERROR instead of silently dropping
          ;; This catches unimplemented TAC instructions that emit markers
          ((consp item)
           (error "tac-codegen: Unhandled marker ~S - this TAC instruction needs implementation" item))

          ;; Truly unknown item type
          (t (error "tac-codegen: Unknown item type in code: ~S" item))))
      resolved)))

;;; ============================================================
;;; Top-Level Interface
;;; ============================================================

(defun allocate-registers-for-function (fn)
  "Apply full register allocation pipeline to a compiled function.
   fn has structure: (name params body-ir param-base)
   Returns: (name params body-ir param-base allocation tac)"
  ;; Initialize TCO tracking for this function
  (setf *tco-loop-label* nil)
  (setf *tco-loop-marker* nil)
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

(defun has-unresolved-markers (code)
  "Check if code contains any unresolved markers.
   Only these markers are VALID (resolved by linker or tac-codegen pass 2):
   - :call-fn, :tail-call-fn - resolved to BL/B by linker
   - :extern-call - resolved to BL to libSystem stub by linker
   - :lambda-ref-marker - resolved to ADR by linker
   - :branch-marker, :branch-ne-marker, :branch-eq-marker - resolved in pass 2
   - :label-marker - used for position tracking, stripped in pass 2
   ANY OTHER marker indicates incomplete codegen and should be flagged."
  (labels ((check (items)
             (cond
               ((null items) nil)
               ((and (consp (car items))
                     (keywordp (caar items))
                     ;; Check if NOT in the known-good list
                     (not (member (caar items)
                                  '(:call-fn :tail-call-fn :lambda-ref-marker
                                    :extern-call
                                    :branch-marker :branch-ne-marker :branch-eq-marker
                                    :label-marker))))
                ;; Unknown marker found - flag as unresolved
                t)
               (t (check (cdr items))))))
    (check code)))

(defun gen-capture-loads-reg (num-captures)
  "Generate code to load captured values from x24 cons list into env slots.
   x24 = (v0 . (v1 . (v2 . nil))) - load into offsets 0, 1, 2, etc.
   Each captured value goes at [x20 - idx*8]."
  (if (<= num-captures 0)
      nil
      (labels ((gen-loads (idx acc)
                 (if (>= idx num-captures)
                     acc
                     (let* ((offset (* idx 8)))
                       (gen-loads (+ idx 1)
                                  (append acc
                                          ;; x24 points to current cons cell
                                          ;; Load car: untag cons (tag 1), load car
                                          (arm64:sub :x9 :closure 1 :imm t)
                                          (arm64:ldr :x9 :x9 :offset 0)
                                          ;; Store at [x20 - offset]
                                          (arm64:sub :x10 :env offset :imm t)
                                          (arm64:str :x9 :x10 :offset 0)
                                          ;; Advance x24 to cdr: x24 = cdr(x24)
                                          (arm64:sub :x9 :closure 1 :imm t)
                                          (arm64:ldr :closure :x9 :offset 8)))))))
        (gen-loads 0 nil))))

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
         ;; For defuns, param-base IS the number of captures (from lambdas-to-defuns)
         ;; For raw lambdas, fourth is free-vars list
         (is-lambda (and fourth (listp fourth)))  ; nil or list = lambda, number = defun
         (num-captures (cond
                         ((numberp fourth) fourth)   ; defun: param-base = capture count
                         ((listp fourth) (length fourth))  ; lambda: free-vars list
                         (t 0)))
         (param-base num-captures)
         ;; Apply register allocation pipeline
         (counter (make-vreg-counter))
         (tac-result (ir-to-tac body-ir counter))
         (tac-instrs (car tac-result))
         (result-vr (cadr tac-result)))
    ;; Check if IR converted successfully
    (if (null tac-instrs)
        ;; IR not supported - return nil (caller will error)
        nil
        (let* (;; Add return instruction
               (full-tac (append tac-instrs (list (list 'tac-return result-vr))))
               ;; Liveness analysis
               (annotated (compute-liveness full-tac))
               ;; Compute intervals
               (intervals (compute-intervals annotated))
               ;; Linear scan allocation
               (allocation (linear-scan intervals))
               ;; Generate prologue - use fn-fixed-prologue for consistent frame layout
               (prologue-code (fn-fixed-prologue))
               ;; Generate capture loads (for lifted lambdas with captures, not defuns)
               (capture-code (gen-capture-loads-reg num-captures))
               ;; Generate param stores
               (param-code (reg-alloc-gen-param-stores params param-base))
               ;; Generate body code with allocation
               (body-code (tac-codegen full-tac allocation))
               ;; Generate epilogue - use fn-fixed-epilogue for consistent frame layout
               (epilogue-code (fn-fixed-epilogue))
               ;; Combine all code
               (all-code (append prologue-code capture-code param-code body-code epilogue-code)))
          ;; Check for unresolved markers - if present, return nil (caller will error)
          (if (has-unresolved-markers all-code)
              nil
              all-code)))))

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

(defun codegen-main-reg-alloc (mir)
  "Compile main program IR using register allocation.
   Returns list of ARM64 instruction bytes with call markers for resolution.
   This replaces the old accumulator-based codegen for main program code.

   The main program differs from functions in that:
   - No parameters to store
   - No captures to load
   - Same prologue/epilogue structure"
  (let* ((counter (make-vreg-counter))
         (tac-result (ir-to-tac mir counter))
         (tac-instrs (car tac-result))
         (result-vr (cadr tac-result)))
    (if (null tac-instrs)
        ;; IR not supported - error out, don't fall back to old codegen
        (error "codegen-main-reg-alloc: IR not supported by register allocator: ~S"
               (if (consp mir) (car mir) mir))
        (let* (;; Add return instruction (main returns result in x0)
               (full-tac (append tac-instrs (list (list 'tac-return result-vr))))
               ;; Liveness analysis
               (annotated (compute-liveness full-tac))
               ;; Compute intervals
               (intervals (compute-intervals annotated))
               ;; Linear scan allocation
               (allocation (linear-scan intervals))
               ;; Generate prologue - use fn-fixed-prologue for consistent frame layout
               (prologue-code (fn-fixed-prologue))
               ;; Generate body code with allocation
               (body-code (tac-codegen full-tac allocation))
               ;; Generate epilogue
               (epilogue-code (fn-fixed-epilogue))
               ;; Combine all code
               (all-code (append prologue-code body-code epilogue-code)))
          ;; Check for unresolved markers - error if present
          (if (has-unresolved-markers all-code)
              (error "codegen-main-reg-alloc: unresolved markers in generated code")
              all-code)))))

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
(defun disassemble-bytes (bytes &optional (stream t) (little-endian t))
  "Disassemble ARM64 bytes to readable instructions.
   BYTES is a list of bytes.
   LITTLE-ENDIAN: if T (default), bytes are in little-endian order (as from file dumps).
                  if NIL, bytes are in big-endian order (as emitted by codegen)."
  (format stream "~%ARM64 Disassembly (~D bytes):~%" (length bytes))
  (format stream "~4A  ~8A    ~A~%" "OFF" "HEX" "INSTRUCTION")
  (format stream "~4,,,'-A  ~8,,,'-A    ~,,,'-A~%" "" "" "")
  (let ((i 0))
    (loop while (< i (- (length bytes) 3)) do
      (let* ((b0 (nth i bytes))
             (b1 (nth (+ i 1) bytes))
             (b2 (nth (+ i 2) bytes))
             (b3 (nth (+ i 3) bytes))
             ;; ARM64 is little-endian
             (instr (if little-endian
                        ;; Little-endian: b0 is LSB
                        (logior b0 (ash b1 8) (ash b2 16) (ash b3 24))
                        ;; Big-endian: b0 is MSB (codegen output)
                        (logior (ash b0 24) (ash b1 16) (ash b2 8) b3))))
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
(defun reg-name (n &optional (use-sp t))
  "Format register name like lldb does.
   USE-SP: if nil, use xzr instead of sp for register 31.
   This is needed for arithmetic/logical instructions where Rn=31 means XZR."
  (case n
    (31 (if use-sp "sp" "xzr"))
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
      ;; When Rn=31, it's XZR not SP (NEG is alias for SUB with XZR)
      ((= (logand instr #xFF200000) #xCB000000)
       (if (= rn 31)
           (format nil "neg    ~A, ~A" (reg-name rd) (reg-name rm))
           (format nil "sub    ~A, ~A, ~A" (reg-name rd) (reg-name rn nil) (reg-name rm))))

      ;; MUL: 10011011000 Rm 011111 Rn Rd (MADD with Ra=XZR)
      ((= (logand instr #xFFE0FC00) #x9B007C00)
       (format nil "mul    ~A, ~A, ~A" (reg-name rd) (reg-name rn) (reg-name rm)))

      ;; SDIV: 10011010110 Rm 000011 Rn Rd
      ((= (logand instr #xFFE0FC00) #x9AC00C00)
       (format nil "sdiv   ~A, ~A, ~A" (reg-name rd) (reg-name rn) (reg-name rm)))

      ;; SUBS register: 11101011000 Rm 000000 Rn Rd
      ;; When Rn=31, it's XZR not SP (NEGS is alias for SUBS with XZR)
      ((= (logand instr #xFF200000) #xEB000000)
       (cond
         ((= rd 31)
          (format nil "cmp    ~A, ~A" (reg-name rn nil) (reg-name rm)))
         ((= rn 31)
          (format nil "negs   ~A, ~A" (reg-name rd) (reg-name rm)))
         (t
          (format nil "subs   ~A, ~A, ~A" (reg-name rd) (reg-name rn nil) (reg-name rm)))))

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
                      (format stream "Found unresolved markers - codegen will fail~%")
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
(defun show-codegen (fn &optional (stream t))
  "Show register-allocated codegen for a function.
   Useful for debugging."
  (format stream "~%========================================~%")
  (format stream "Codegen Output~%")
  (format stream "========================================~%")
  (format stream "~%Function: ~S~%" fn)

  ;; Generate with reg-alloc
  (format stream "~%--- Register Allocator ---~%")
  (let ((reg-code (codegen-fn-reg-alloc fn)))
    (if reg-code
        (progn
          (format stream "Generated ~D bytes~%" (length reg-code))
          (disassemble-bytes reg-code stream))
        (format stream "Codegen failed (returned nil)~%"))))

#+sbcl
(defun test-fn-execution (source &optional (stream t))
  "Compile a source string and test execution.
   Returns exit code or error info."
  (format stream "~%Testing: ~S~%" source)
  (let ((tmp-path "/tmp/habu_reg_alloc_test"))
    ;; Compile with reg-alloc
    (format stream "~%Compiling...~%")
    (deliver source tmp-path)
    ;; Run and capture exit code
    (let ((exit-code (nth-value 2 (uiop:run-program tmp-path :ignore-error-status t))))
      (format stream "Exit code: ~D~%" exit-code)
      (cond
        ((= exit-code 132) (format stream "  SIGILL - Illegal instruction~%"))
        ((= exit-code 139) (format stream "  SIGSEGV - Segmentation fault~%"))
        ((= exit-code 138) (format stream "  SIGBUS - Bus error~%"))
        ((= exit-code 137) (format stream "  SIGKILL - Killed (codesign?)~%")))
      exit-code)))
