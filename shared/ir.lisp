;;;; IR - Intermediate Representation ADT
;;;;
;;;; Proper sum type for IR nodes with exhaustiveness checking.
;;;; Replaces the numeric ir-tag-* functions with type-safe constructors.
;;;;
;;;; Uses :habu package directly so ir-* functions are available
;;;; without package prefixes in habu0.lisp (build concatenates files).

(in-package :habu)

;;; IR ADT - all IR node types with :prefix ir
;;; Match patterns use short names: (match ir node (lit (v) ...) (add (l r) ...) ...)

(habu.types:deftype ir :prefix ir
  ;; === Literals and Variables ===
  (lit value)                    ; fixnum literal
  (var offset)                   ; variable reference by env offset
  (nil)                          ; nil literal (no fields)
  (t)                            ; t literal (no fields)
  (str-lit value)                ; string literal
  (kw-lit value)                 ; keyword literal
  (quote-sym value)              ; quoted symbol

  ;; === Arithmetic ===
  (add left right)
  (sub left right)
  (mul left right)
  (div left right)
  (mod left right)
  (neg value)                    ; unary negation

  ;; === Comparison ===
  (cmp-eq left right)            ; numeric ==
  (cmp-lt left right)            ; numeric <
  (cmp-gt left right)            ; numeric >
  (cmp-le left right)            ; numeric <=
  (cmp-ge left right)            ; numeric >=
  (eq left right)                ; pointer eq
  (eql left right)               ; eql
  (sym-eq left right)            ; symbol name equality

  ;; === Control Flow ===
  (if test then else)
  (let offset value body)        ; let binding
  (progn forms)                  ; sequence (forms is a list)
  (setq offset value)            ; variable assignment

  ;; === List Operations ===
  (cons car cdr)
  (car cell)
  (cdr cell)
  (null value)                   ; null predicate
  (length list)                  ; list length

  ;; === Type Predicates ===
  (consp value)
  (symbolp value)
  (numberp value)
  (stringp value)
  (keywordp value)

  ;; === Bitwise/Logical ===
  (logand left right)
  (logior left right)
  (lognot value)
  (ash value count)
  (not value)                    ; boolean not

  ;; === String Operations ===
  (str-len str)
  (str-ref str index)
  (string-eq left right)
  (symbol-name sym)
  (keyword-name kw)
  (make-string-from-vector vec)
  (make-symbol-from-string str)

  ;; === Vector Operations ===
  (make-vector size init)
  (vector-ref vec index)
  (vector-set vec index value)
  (vector-length vec)

  ;; === Tag Operations ===
  (get-tag value)
  (set-tag value tag)

  ;; === Functions ===
  (lambda params body free-vars free-offsets)
  (lambda-ref name free-offsets) ; reference to lifted lambda
  (funcall fn args)
  (call name args)               ; named function call

  ;; === Multiple Values ===
  (values forms)                 ; (values x y z) - forms is list of IR

  ;; === Control Flow (extended) ===
  (loop body)                    ; infinite loop
  (block name body)              ; named block for return-from
  (return-from name value)       ; non-local return
  (continue)                     ; continue loop
  (dolist var list body)         ; dolist iteration
  (dotimes var count body)       ; dotimes iteration

  ;; === Type Predicates (extended) ===
  (vectorp value)                ; vector type predicate

  ;; === Error ===
  (error message))

;;; Lambda Pipeline ADT
;;;
;;; Lift-lambdas extracts lambda-ir nodes and returns lambda-entry records.
;;; lambdas-to-defuns converts lambda-entry to defun-fn.
;;; This ADT makes the conversion explicit and type-safe.

(habu.types:deftype lambda-entry :prefix le
  "Lifted lambda before conversion to defun.
   Created by lift-lambdas, consumed by lambdas-to-defuns."
  (entry name params body free-vars free-offsets))

(habu.types:deftype defun-fn :prefix df
  "Function definition ready for codegen.
   param-base is the environment offset where params start
   (after captured vars for closures, 0 for regular functions)."
  (fn name params body param-base))

;;; Frame Layout ADT
;;;
;;; Single source of truth for stack frame layout.
;;; All frame-related code generation MUST use this type.
;;; This prevents offset mismatches between prologue/epilogue/spills/env.

(habu.types:deftype frame-layout :prefix fl
  "Stack frame layout computed once, used everywhere.
   All offsets are from SP after prologue executes.

   Layout (grows down):
     sp+0:           callee-saved registers (x19-x26)
     sp+callee-size: spill slots
     sp+env-base:    environment slots (x20 points here)
     sp+fp-offset:   saved frame pointer
     sp+lr-offset:   saved link register
     sp+frame-size:  (original sp before prologue)"
  (layout
    frame-size      ; total frame size in bytes (16-byte aligned)
    fp-offset       ; offset to saved fp from sp
    lr-offset       ; offset to saved lr from sp
    callee-base     ; offset where callee-saved regs start
    callee-size     ; bytes used by callee-saved regs
    spill-base      ; offset where spill slots start
    spill-count     ; number of 8-byte spill slots
    env-base        ; offset where env starts (x20 = sp + env-base)
    env-slots))

;;; Frame offset conversion functions
;;; These depend on both frame-offset ADT (types.lisp) and frame-layout ADT (above)

;; Callee-save offset helper (duplicated from types.lisp for habu0 compatibility)
(defun callee-save-offset-internal (reg)
  "Return the sp-relative offset for a callee-saved register."
  (cond
    ((eq reg :x19) 16)
    ((eq reg :x20) 24)
    ((eq reg :env) 24)
    ((eq reg :x21) 32)
    ((eq reg :x22) 40)
    ((eq reg :x23) 48)
    ((eq reg :x24) 56)
    ((eq reg :closure) 56)
    ((eq reg :x26) 64)
    ((eq reg :code-base) 64)
    (t (error "callee-save-offset-internal: unknown register ~S" reg))))

(defun frame-offset-to-bytes (foff layout)
  "Convert typed frame-offset to actual byte offset from sp.
   LAYOUT must be a frame-layout instance.
   FOFF is a frame-offset ADT: (:FRAME-OFFSET :variant ...)"
  ;; Use cond for habu0 compatibility (no ecase)
  (let ((variant (cadr foff)))
    (cond
      ((eq variant :env)
       ;; env uses NEGATIVE offset from env-base
       (- (fl-layout-env-base layout) (* (nth 2 foff) 8)))
      ((eq variant :spill)
       ;; spill uses POSITIVE offset from spill-base
       (+ (fl-layout-spill-base layout) (* (nth 2 foff) 8)))
      ((eq variant :callee)
       ;; callee-save at fixed locations
       (callee-save-offset-internal (nth 2 foff)))
      ((eq variant :temp)
       ;; temp at fixed location (shares with spill area)
       (+ (fl-layout-spill-base layout) (* (nth 2 foff) 8)))
      ((eq variant :fp-save)
       (fl-layout-fp-offset layout))
      ((eq variant :lr-save)
       (fl-layout-lr-offset layout))
      (t (error "frame-offset-to-bytes: unknown variant ~S" variant)))))

(defun frame-offsets-may-collide-p (foff1 foff2 layout)
  "Check if two frame offsets resolve to the same memory location.
   This is a BUG if it happens - use this in debug assertions."
  (= (frame-offset-to-bytes foff1 layout)
     (frame-offset-to-bytes foff2 layout)))

(defun validate-frame-layout (layout)
  "Validate that a frame layout has no overlapping regions.
   Call this after make-frame-layout to catch bugs early."
  (let* ((callee-end 72)  ; callee-saved regs end at offset 72 (9 slots)
         (spill-base (fl-layout-spill-base layout))
         (spill-end (+ spill-base (* (fl-layout-spill-count layout) 8)))
         (env-base (fl-layout-env-base layout))
         (env-slots (fl-layout-env-slots layout))
         (env-lowest (- env-base (* env-slots 8))))  ; env uses negative offsets!
    ;; Check: callee-save < spill < env-lowest < env-base < fp < lr
    ;; Use if/error for habu0 compatibility (no assert)
    (if (> callee-end spill-base)
        (error "Frame layout bug: callee-save overlaps spill-base"))
    (if (> spill-end env-lowest)
        (error "Frame layout bug: spill area overlaps env area"))
    (if (>= env-base (fl-layout-fp-offset layout))
        (error "Frame layout bug: env-base >= fp-offset"))
    layout))
