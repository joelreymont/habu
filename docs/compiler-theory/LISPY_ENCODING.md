# Lispy Encoding Layer for Habu Compiler IR

**Source**: Adapted from external compiler specifications
**Date**: December 5, 2025
**Status**: Design reference for S-expression-based IR

## Purpose

This document specifies **S-expression encodings** for compiler intermediate representations, metadata structures, and analysis results. It provides a Lisp-native way to represent compiler IRs in Habu.

## Philosophy

Rather than using record types or structs, we represent all compiler data structures as S-expressions. This approach:

- Is natural for Lisp implementation
- Enables easy pretty-printing and debugging
- Allows pattern matching on IR forms
- Simplifies serialization and meta-circular compilation
- Makes the compiler itself more "Lispy"

## 1. Types (`Ty`)

Types form a lattice for dataflow analysis.

### Type Constructors

```scheme
;; Types:
(bottom)                  ; unreachable code, ⊥ in lattice
(fixnum)                  ; tagged integers
(double)                  ; boxed or unboxed floats
(bool)                    ; boolean values
(pair)                    ; cons cells
(vector elem-ty)          ; vectors with element type (or #f for unknown)
(closure)                 ; function closures
(top)                     ; unknown type, ⊤ in lattice
(union ty1 ty2 ...)       ; union types (normalized: no nested unions)
```

### Examples

```scheme
;; Simple types
(fixnum)
(double)
(pair)

;; Typed vector of doubles
(vector (double))

;; Union type (numeric)
(union (fixnum) (double))

;; Unknown element type
(vector #f)
```

### Helper Functions (optional)

```scheme
(define (ty-bottom) '(bottom))
(define (ty-fixnum) '(fixnum))
(define (ty-double) '(double))
(define (ty-bool) '(bool))
(define (ty-pair) '(pair))
(define (ty-vector elem) (list 'vector elem))
(define (ty-closure) '(closure))
(define (ty-top) '(top))
(define (ty-union . tys) (cons 'union tys))
```

## 2. Representations (`Rep`)

Physical representation of runtime values.

```scheme
;; Representations:
'tagged   ; Full Lisp value (tagged pointer/immediate)
'int64    ; Unboxed 64-bit integer
'double   ; Unboxed IEEE 754 double
```

**Habu current**: Only `'tagged` is used
**Future**: Add `'int64` and `'double` for unboxed arithmetic

## 3. High-Level IR (HIR)

HIR uses A-Normal Form (ANF) with Control Flow Graph (CFG) structure.

### 3.1. Expressions

```scheme
;; Expr:
(var v)                         ; variable reference
(lit value)                     ; literal constant
(prim op (v1 v2 ...))          ; primitive operation
(call fun (arg1 arg2 ...)      ; function call
      :tail? #t/#f)            ; optional tail-call marker
```

#### Examples

```scheme
(lit 42)                       ; literal integer
(var x)                        ; variable reference
(prim 'add (x y))             ; x + y (generic add)
(prim 'add-fixnum (x y))      ; specialized fixnum addition
(call f (x y) :tail? #f)      ; non-tail call
(call loop (i acc) :tail? #t) ; tail call
```

### 3.2. Terminators

Control flow terminators end basic blocks.

```scheme
;; Term:
(if cond then-bid else-bid)      ; conditional branch
(jump target-bid (args ...))     ; jump with optional args
(return value)                   ; return from function
```

#### Examples

```scheme
(if x 'then-block 'else-block)
(jump 'loop-head (i acc))
(return result)
```

### 3.3. Instructions

```scheme
;; Instr:
(let dst expr)      ; bind variable to expression result
(term term)         ; block terminator
```

### 3.4. Blocks, Functions, Programs

```scheme
;; Block:
(block block-id label
  (instrs ...))     ; sequence of (let ...) ending with (term ...)

;; Function:
(function func-id name (params ...) entry-block-id
  (blocks ...))

;; Program:
(program (functions ...))
```

#### Example Function

```scheme
(function 'add3 "add3" (a b c) 'entry
  ((block 'entry "entry"
     ((let tmp1 (prim 'add (b c)))
      (let result (prim 'add (a tmp1)))
      (term (return result))))))
```

## 4. TypeInfo

Type information from inference and refinement passes.

```scheme
;; TypeInfo:
(type-info var-types block-in-types)

;; var-types: list of ((func-id var-id) . type)
;; block-in-types: list of ((func-id block-id) . ((var-id . type) ...))
```

### Example

```scheme
(type-info
  ;; Variable types
  '(((f1 v1) . (fixnum))
    ((f1 v2) . (double))
    ((f1 v3) . (union (fixnum) (pair))))
  ;; Block entry types
  '(((f1 entry) . ((v1 . (fixnum))
                   (v2 . (top))))
    ((f1 loop) . ((i . (fixnum))
                  (acc . (fixnum))))))
```

## 5. RepInfo

Representation choices from unboxing analysis.

```scheme
;; RepInfo:
(rep-info var-reps)

;; var-reps: list of ((func-id var-id) . rep)
```

### Example

```scheme
(rep-info
  '(((f1 v1) . int64)      ; unboxed integer
    ((f1 v2) . double)      ; unboxed float
    ((f1 v3) . tagged)))    ; tagged value
```

## 6. Machine IR (MIR)

Low-level IR with virtual registers.

### 6.1. Virtual Registers and Operands

```scheme
;; VReg: integers or symbols (e.g., 0, 1, 2 or v0, v1, v2)

;; MOperand:
(reg vreg)          ; virtual register
(imm n)             ; integer immediate
(label block-id)    ; jump target label
```

### 6.2. Operations

```scheme
;; MOp: Symbols representing operations
;;
;; Integer:
;;   'add 'sub 'mul 'cmp 'mov 'asr 'lsl 'and 'orr
;;
;; Floating-point:
;;   'fadd 'fsub 'fmul
;;
;; Memory:
;;   'ldr 'str
;;
;; Control:
;;   'b                       ; unconditional branch
;;   (bcond cond-code)        ; conditional (cond: 'eq 'ne 'lt 'le 'gt 'ge)
;;   'call                    ; function call
;;   'tail-call               ; tail call
;;   'ret                     ; return
;;
;; Runtime:
;;   'gc-alloc                ; allocate from GC heap
```

### 6.3. Instructions

```scheme
;; MInstr:
(minstr instr-id op (dsts ...) (srcs ...) safe-point-id-or-#f)

;; instr-id: unique identifier (integer or symbol)
;; op: operation (MOp)
;; dsts: list of vregs written
;; srcs: list of MOperands
;; safe-point-id: #f or safe-point identifier (for GC)
```

#### Examples

```scheme
;; v0 = 42 (immediate move)
(minstr 0 'mov (v0) ((imm 672)) #f)  ; 672 = 42 << 4 (tagged)

;; v2 = v0 + v1
(minstr 1 'add (v2) ((reg v0) (reg v1)) #f)

;; Store to memory
(minstr 2 'str () ((reg v0) (reg v1)) #f)

;; Call with safe point
(minstr 3 'call (v0) ((label 'allocate-cons) (reg v1) (reg v2)) 'sp-0)

;; Conditional branch
(minstr 4 (bcond 'eq) () ((reg v0) (label 'then-block)) #f)
```

### 6.4. Blocks, Functions, Program

```scheme
;; MBlock:
(mblock block-id label (instrs ...))

;; MFunction:
(mfunction func-id name entry-block-id
  reg-class-table      ; ((vreg . 'gpr/'fpr) ...)
  (blocks ...))

;; MProgram:
(mprogram (functions ...))
```

#### Example Register Class Table

```scheme
;; Specifies whether each vreg is general-purpose or floating-point
'((v0 . gpr)
  (v1 . fpr)
  (v2 . gpr))
```

## 7. GC Metadata

### 7.1. Safe Points

Points where GC can safely interrupt execution.

```scheme
;; SafePoint:
(safe-point id func-id block-id instr-index)

;; id: unique safe-point ID
;; func-id: function containing this safe point
;; block-id: basic block
;; instr-index: index within block (0-based)
```

### 7.2. Root Locations and Stack Maps

Describes where GC roots are located at each safe point.

```scheme
;; PhysReg:
(x n)                  ; ARM64 general-purpose register Xn
(v n)                  ; ARM64 floating-point/vector register Vn

;; RootLocation:
(reg phys-reg)         ; root in register
(stack-slot offset)    ; root in stack slot (offset from FP)

;; StackMapEntry:
(stack-map-entry safe-point-id (roots ...))

;; GcMetadata:
(gc-metadata (safe-points ...) (stack-maps ...))
```

#### Example

```scheme
(gc-metadata
  ;; Safe points
  ((safe-point 'sp-0 'foo 'block2 5)
   (safe-point 'sp-1 'foo 'block3 2))
  ;; Stack maps (which registers/slots contain GC roots)
  ((stack-map-entry 'sp-0 ((reg (x 0)) (stack-slot -16)))
   (stack-map-entry 'sp-1 ((reg (x 1)) (reg (x 2))))))
```

## 8. Register Allocation Results

### 8.1. Live Intervals (diagnostic)

```scheme
;; LiveInterval:
(live-interval vreg reg-class start end assigned-reg spill-slot)

;; reg-class: 'gpr or 'fpr
;; start, end: instruction positions defining live range
;; assigned-reg: #f or PhysReg
;; spill-slot: #f or stack offset
```

### 8.2. Allocation Mapping

```scheme
;; RaResult:
(ra-result vreg->reg vreg->spill)

;; vreg->reg: (((func-id vreg) . phys-reg-or-#f) ...)
;; vreg->spill: (((func-id vreg) . offset-or-#f) ...)
```

#### Example

```scheme
(ra-result
  ;; Register assignments
  '(((f1 v0) . (x 9))        ; v0 allocated to x9
    ((f1 v1) . (v 0))        ; v1 allocated to v0 (FP reg)
    ((f1 v2) . #f))          ; v2 spilled
  ;; Spill slots
  '(((f1 v2) . -24)))        ; v2 at [FP-24]
```

## 9. Pass Function Signatures

Compiler passes as Lisp functions:

```scheme
;; T1: Type inference
;;   Program -> TypeInfo
(defun t1-type-inference (program)
  ...)

;; T2: Branch refinement
;;   Program TypeInfo -> TypeInfo
(defun t2-branch-refinement (program type-info)
  ...)

;; T3: Primitive specialization
;;   Program TypeInfo -> Program
(defun t3-specialize-prims (program type-info)
  ...)

;; U1: Choose representation
;;   Program TypeInfo -> RepInfo
(defun u1-choose-representation (program type-info)
  ...)

;; U2: Insert box/unbox
;;   Program TypeInfo RepInfo -> Program
(defun u2-insert-box-unbox (program type-info rep-info)
  ...)

;; TCO: Mark tail calls
;;   Program -> Program
(defun tco-mark-tail-calls (program)
  ...)

;; Lower to MIR
;;   Program TypeInfo RepInfo -> MProgram
(defun lower-to-mir (program type-info rep-info)
  ...)

;; GC: Insert safe points
;;   MProgram -> GcMetadata
(defun gc-insert-safe-points (mprogram)
  ...)

;; GC: Compute liveness
;;   MProgram -> LiveInfo
(defun gc-compute-liveness (mprogram)
  ...)

;; GC: Build stack maps
;;   MProgram RepInfo LiveInfo RaResult -> GcMetadata
(defun gc-build-stack-maps (mprogram rep-info live-info ra-result)
  ...)

;; RA: Linear scan register allocation
;;   MProgram RepInfo -> (values MProgram RaResult)
(defun ra-linear-scan (mprogram rep-info)
  ...)

;; Codegen: ARM64 code generation
;;   MProgram GcMetadata -> Assembly
(defun codegen-arm64 (mprogram gc-metadata)
  ...)
```

## 10. Example: Complete Transformation

```scheme
;; Source:
(defun double-add (x y)
  (+ (* x 2) y))

;; HIR (after parsing):
(function 'double-add "double-add" (x y) 'entry
  ((block 'entry "entry"
     ((let two (lit 2))
      (let x2 (prim 'mul (x two)))
      (let result (prim 'add (x2 y)))
      (term (return result))))))

;; After type inference (TypeInfo):
(type-info
  '(((double-add x) . (fixnum))
    ((double-add y) . (fixnum))
    ((double-add two) . (fixnum))
    ((double-add x2) . (fixnum))
    ((double-add result) . (fixnum)))
  ...)

;; After primitive specialization:
(function 'double-add "double-add" (x y) 'entry
  ((block 'entry "entry"
     ((let two (lit 2))
      (let x2 (prim 'mul-fixnum (x two)))     ; specialized!
      (let result (prim 'add-fixnum (x2 y)))  ; specialized!
      (term (return result))))))

;; After representation choice (RepInfo):
(rep-info
  '(((double-add x) . int64)       ; unboxed!
    ((double-add y) . int64)
    ((double-add two) . int64)
    ((double-add x2) . int64)
    ((double-add result) . int64)))

;; MIR (after lowering):
(mfunction 'double-add "double-add" 'entry
  '((v0 . gpr) (v1 . gpr) (v2 . gpr) (v3 . gpr) (v4 . gpr))
  ((mblock 'entry "entry"
     ((minstr 0 'mov (v0) ((reg 'param0)) #f)    ; x (already unboxed)
      (minstr 1 'mov (v1) ((reg 'param1)) #f)    ; y
      (minstr 2 'mov (v2) ((imm 2)) #f)          ; two (unboxed)
      (minstr 3 'mul (v3) ((reg v0) (reg v2)) #f) ; x * 2 (unboxed mul)
      (minstr 4 'add (v4) ((reg v3) (reg v1)) #f) ; + y (unboxed add)
      (minstr 5 'lsl (v4) ((reg v4) (imm 4)) #f)  ; box result
      (minstr 6 'ret () ((reg v4)) #f)))))

;; After register allocation (RaResult):
(ra-result
  '(((double-add v0) . (x 0))      ; param in x0
    ((double-add v1) . (x 1))      ; param in x1
    ((double-add v2) . (x 9))      ; const in x9
    ((double-add v3) . (x 10))     ; temp in x10
    ((double-add v4) . (x 0)))     ; result in x0 (reuse)
  '())  ; no spills!

;; ARM64 assembly:
;;   mov x9, #2
;;   mul x10, x0, x9
;;   add x0, x10, x1
;;   lsl x0, x0, #4      ; box for tagged return
;;   ret
```

## Integration with Habu

### Current Habu IR

Habu currently uses tree-structured IR:

```scheme
;; Habu current
'(defun foo (x) (+ x 1))
=> (function-def foo (x) (add (var x) (lit 1)))
```

### Migration Path

1. **Keep current tree IR for frontend** (parsing, macro expansion)
2. **Add ANF conversion pass**: `tree-ir → anf-ir`
3. **Build CFG**: `anf-ir → cfg-ir`
4. **Run optimization passes**: type inference, specialization, etc.
5. **Lower to MIR**: `cfg-ir → mir`
6. **Register allocation**: `mir → allocated-mir`
7. **Code generation**: `allocated-mir → arm64`

### Advantages

- **Separation of concerns**: Frontend vs. optimization vs. backend
- **Better optimization**: CFG enables dataflow analysis
- **Easier debugging**: Each pass has well-defined input/output
- **Meta-circular**: IR is just S-expressions, easy to manipulate in Lisp

## See Also

- `/Users/joel/Work/habu/docs/compiler-theory/NANOPASS_COMPILER_SPEC.md` - Overall architecture
- `/Users/joel/Work/habu/bootstrap/compiler.lisp` - Current compiler implementation
- `/Users/joel/Work/habu/bootstrap/codegen.lisp` - Current code generator

## References

This encoding layer is adapted from nanopass compiler frameworks used in:
- Chez Scheme
- Racket
- Modern research compilers

The S-expression representation makes the compiler implementation natural in Lisp and enables powerful meta-programming capabilities.
