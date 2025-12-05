# Habu Nanopass Compiler Specification

**Source**: Adapted from external Lisp compiler design specifications
**Date**: December 5, 2025
**Status**: Design reference for future Habu improvements

## Purpose

This document describes a high-performance nanopass compiler architecture for targeting ARM64, originally designed for a Scheme-like Lisp. It serves as a reference for potential improvements to Habu's compilation pipeline.

## Overview

This spec outlines a compiler structured as a sequence of small ("nano") passes, each performing a focused transformation. The goal is C-like performance on numeric and control-flow-heavy code.

## Assumptions

- Lexically scoped Lisp with closures (matches Habu's Common Lisp subset)
- Tagged values with precise, moving GC
- Generational runtime with nursery and safe points
- ARM64 target architecture (matches Habu's current focus)

## 1. Runtime and Representation

### Tagged Pointer Format

**Current Habu**: 4-bit tags (value << 4)
- Fixnum: `xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx0001`
- Cons/Heap: `xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx0000`

**Proposed (from spec)**: 3-bit tags for better space efficiency
- `...000` → heap pointer (GC object, 8-byte aligned)
- `...001` → fixnum
- `...010` → reserved
- `...011` → reserved

### Fixnum Operations

**Spec approach**: `raw = (n << 3) | 1` and `n = raw >> 3` (arithmetic shift)
**Habu current**: `raw = (n << 4)` and `n = raw >> 4`

**Benefit**: 3-bit tags allow one more bit for fixnum range, reducing boxing overhead.

### Heap Objects

- Closures
- Pairs (cons cells)
- Vectors
- Boxed doubles

### GC Safe Points

Safe points required at:
- Function calls
- Heap allocation
- Loop back edges (for long-running loops)

**Habu status**: Basic GC safe points at allocation sites; loop back edges not yet implemented.

### Calling Convention

- External C calls: Use AAPCS64 standard
- Internal Lisp calls: Use dedicated environment register (e.g., x19 or x20)

**Habu current**: x20 for environment base, x24 for closure environment

## 2. High-Level IR (HIR)

### IR Structure

**ANF (A-Normal Form) + CFG (Control Flow Graph)** per function:

```scheme
;; Expressions
(var v)                              ; variable reference
(lit value)                          ; literal constant
(prim op (v1 v2 ...))               ; primitive operation
(call fun (arg1 arg2 ...) :tail? #t) ; function call

;; Terminators
(if cond then-block else-block)
(jump target-block (args ...))
(return value)

;; Instructions
(let dst expr)   ; bind result to variable
(term term)      ; block terminator
```

**Habu current**: Tree-based IR without explicit CFG
**Opportunity**: Adopting ANF+CFG would enable better dataflow analysis

### Program Structure

```scheme
(program
  (function func-id name (params ...)
    entry-block-id
    (blocks ...)))
```

## 3. Type System and Inference

### Type Lattice

```scheme
;; Types
(bottom)                  ; unreachable code
(fixnum)
(double)
(bool)
(pair)
(vector elem-ty)         ; typed vectors
(closure)
(top)                    ; unknown type
(union ty1 ty2 ...)     ; union types
```

### Lattice Operations

- `join`: Compute least upper bound (for merging control flow)
- `meet`: Compute greatest lower bound (for refinement)
- `remove`: Subtract types (for negative type tests)

### Type Inference Passes

**T1 - Type Inference**:
- Forward dataflow over CFG
- Infer types for each variable based on primitive typing rules
- Handles phi-functions at merge points

**T2 - Branch Refinement**:
- Recognizes type predicates: `(if (fixnum? x) ...)`
- Refines `x` to `fixnum` on then-branch
- Enables unboxing in specialized branches

**T3 - Primitive Specialization**:
- Rewrites generic primitives to specialized variants
- `Add` → `AddFixnum` or `AddDouble` based on operand types
- `VectorRef` → `VectorRefDouble` for typed vectors

**Habu current**: No type inference system
**Major opportunity**: Type inference would enable unboxing and eliminate runtime type checks

## 4. Representation and Unboxing

### Representation Classes

```scheme
;; Rep
'tagged    ; tagged value (pointer or immediate)
'int64     ; unboxed 64-bit integer
'double    ; unboxed IEEE double
```

### Unboxing Passes

**U1 - Choose Representation**:
- Start from inferred types
- `fixnum` → `int64` (unboxed)
- `double` → `double` (unboxed)
- others → `tagged`
- Force `tagged` for values used in generic contexts (closures, containers, unknown calls)

**U2 - Insert Box/Unbox**:
- Insert explicit boxing/unboxing operations
- Lower specialized primitives to representation-aware ops
- `AddFixnum` → `AddInt64` with unboxing/boxing
- `VectorRefDouble` → `VectorRefDoubleRaw` with raw access

**Habu current**: All values are tagged; no unboxing
**Major opportunity**: Unboxed arithmetic would eliminate most tagging overhead

## 5. Tail-Call Optimization (TCO)

### TCO Strategy

**Mark tail positions**:
- Calls whose results are directly returned
- Can reuse stack frame

**Lower to MIR**:
- Self tail calls → jump that reuses frame
- Mutual tail calls → tail-call convention

**Habu current**: Has basic TCO support in codegen
**Opportunity**: Formalize tail position detection in HIR

## 6. Machine IR (MIR)

### MIR Structure

**Virtual registers** with register classes:
- `gpr` - General purpose registers
- `fpr` - Floating point registers

**Operands**:
```scheme
(reg vreg)           ; virtual register
(imm n)              ; immediate constant
(label block-id)     ; jump target
```

**Operations**:
- Integer ops: `add`, `sub`, `mul`, `cmp`, `mov`, `asr`, `lsl`, `and`, `orr`
- Float ops: `fadd`, `fsub`, `fmul`
- Memory: `ldr`, `str`
- Control: `b`, `bcond`, `call`, `tail-call`, `ret`
- Runtime: `gc-alloc`

**Habu current**: Direct tree-to-ARM64 code generation
**Opportunity**: Explicit MIR would enable better optimization and register allocation

## 7. GC Safe Points and Stack Maps

### Safe Point Tracking

```scheme
;; SafePoint
(safe-point id func-id block-id instr-index)
```

Safe points at:
- All `call`, `tail-call`, and `gc-alloc` instructions
- Loop back edges (for GC to interrupt long-running loops)

### Stack Maps

For each safe point, record which registers/stack slots contain GC roots:

```scheme
;; RootLocation
(reg phys-reg)         ; e.g., (x 0)
(stack-slot offset)    ; e.g., FP-24

;; StackMapEntry
(stack-map-entry safe-point-id (roots ...))
```

GC uses tag bits to identify pointers vs. immediate values.

**Habu current**: Basic GC support, no precise stack maps
**Opportunity**: Precise stack maps would enable:
- More aggressive GC
- Better memory locality
- Verification of root correctness

## 8. Register Allocation

### Linear Scan Algorithm

**Per register class** (GPR/FPR separately):

1. **Build live intervals**: For each vreg, compute [start, end) range
2. **Sort by start position**
3. **Allocate greedily**:
   - Try to assign physical register
   - Spill to stack if no register free
4. **Record allocation**: `vreg → phys-reg or spill-slot`

### Physical Register Usage

**ARM64 registers**:
- `x0-x7`: Arguments, return value (caller-saved)
- `x9-x15`: Allocatable (caller-saved) - 7 registers
- `x19-x22`: Callee-saved, can use for values spanning calls - 4 registers
- Reserved:
  - `x20`: Habu environment base (but could use x19 and free x20)
  - `x24`: Habu closure environment
  - `x26`: Code base
  - `x27`: Heap base
  - `x28`: Heap bump pointer
  - `x29`: Frame pointer
  - `x30`: Link register

**Habu current**: Accumulator model (all results to x0, heavy spilling)
**Major opportunity**: Linear scan would reduce spilling by 10-100x

## 9. ARM64 Code Generation

### Instruction Selection

Map MIR operations to ARM64 instructions:

```
UnboxFixnum → asr by 3 (or 4 for current Habu)
BoxFixnum → lsl by 3, orr with 1
VectorRefDoubleRaw → pointer arithmetic + ldr to FP reg
AddInt64 → add (native 64-bit addition)
```

### Prologue/Epilogue

```asm
function_entry:
    stp x29, x30, [sp, #-16]!  ; Save FP, LR
    mov x29, sp                 ; Set up frame pointer
    sub sp, sp, #frame_size     ; Allocate frame
    ; ... function body ...
    add sp, sp, #frame_size     ; Deallocate frame
    ldp x29, x30, [sp], #16     ; Restore FP, LR
    ret
```

### Calls

**Regular calls**:
- Place arguments in x0-x7 (or spill to stack for >8 args)
- `bl` to target
- Result in x0

**Tail calls**:
- Move arguments to parameter positions
- Restore caller's frame
- `br` to target (or `ret` with target in x30)

**Habu current**: Standard call convention working
**Opportunity**: Optimize argument passing for internal calls

## 10. Pass Pipeline

Proposed full pipeline:

1. `t1-type-inference` - Infer types via dataflow
2. `t2-branch-refinement` - Refine types on branches
3. `t3-specialize-prims` - Specialize primitives by type
4. `u1-choose-representation` - Select tagged/unboxed reps
5. `u2-insert-box-unbox` - Insert boxing conversions
6. `tco-mark-tail-calls` - Mark tail positions
7. `lower-to-mir` - Convert HIR to MIR with vregs
8. `gc-insert-safe-points` - Tag GC safe points
9. `gc-compute-liveness` - Backward liveness analysis
10. `ra-linear-scan` - Allocate physical registers
11. `gc-build-stack-maps` - Generate precise root maps
12. `codegen-arm64` - Emit native code

**Habu current pipeline**:
1. Macro expansion
2. Compiler (tree transform)
3. Codegen (direct to ARM64)

**Opportunity**: Restructure as nanopass pipeline for better modularity and optimization

## Integration Recommendations

### Phase 1: Foundation (High Priority)

1. **Add basic type inference**
   - Track fixnum/double/tagged types
   - Specialize arithmetic operations
   - Quick win: 2-5x speedup on numeric code

2. **Implement MIR with virtual registers**
   - Replaces current tree-based codegen
   - Enables register allocation
   - Foundation for all other improvements

3. **Linear scan register allocation**
   - Replace accumulator model
   - Reduces spilling by 10-100x
   - Major performance win: 5-20x speedup

### Phase 2: Optimizations (Medium Priority)

4. **Branch refinement**
   - Optimize type checks
   - Enable conditional unboxing

5. **Unboxing pass**
   - Eliminate boxing in numeric loops
   - Potential 5-10x speedup on numeric code

6. **Precise GC stack maps**
   - Enable better GC
   - Reduce memory usage
   - Improve pause times

### Phase 3: Advanced (Lower Priority)

7. **Loop optimizations** (not in this spec, but natural extension)
   - Loop invariant code motion
   - Strength reduction
   - Unrolling

8. **Inlining** (not in this spec)
   - Inline small functions
   - Eliminate call overhead

9. **Escape analysis** (not in this spec)
   - Stack-allocate non-escaping objects
   - Reduce GC pressure

## Comparison: Spec vs. Habu Current

| Feature | Spec | Habu Current | Priority |
|---------|------|--------------|----------|
| Tagged format | 3-bit tags | 4-bit tags | Low |
| IR structure | ANF+CFG | Tree | High |
| Type inference | Full lattice | None | High |
| Unboxing | Yes | No | High |
| Register allocation | Linear scan | Accumulator | Critical |
| GC stack maps | Precise | Basic | Medium |
| TCO | Formalized | Basic | Medium |
| Calling convention | Optimized | Standard | Low |

## Performance Estimates

Based on typical nanopass compiler improvements:

- **Type inference + specialization**: 2-5x on numeric code
- **Linear scan RA**: 5-20x overall (eliminates most spilling)
- **Unboxing**: 5-10x on numeric loops (eliminates tagging overhead)
- **Combined**: 50-200x potential speedup on numeric/loop-heavy code

These gains bring Habu from "interpreter speed" to "C-like speed" for compiled code.

## References

This specification is adapted from external compiler design research for high-performance Lisp compilers. It represents best practices from:
- Chez Scheme's nanopass framework
- Modern optimizing compilers (LLVM, GCC)
- Typed Racket's gradual typing system
- Production Lisp systems (SBCL, CCL)

## See Also

- `/Users/joel/Work/habu/docs/compiler-theory/LISPY_ENCODING.md` - S-expression encoding for IR
- `/Users/joel/Work/habu/docs/codegen/COMPILATION_ARCHITECTURE.md` - Current Habu architecture
- `/Users/joel/Work/habu/bootstrap/reg-alloc.lisp` - Current RA implementation (in progress)
