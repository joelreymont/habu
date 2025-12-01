# Habu Compiler Master Roadmap

**Last Updated**: December 1, 2025

## Overview

This document tracks the implementation roadmap for the Habu self-hosting Lisp compiler. Items are prioritized by their impact on achieving self-hosting and production quality.

## Completed

- [x] Core compiler (expressions, control flow, bindings, functions)
- [x] Closures with captured variables
- [x] Native ARM64 code generation
- [x] Mach-O executable generation
- [x] Stage 1 compiler compiles and runs
- [x] `while` loop construct for true iteration
- [x] Mach-O zerofill sections (67KB vs 8.7MB)

## In Progress

### Stage 1 → Stage 2 Generation
**Priority**: P1 (Blocker for fixed-point verification)

Stage 1 compiler runs natively but needs to generate Stage 2 binary.
Current blocker: crash during compilation of larger programs.

## Completed Recently

### Stack Overflow Fix - DONE
- Rewrote `read-list-elems` to use iterative `while` loop
- Added TCO (Tail Call Optimization) for recursive functions

### Tail Call Optimization (TCO) - DONE
**Implemented as nanopass architecture**:

**Pass 1: apply-tco-to-function** (optimize.lisp)
- Identifies self-tail-calls in function body
- Transforms to `loop-ir` / `continue-ir` nodes
- Applied via `apply-tco-to-all-functions` in compile-forms

**Pass 2: codegen** (codegen.lisp)
- Handles `loop-ir` and `continue-ir` nodes
- Emits `:tco-branch` markers during code generation
- `resolve-tco-branches` converts markers to backward B instructions

Files modified:
- `optimize.lisp`: TCO transformation pass
- `compiler.lisp`: Integrated TCO into compilation pipeline
- `codegen.lisp`: loop-ir/continue-ir handling, resolve-tco-branches

Tested: 100,000 recursive calls without stack overflow

## Planned Features

### 2. DWARF5 Debug Information
**Priority**: P3

Generate debug info for lldb:
- `.debug_line` section: PC to source line mapping
- `.debug_info` section: function names, types
- `.debug_abbrev` section: abbreviation tables

Benefits:
- `bt` shows function names instead of addresses
- `list` shows source code at crash location
- Step-through debugging

Files to modify:
- `macho.lisp`: Add DWARF sections to Mach-O
- `codegen.lisp`: Track source locations during codegen

### 3. Register Allocator
**Priority**: P4
**Status**: Architecture implemented, codegen integration pending

Current codegen uses an "accumulator model" - all results go to x0, spill to stack.
New register model uses virtual registers with linear scan allocation.

**Nanopass Architecture** (implemented in `regalloc.lisp`):

**Pass 1: ir-to-tac** (IR → Three-Address Code)
- Converts tree IR to linear TAC with virtual registers
- Input: `(add (var 0) (mul (lit 2) (var 1)))`
- Output: `((tac-var v0 0) (tac-lit v1 2) (tac-var v2 1) (tac-binop v3 mul v1 v2) (tac-binop v4 add v0 v3))`

**Pass 2: compute-liveness** (TAC → TAC + Liveness Info)
- Backward dataflow analysis
- Computes live-in/live-out sets per instruction
- Formula: `live-in[i] = use[i] ∪ (live-out[i] - def[i])`

**Pass 3: compute-intervals** (Liveness → Live Intervals)
- Converts liveness info to `(vreg start-pos end-pos)` tuples
- Sorted by start position for linear scan

**Pass 4: linear-scan** (Intervals → Allocation)
- Allocates physical registers (x9-x15 for temporaries)
- Spills to stack when registers exhausted
- Returns map: `vreg → physical-reg | (:spill slot)`

**Pass 5: tac-codegen** (TAC + Allocation → ARM64)
- Generates ARM64 from TAC with register assignments
- STATUS: Placeholder - needs implementation

**TAC Instruction Format**:
```lisp
(tac-lit vreg value)           ; vreg = literal
(tac-var vreg offset)          ; vreg = env[offset]
(tac-setvar offset vreg)       ; env[offset] = vreg
(tac-binop vreg op vr1 vr2)    ; vreg = vr1 op vr2
(tac-call vreg fn args)        ; vreg = fn(args...)
(tac-if vreg then else)        ; conditional branch
(tac-return vreg)              ; return value
```

**Register Usage**:
- x9-x15: 7 allocatable temporaries (caller-saved)
- x19, x21, x22: Values spanning calls (callee-saved, x20 reserved)
- x20: Environment frame base (reserved)
- x24: Closure environment (reserved)
- x26-x28: Code base, heap base, bump pointer (reserved)

Files:
- `reg-alloc.lisp`: Full nanopass implementation (580 lines)

### 4. Heap Allocator with mmap
**Priority**: P5

Replace fixed 64MB heap with dynamic allocation:

```lisp
;; Current: fixed heap at load time
(defconstant +HEAP-SIZE+ #x4000000)

;; New: mmap on demand
(defun alloc-heap-page ()
  (sys-mmap 0 #x4000 PROT_READ|PROT_WRITE MAP_PRIVATE|MAP_ANON -1 0))
```

Implementation:
1. Add `sys-mmap` syscall wrapper
2. Initial small heap (1MB)
3. Grow on demand when bump pointer exhausts page
4. Track allocated pages for future GC

### 5. Common Lisp `loop` Macro
**Priority**: P6

Implement full CL `loop` specification:

```lisp
(loop for i from 1 to 10
      sum i)

(loop for x in list
      when (evenp x)
      collect (* x 2))
```

Features:
- Iteration: `for`, `repeat`, `while`, `until`
- Accumulation: `collect`, `append`, `sum`, `count`
- Conditionals: `when`, `unless`, `if`
- Control: `return`, `finally`

Implementation:
- Macro that expands to `while` + `let`
- Can be implemented in pure Lisp once self-hosting

## Architecture Decisions

### Nanopass Design

All optimization passes follow nanopass principles:
1. Each pass does one thing
2. Input IR -> Output IR (same or different type)
3. Passes are composable and can be enabled/disabled
4. Easy to test each pass in isolation

**Current Compilation Pipeline**:
```
Source → read-all → S-expressions
                        ↓
                   compile-program → (defun-list, main-ir)
                        ↓
                   apply-tco-to-all-functions (optimize.lisp)
                        ↓
                   codegen → ARM64 bytes + :tco-branch markers
                        ↓
                   resolve-tco-branches → ARM64 bytes (resolved)
                        ↓
                   build-macho → Mach-O executable
```

**Register Allocation Pipeline** (reg-alloc.lisp):
```
IR → ir-to-tac → TAC (Three-Address Code)
                   ↓
             compute-liveness → TAC + live-in/live-out sets
                   ↓
             compute-intervals → (vreg, start, end) tuples
                   ↓
             linear-scan → allocation: vreg → phys-reg | spill
                   ↓
             tac-codegen → ARM64 bytes (TODO)
```

### Intermediate Representations

**Tree IR**: Original compilation output
- Nested expression trees: `(add (var 0) (mul (lit 2) (var 1)))`
- Used by current codegen (accumulator model)

**TAC (Three-Address Code)**: Linear SSA-like form
- Virtual registers: `v0, v1, v2, ...`
- Explicit operations: `(tac-binop v3 add v1 v2)`
- Used by register allocator

### File Organization

```
bootstrap/
  compiler.lisp      - Frontend: S-expr to Tree IR
  optimize.lisp      - Optimization passes (TCO)
  codegen.lisp       - Backend: Tree IR to ARM64
  reg-alloc.lisp     - Register allocation nanopasses
  macho.lisp         - Linker: ARM64 to Mach-O
  macho-utils.lisp   - Native Mach-O utilities
  reader.lisp        - Lisp reader
  dwarf.lisp         - (future) Debug info generator
```

## Testing Strategy

Each feature should include:
1. Unit tests for the pass itself
2. Integration tests for end-to-end behavior
3. Performance benchmarks where relevant

Test commands:
```bash
# Build and run specific test
sbcl --load bootstrap/*.lisp --eval '(habu:deliver-v3 "..." "/tmp/test")' --quit
/tmp/test; echo $?

# Run while loop test
(deliver-v3 "(let ((sum 0) (i 1))
               (while (<= i 10)
                 (setq sum (+ sum i))
                 (setq i (+ i 1)))
               (sys-exit sum))"
            "/tmp/test_while")
```

## References

- ARM64 Architecture Reference Manual
- Apple Mach-O File Format Reference
- DWARF Debugging Standard v5
- Common Lisp HyperSpec (loop, tail recursion)
