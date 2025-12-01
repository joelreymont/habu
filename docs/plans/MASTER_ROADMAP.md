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

### Stack Overflow Fix
**Priority**: P1 (Blocker for self-hosting)

The reader uses recursive functions that blow the stack when processing large files:
- `read-sym-chars` - recurses per character
- `upcase-string-iter` - recurses per character
- `read-list-elems` - recurses per list element

**Option A: Rewrite reader to use `while` loops**
- Estimated: 2-3 functions to rewrite
- Lower risk, mechanical transformation
- Works immediately with existing codegen

**Option B: Implement TCO**
- More general solution
- Higher complexity
- Fixes all recursive functions automatically

## Planned Features

### 1. Tail Call Optimization (TCO)
**Priority**: P2

Implement as two nanopasses:

**Pass 1: mark-tail-positions**
- Walk IR and mark calls in tail position
- Tail position = result is immediately returned
- Handle: if branches, progn last form, let body

**Pass 2: TCO codegen**
- For self-recursive tail calls: emit `b` (jump) instead of `bl` (call)
- Reuse current stack frame
- Update arguments in-place before jump

Files to modify:
- `compiler.lisp`: Add mark-tail-positions pass
- `codegen.lisp`: Handle `tail-call-ir` node

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

Current codegen spills heavily. Implement proper allocation:

**Linear Scan Algorithm** (simpler):
1. Compute live intervals for each variable
2. Sort by start position
3. Allocate registers greedily, spill when exhausted

**Graph Coloring** (better results):
1. Build interference graph
2. Color with k=8 (available registers)
3. Spill nodes that can't be colored

ARM64 available registers: x9-x15, x19-x23 (~12 registers)

Files to modify:
- `codegen.lisp`: Replace temp-slot allocation
- New file: `regalloc.lisp`

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

All optimization passes should follow nanopass principles:
1. Each pass does one thing
2. Input IR -> Output IR (same or different type)
3. Passes are composable and can be enabled/disabled
4. Easy to test each pass in isolation

Current passes:
1. `read-all`: Source -> S-expressions
2. `compile-program`: S-expressions -> IR + defuns
3. `lift-lambdas`: IR -> IR (with lambdas extracted)
4. `codegen`: IR -> ARM64 bytes
5. `build-macho`: bytes -> executable

Planned passes:
- `mark-tail-positions`: IR -> IR (with tail markers)
- `optimize-ir`: IR -> IR (constant folding, etc.)
- `allocate-registers`: IR -> IR (with register assignments)

### File Organization

```
bootstrap/
  compiler.lisp      - Frontend: parsing to IR
  codegen.lisp       - Backend: IR to ARM64
  macho.lisp         - Linker: ARM64 to Mach-O
  reader.lisp        - Lisp reader
  regalloc.lisp      - (future) Register allocator
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
