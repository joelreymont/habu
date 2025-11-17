# Habu Lisp - Roadmap to Full Implementation

## Current Status (Implemented)
- ✅ **Data Types**: Fixnum integers (tagged pointers)
- ✅ **Arithmetic**: +, -, *, /, mod
- ✅ **Comparison**: <, >, =, <=, >=
- ✅ **Boolean Logic**: and, or, not (with short-circuit)
- ✅ **Control Flow**: if, cond, case, when, unless, progn
- ✅ **Variables**: let bindings (lexical scoping)
- ✅ **Functions**: lambda expressions, closures
- ✅ **Quote**: Literal fixnums
- ✅ **Lists**: car, cdr (read-only access)
- ✅ **Dual Target**: x86_64 and ARM64 code generation

## Phase 1: Complete Core Language (Bootstrap-friendly)
### Immediate Priorities
- [ ] **List Construction**: cons, list (requires runtime heap allocation)
- [ ] **More Data Types**:
  - [ ] Booleans (t/nil as special values)
  - [ ] Characters (tagged as fixnums)
  - [ ] Symbols (symbol table + interning)
  - [ ] Strings (heap-allocated, immutable)
- [ ] **More List Operations**:
  - [ ] null, consp, atom predicates
  - [ ] list, append, reverse
  - [ ] nth, nthcdr, length
- [ ] **Global Definitions**:
  - [ ] defun (named functions)
  - [ ] defparameter/defvar (global variables)
  - [ ] defconstant (compile-time constants)

### Recursion & Iteration
- [ ] **Tail-Call Optimization**: Critical for recursive algorithms
- [ ] **Named-let**: Local recursion (Scheme-style)
- [ ] **Loop Constructs**:
  - [ ] dotimes
  - [ ] dolist
  - [ ] loop (basic form)

### Advanced Control
- [ ] **Non-local Exits**:
  - [ ] block/return-from
  - [ ] catch/throw
  - [ ] unwind-protect
- [ ] **Multiple Values**: values, multiple-value-bind

## Phase 2: Runtime System
### Memory Management
- [ ] **Heap Allocator**: Simple bump allocator
- [ ] **Garbage Collector**: Mark-and-sweep or copying GC
- [ ] **Memory Pools**: For different object sizes
- [ ] **Write Barriers**: For generational GC (future)

### Symbol System
- [ ] **Symbol Table**: Hash table for interning
- [ ] **Packages**: Basic package system
- [ ] **Symbol Properties**: plist, symbol-value, symbol-function

### Function System
- [ ] **Function Objects**: First-class functions
- [ ] **Apply/Funcall**: Dynamic function application
- [ ] **Function Cells**: Separate from value cells
- [ ] **Closures**: Full closure support with environment capture

## Phase 3: Advanced Features
### Macro System
- [ ] **defmacro**: Macro definitions
- [ ] **Backquote**: ` , ,@ reader syntax
- [ ] **gensym**: Hygienic macro support
- [ ] **Macro Expansion**: macroexpand, macroexpand-1
- [ ] **Compiler Macros**: Optimization hints

### Object System (CLOS-lite)
- [ ] **defstruct**: Structure definitions
- [ ] **defclass**: Basic classes (optional)
- [ ] **defmethod**: Generic functions (optional)
- [ ] **Slots**: Accessor functions

### Advanced Data Types
- [ ] **Arrays**: Multi-dimensional arrays
- [ ] **Hash Tables**: make-hash-table, gethash, etc.
- [ ] **Streams**: Input/output streams
- [ ] **Floating Point**: IEEE 754 doubles
- [ ] **Bignums**: Arbitrary precision integers

## Phase 4: Standard Library
### Core Functions
- [ ] **List Processing**: mapcar, reduce, filter, etc.
- [ ] **Sequence Functions**: elt, subseq, concatenate
- [ ] **String Functions**: string=, string-upcase, format
- [ ] **Math Functions**: sin, cos, sqrt, expt
- [ ] **I/O Functions**: read, print, format

### Utilities
- [ ] **Error Handling**: error, warn, cerror
- [ ] **Conditions**: define-condition, handler-case
- [ ] **Assertions**: assert, check-type
- [ ] **Time/Date**: get-universal-time, sleep

## Phase 5: Self-Hosting
### Compiler in Habu
- [ ] **Reader**: S-expression parser in Habu
- [ ] **Compiler**: Bootstrap compiler written in Habu
- [ ] **Code Generator**: x86_64/ARM64 backends in Habu
- [ ] **Optimizer**: Peephole, constant folding, etc.

### Metaprogramming
- [ ] **Compiler Hooks**: Compile-time code execution
- [ ] **Load-time Evaluation**: eval-when
- [ ] **Code Walking**: For analysis and transformation

## Phase 6: Performance & Optimization
### Compiler Optimizations
- [ ] **Constant Folding**: Compile-time evaluation
- [ ] **Dead Code Elimination**: Remove unused code
- [ ] **Inline Expansion**: Inline small functions
- [ ] **Register Allocation**: Better register usage
- [ ] **Peephole Optimization**: Local code improvements
- [ ] **Type Inference**: Optional type declarations

### Runtime Optimizations
- [ ] **Generational GC**: Faster garbage collection
- [ ] **JIT Compilation**: Runtime code generation (optional)
- [ ] **Native Threads**: Parallel execution (ARM64 specific)

## Phase 7: Tooling & Ecosystem
### Development Tools
- [ ] **REPL**: Interactive read-eval-print loop
- [ ] **Debugger**: Step, breakpoints, inspect
- [ ] **Profiler**: Performance analysis
- [ ] **Tracer**: Function call tracing

### Documentation
- [ ] **Language Manual**: Complete reference
- [ ] **Tutorial**: Getting started guide
- [ ] **API Documentation**: Standard library docs
- [ ] **Examples**: Code samples and patterns

## Phase 8: Integration & Deployment
### FFI (Foreign Function Interface)
- [ ] **C Interop**: Call C functions
- [ ] **Struct Marshalling**: Pass C structs
- [ ] **Callbacks**: C code calling Habu

### Bare Metal Support
- [ ] **No-OS Boot**: Boot without OS
- [ ] **Interrupt Handlers**: ARM64 interrupts
- [ ] **Memory-Mapped I/O**: Hardware access
- [ ] **Real-Time Guarantees**: Predictable GC pauses

## Current Focus
Based on "keep going to full Lisp", the immediate priorities are:

1. **Fix named-let** or skip it temporarily
2. **Add more special forms** that don't need runtime:
   - begin (alias for progn)
   - multiple expressions in cond clauses
   - setq for mutation (with lexical tracking)
3. **Implement defun** with a global function table
4. **Add more operators**:
   - Bitwise: and, or, xor, not, shift
   - Numeric: min, max, abs, truncate
5. **Build test suite** for all features
6. **Create simple runtime** for cons/list support
7. **Add tail-call optimization** for recursion
8. **Implement basic REPL** for interactive development

This roadmap provides a clear path from current capabilities to a full, self-hosting Common Lisp implementation suitable for bare-metal ARM64 with real-time constraints.
