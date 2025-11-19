# Habu Lisp Compiler Roadmap

## Overview

Habu is a Lisp compiler being built in two phases:
- **Phase 1 (Current):** Bootstrap compiler using SBCL FFI trampolines
- **Phase 2 (Future):** Standalone compiler with inline allocation

---

## Phase 1: Bootstrap Compiler (Current)

Using SBCL's runtime via FFI for rapid development while maintaining a clean architecture for eventual standalone operation.

### 1. Runtime Funcall ✅

**Status:** Complete
**Completed:** Session 4

**Implementation:**
- ✅ Store compiled code pointers in symbol-function slots
- ✅ Generate machine code for runtime function lookup
- ✅ Support calling conventions (0-3 parameters via registers)
- ✅ Integrated with defun/symbol system via alien-callables
- ✅ Both x86_64 and ARM64 support

**Testing:** 597/597 tests passing + 15 infrastructure tests

**Benefit:** Enables higher-order functions, dynamic dispatch, true functional programming

---

### 2. Self-Hosting Capability

**Status:** Partially complete
**Goal:** Compiler can compile itself

**Missing Components:**

#### Reader/Printer ✅
**Status:** Complete (Session 6)
**Implementation:**
- ✅ Tokenizer for S-expressions (numbers, symbols, strings, lists, quotes)
- ✅ Parser with support for lists, dotted pairs, and quoted expressions
- ✅ Printer for all runtime values (fixnums, strings, cons cells, symbols, closures)
- ✅ read operation - parse S-expression from string
- ✅ print operation - convert runtime value to string
- ✅ Round-trip tested (read then print)
- ✅ x86_64 code generation via FFI trampolines
**Note:** Basic file I/O now implemented! Can read/write files programmatically.

#### File I/O ✅
**Status:** Complete (Session 6)
**Implementation:**
- ✅ file-open: Open file with mode ("r", "w", "a"), returns file handle
- ✅ file-read: Read contents from file handle
- ✅ file-write: Write string to file handle
- ✅ file-close: Close file handle
- ✅ read-file: Convenience function to read entire file
- ✅ write-file: Convenience function to write entire file
- ✅ File handle table for managing open files
- ✅ x86_64 code generation via FFI trampolines
**Note:** Load compiled code not yet implemented

#### Error Handling
**Status:** Partially complete
**Implementation:**
- ✅ Catch/throw for non-local exits (Phase 1)
- ✅ Both x86_64 and ARM64 support
- ✅ FFI trampolines to SBCL's catch/throw
- ⏳ handler-case/handler-bind (not yet implemented)
- ⏳ unwind-protect for cleanup (not yet implemented)
- ⏳ Stack traces and error messages (Phase 2)

#### More Control Flow
- do, dotimes, dolist
- block, return-from
- tagbody, go (for complex control flow)

**Benefit:** True bootstrapping - compiler compiles itself

---

### 3. Essential Language Features

**Status:** Partially complete

#### Strings ✅
**Status:** Complete (Session 6) - Basic ASCII support
**Implementation:**
- ✅ String type with heap allocation (tag 0x3)
- ✅ String literals in compiler ("hello")
- ✅ String operations: string-concat, string-substring, string-length
- ✅ String comparison: string-equal
- ✅ x86_64 and ARM64 code generation
- ✅ FFI trampolines for runtime operations
**Limitations:**
- ASCII only (no UTF-8 support yet)
- Symbol names still use fixnum hashes
**Note:** Symbol names as actual strings pending future work

#### Closures ✅
**Status:** Complete (Session 5)
**Implementation:**
- ✅ Free variable analysis
- ✅ Heap-allocated closure objects (tag 0x7)
- ✅ Captured environment support (0-3 variables in Phase 1)
- ✅ First-class functions that can be stored, passed, and returned
- ✅ Closure factories (functions returning closures)
- ✅ Runtime wrapper creation via eval
- ✅ Both inline and standalone closure creation
- ✅ x86_64 and ARM64 support

**Testing:** All tests passing (inline, standalone, factories)

#### Hash Tables
**Current:** None
**Needed:**
- Hash table type
- get, put, remove operations
- Critical for many algorithms

#### Multiple Return Values
**Current:** Single return value only
**Needed:**
- values, multiple-value-bind
- Common Lisp compatibility

#### Destructuring
**Current:** None
**Needed:**
- destructuring-bind
- Pattern matching in let/lambda

#### Iteration
**Current:** Recursion only
**Needed:**
- loop macro
- do, dotimes, dolist
- More ergonomic iteration

---

### 4. Standard Library Expansion

**Status:** Basic operations complete

#### List Operations
**Current:** cons, car, cdr, list, length, nth, append, reverse
**Needed:**
- member, assoc, remove-if, filter, map
- find, position, count
- every, some, notany, notevery
- push, pop, pushnew
- last, butlast, nthcdr

#### Sequence Operations
**Needed:**
- Generic operations working on lists and arrays
- reduce, map, filter patterns
- Sequence predicates

#### Number Operations
**Current:** Basic arithmetic, bitwise, predicates
**Needed:**
- Floating point support
- More number theory (lcm, expt, sqrt, etc.)
- Random numbers
- Ratios and bignums

#### I/O Primitives
**Needed:**
- read, print, format
- File operations
- Stream abstraction

---

## Phase 2: Standalone Compiler (Future)

Remove all SBCL dependencies, make Habu truly standalone.

### 1. Inline Allocation

**Goal:** Eliminate FFI trampolines

**Current Architecture:**
- Compiler generates calls to SBCL FFI trampolines
- Trampolines call Habu runtime functions
- Requires SBCL to run compiled code

**Target Architecture:**
- Compiler generates direct heap allocation code
- No FFI calls, just machine code
- Compiled programs run without SBCL

**What's Needed:**
- Compile runtime/memory.lisp to machine code
- Generate inline allocation sequences
- Handle heap exhaustion inline
- Calling convention for GC

**Benefit:** Standalone executables, no SBCL runtime dependency

---

### 2. Conservative/Precise GC

**Goal:** Robust garbage collection

**Current Limitation:**
- GC uses explicit root registry
- FFI calls don't automatically track stack values
- Workaround: 1MB heap is large enough for typical operations

**Target:**
- Stack scanning to find all live references
- Conservative GC (treat anything that looks like a pointer)
- Or precise GC with compiler-generated GC maps

**Benefit:** Safer memory management, handle larger programs

---

### 3. Full Standalone Operation

**Components:**

#### Symbol Packages
- Namespace isolation
- import, export, use-package
- Package prefixes (package:symbol)

#### Complete String Implementation
- First-class string type
- Full Unicode support
- String interning for symbols

#### Standard Library in Pure Habu
- Rewrite SBCL-dependent code
- All stdlib in Habu itself
- Minimal C runtime dependencies

#### Standalone Executable
- Compile to native executable
- No SBCL dependency
- Minimal runtime (just libc)

---

## Current Status (November 2025)

### Completed ✅

**Core Compiler:**
- 597 passing tests
- x86_64 and ARM64 targets (both fully supported)
- Comprehensive optimization passes
- ARM64 support: all runtime operations (lists, strings, read/print, file I/O)

**Data Types:**
- Fixnums (tagged pointers)
- Cons cells (heap-allocated)
- Symbols (with interning)

**Language Features:**
- defun/defvar/defmacro with symbol integration
- Macros with quasiquote and nested macro support
- funcall (compile-time only)
- set for modifying global variables
- Lambda expressions
- Lexical scoping (let bindings)

**List Operations:**
- cons, car, cdr, list
- length, nth, append, reverse

**Control Flow:**
- if, cond, case, when, unless
- progn, begin
- and, or (short-circuit)

**Operators:**
- Arithmetic: +, -, *, /, mod, div
- Comparison: <, >, =, <=, >=
- Bitwise: logand, logior, logxor, lognot, ash
- Boolean: not, and, or
- 80+ operators total

**Memory Management:**
- Mark-and-sweep GC with compaction
- GC root registry
- Automatic GC triggering
- Heap allocation via FFI trampolines

**ARM64 Code Generation:**
- Complete parity with x86_64 for all runtime operations
- List operations: cons, car, cdr, list, length, nth, append, reverse
- String operations: string-length, string-concat, string-equal, string-substring
- Reader/printer: read, print
- File I/O: file-open, file-read, file-write, file-close, read-file, write-file
- Error handling: catch, throw
- Helper functions: arm64-load-imm64, int-to-arm64-mov-imm
- All 597 tests passing on ARM64
- Testable locally on Apple Silicon Macs

**Error Handling:**
- catch/throw for non-local exits
- Both x86_64 and ARM64 support
- Fixnum and symbol tags
- Phase 1 implementation via SBCL's catch/throw
- Documentation: docs/ERROR_HANDLING.md

---

## Architecture Decisions

### Two-Phase Strategy
The hybrid bootstrap approach allows:
- **Rapid development** using SBCL's infrastructure (Phase 1)
- **Clean architecture** that's ready for standalone operation (Phase 2)
- **No technical debt** from shortcuts

### Symbol System (Lisp-2)
- Separate value and function namespaces
- Traditional Common Lisp semantics
- Enables both variables and functions with same name
- 48-byte symbol structure: header + name + value + function + plist

### Mark-and-Sweep GC
- Simple and correct
- Good cache locality after compaction
- Foundation for future generational GC
- Unbound marker: 0xFFFFFFFFFFFFFFFF (all bits set)

### FFI Trampolines (Phase 1)
- System V AMD64 ABI calling convention
- Clean separation between compiler and runtime
- Easy to replace with inline code (Phase 2)

---

## Testing Strategy

- Comprehensive test suite (597 tests, 100% passing)
- Test-driven development
- Each feature gets dedicated test file
- All tests must pass before commit
- Tests cover: operators, control flow, lists, symbols, GC, macros

---

## Documentation

### Current Docs:
- `docs/GC_INTEGRATION.md` - GC architecture and usage
- `docs/SYMBOLS.md` - Symbol system reference
- `docs/BOOTSTRAP_VS_STANDALONE.md` - Two-phase approach explanation
- `bootstrap/SESSION_SUMMARY.md` - Detailed session work log
- `ROADMAP.md` - This file

### Code Structure:
- `bootstrap/compiler.lisp` - Main compiler (4200+ lines)
- `runtime/memory.lisp` - Heap allocator and GC
- `runtime/symbols.lisp` - Symbol table and operations
- `runtime/lists.lisp` - List runtime functions
- `bootstrap/test-*.lisp` - Test files
- `bootstrap/run-all-tests.lisp` - Test runner

---

## Priority Queue

### Immediate (This Session)
1. ✅ Global variable modification (set)
2. ✅ List operations (length, nth, append, reverse)
3. ✅ Runtime funcall design and implementation
4. ✅ Closures (lexical function values)
5. ✅ String type implementation (ASCII)

### Short Term (Next Sessions)
1. ✅ Reader/printer for S-expressions
2. ✅ Basic file I/O
3. Error handling/condition system
4. More control flow (do, dotimes, dolist)
5. Hash tables

### Medium Term (Phase 1 Completion)
1. Self-hosting capability
2. Complete standard library
3. Error handling/condition system
4. Loop/iteration constructs
5. Hash tables
6. Multiple return values

### Long Term (Phase 2)
1. Inline allocation
2. Conservative/Precise GC
3. Symbol packages
4. Standalone executable
5. Remove all SBCL dependencies

---

## Contributing Priorities

1. **Phase 1 completion** - Get to self-hosting
2. **Test coverage** - Maintain 100% passing tests
3. **Documentation** - Keep docs updated
4. **Performance** - Optimize after correctness

---

## Version History

- **v0.1** (2025-11-17): Initial compiler with basic operators
- **v0.2** (2025-11-19): Symbol system, GC, macros, list operations
- **v0.3** (2025-11-19): Runtime funcall, closures, strings (ASCII)
- **v0.4** (2025-11-19): Reader/printer, file I/O, comprehensive ARM64 support
- **v0.5** (2025-11-19): Error handling with catch/throw

---

**Last updated:** 2025-11-19
**Current phase:** Phase 1 - Bootstrap with FFI
**Tests passing:** 597/597 ✅ (x86_64 and ARM64)
**Next priority:** More control flow (do, dotimes, dolist) OR Hash tables
