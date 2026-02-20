# Habu Documentation

Technical documentation for Habu Lisp (Zig implementation).

## Architecture

- **[ARCHITECTURE.md](ARCHITECTURE.md)** - System architecture overview
  - Value representation and tagging
  - Compiler pipeline (Reader → IR → Bytecode → VM)
  - Memory management and GC
  - Directory structure

## Type System

- **[type-system.md](type-system.md)** - Type system specification
  - Gradual typing with occurrence typing
  - Type syntax and annotations
  - Type checking semantics

## Implementation Details

- **[zig-0.15-api.md](zig-0.15-api.md)** - Zig 0.15 API patterns
  - ArrayList unmanaged API
  - I/O changes
  - Build system patterns

- **[KEYWORD-SUPPORT.md](KEYWORD-SUPPORT.md)** - Keyword implementation
- **[PRIMITIVES-ADDED.md](PRIMITIVES-ADDED.md)** - Primitive function reference
- **[maxima-loader.md](maxima-loader.md)** - Maxima loader and stub workflow
- **[gc-architecture-comparison.md](gc-architecture-comparison.md)** - SBCL/OCaml GC technique mapping and Habu parity plan
- **[gc-parity-contract.md](gc-parity-contract.md)** - GC parity gates, regression contract, and CI enforcement

## Archive

Old documentation from the Common Lisp implementation is in `archive/`.
