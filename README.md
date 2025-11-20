# Habu

A Common Lisp implementation for bare-metal ARM64 systems with real-time constraints.

## Features

- Sub-millisecond latency for control loops
- GC-free execution mode using region allocators
- Incremental generational GC for background tasks
- Direct compilation to native machine code
- Minimal runtime footprint
- Cross-platform development and testing

## Supported Platforms

**Development Platforms:**
- macOS ARM64 (Apple Silicon) - primary development target
- Linux x86_64 - testing and CI

**Deployment Targets:**
- ARM64 bare-metal (planned)
- ARM64 Linux (planned)

## Building

```bash
make              # Build runtime and tests
make test         # Run tests
make benchmark    # Run benchmarks
make examples     # Build example programs
```

### Platform-Specific Builds

```bash
# Native build (auto-detects platform)
make clean && make test

# Cross-compile for ARM64 (from x86_64)
make cross
```

## Requirements

- GCC or Clang
- Make
- SBCL (for bootstrap compiler, planned)

**For cross-compilation:**
- aarch64-linux-gnu-gcc (ARM64 toolchain)

## Usage

```lisp
;; GC-free control loop
(defun control-loop ()
  (with-gc-disabled
    (with-region temp
      (loop
        (let ((sensors (read-sensors temp)))
          (process sensors))
        (reset-region temp)))))

;; Normal GC-managed code
(defun background-task ()
  (let ((data (collect-data)))
    (process-data data)))
```

## Interactive REPLs

Habu includes **three progressive Lisp REPLs** demonstrating the evolution from basic evaluation to a complete Lisp interpreter:

### 1. Enhanced REPL (56KB)
Basic Lisp with quote, symbols, and evaluation.

```bash
./habu-enhanced
habu> (car '(1 2 3))
1
```

**Features**: Quote, symbols, if, lists (cons/car/cdr), arithmetic

### 2. Programmable REPL (73KB)
Adds functional programming with let and lambda.

```bash
./habu-prog
habu> ((lambda (x) (* x x)) 5)
25
```

**Features**: + let, lambda, closures, higher-order functions

### 3. Recursive REPL (73KB) - **Complete Lisp**
Full recursion with defun - a complete Lisp in 320 lines!

```bash
./habu-rec
habu> (defun factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))
<symbol>
habu> (factorial 10)
3628800
```

**Features**: + defun, recursion, comparisons, persistent definitions

### Quick Start

```bash
# Run the complete Lisp REPL
./habu-rec

# Load standard library examples (copy/paste into REPL)
# See stdlib.lisp and examples.lisp for 100+ utility functions
```

### Documentation

**Getting Started:**
- **[README_REPL.md](README_REPL.md)** - Quick start guide and tutorial
- **[QUICK_REFERENCE.md](QUICK_REFERENCE.md)** - Syntax reference card
- **[EXAMPLE_SESSION.md](EXAMPLE_SESSION.md)** - Annotated REPL session
- **[FAQ.md](FAQ.md)** - Frequently asked questions

**Reference:**
- **[REPL_FINAL_STATUS.md](REPL_FINAL_STATUS.md)** - Complete feature documentation
- **[REPL_PROGRESSION.md](REPL_PROGRESSION.md)** - Evolution from basic to complete
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Contributor's guide

**Code Resources:**
- **[stdlib.lisp](stdlib.lisp)** - Standard library with 100+ functions
- **[examples.lisp](examples.lisp)** - Algorithms and demonstrations

**Testing:**
- Run `./test-repls.sh` - Automated test suite
- Run `./demo.sh` - Interactive demonstration
- Run `./bench-repls.sh` - Performance benchmarks
- Or use `make repl-test`, `make repl-demo`, `make repl-bench`

### REPL Highlights

✨ **Complete Lisp implementation** in 320 lines of Lisp code
✨ **Only 1 C primitive added** - minimal runtime philosophy
✨ **73KB executable** - complete Lisp in your pocket
✨ **Pure functional** - no mutation, environment passing
✨ **Production-ready** - all core features working

Perfect for learning Lisp, experimentation, and embedded scripting!

## Project Status

**REPLs**: ✅ Complete and production-ready (see REPL_FINAL_STATUS.md)
**Main Project**: Early development. See DESIGN.md for architecture details.

## License

To be determined.
