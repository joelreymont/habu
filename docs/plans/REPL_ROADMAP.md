# Habu REPL - Development Roadmap

This document outlines potential future enhancements and development directions for the Habu REPL project.

## Current Status: v1.0 - Complete ✅

**Released**: November 19, 2024

The current implementation provides:
- ✅ Three progressive REPLs (enhanced → programmable → recursive)
- ✅ Complete Lisp interpreter (320 lines)
- ✅ Comprehensive documentation (4,500+ lines)
- ✅ Standard library (100+ functions)
- ✅ Automated testing and benchmarking
- ✅ Production-ready implementation

## Development Philosophy

**Priorities**:
1. **Simplicity** - Keep code clear and understandable
2. **Minimalism** - Avoid unnecessary features
3. **Educational Value** - Teach Lisp and language implementation
4. **Compatibility** - Maintain compatibility with v1.0

**Non-Goals**:
- Competing with production Lisps (SBCL, Chez Scheme)
- Maximum performance
- Feature parity with Common Lisp or Scheme standards

---

## 🔵 Near-Term Enhancements (v1.1-1.3)

### v1.1 - Usability Improvements

#### Better Error Messages
**Priority**: High | **Effort**: Medium

Add informative error handling:
```lisp
Before: habu> (foo 1 2)
        nil

After:  habu> (foo 1 2)
        Error: Undefined function 'foo'
```

#### Multi-Line Input
**Priority**: Medium | **Effort**: Low

```lisp
habu> (defun factorial (n)
  ...   (if (= n 0) 1
  ...     (* n (factorial (- n 1)))))
```

#### Command History
**Priority**: Medium | **Effort**: Medium

- Up/down arrow navigation
- Persist to ~/.habu_history

### v1.2 - Minor Language Features

#### Logical Operators
**Priority**: High | **Effort**: Low

```lisp
(and expr1 expr2 ...)  ; Short-circuit
(or expr1 expr2 ...)
(not expr)
```

#### More Comparisons
**Priority**: Medium | **Effort**: Low

```lisp
(<= a b)  (>= a b)  (!= a b)
```

#### Progn (Sequencing)
**Priority**: Medium | **Effort**: Low

```lisp
(progn expr1 expr2 ... exprN)  ; Returns last
```

#### Cond (Multi-way Conditional)
**Priority**: Medium | **Effort**: Low

```lisp
(cond
  ((< x 0) 'negative)
  ((= x 0) 'zero)
  (else 'positive))
```

---

## 🟢 Mid-Term Enhancements (v2.0)

### Tail-Call Optimization
**Priority**: High | **Effort**: Very High

Enable deep recursion without stack overflow.

**Approaches**:
- Trampoline pattern
- CPS transformation
- Custom C runtime changes

### Let* and Letrec
**Priority**: Medium | **Effort**: Medium

```lisp
; Sequential binding
(let* ((x 10)
       (y (+ x 5)))  ; Can use x
  y)

; Recursive binding
(letrec ((even? (lambda (n) ...))
         (odd? (lambda (n) ...)))
  (even? 10))
```

### Variadic Functions
**Priority**: Medium | **Effort**: Medium

```lisp
(defun sum (& args)
  (fold + 0 args))

(sum 1 2 3 4 5)  ; → 15
```

### Basic Macro System
**Priority**: Medium | **Effort**: Very High

```lisp
(defmacro when (test & body)
  `(if ,test (progn ,@body) nil))
```

Requires quasiquote, unquote, macro expansion phase.

---

## 🟡 Long-Term Enhancements (v3.0+)

### Module System
Organize code into modules with imports/exports.

### Floating-Point Numbers
Add float type, parsing, printing, arithmetic.

### Bytecode Compilation
Compile to bytecode for faster execution.

### Simple JIT
Compile hot code to native (extremely complex).

### Continuations
First-class continuations like Scheme (call/cc).

---

## Performance Optimizations

### Interpreter-Level
- Instruction cache
- Inline primitives
- Constant folding

### Runtime-Level
- Hash tables for environment (O(1) vs O(n))
- Generational GC
- Better memory layout

---

## Documentation Improvements

### Tutorial Series
- "Learn Lisp with Habu" (10 lessons)
- "Implementing a Lisp" (for students)
- "Advanced Habu Techniques"

### Video Tutorials
- Getting started
- Building from scratch
- Advanced programming

### Interactive Exercises
- Beginner (20 problems)
- Intermediate (20 problems)
- Advanced (10 problems)

---

## Community and Ecosystem

### Standard Library Expansion
Grow to 200+ functions.

### Example Programs
- Parser combinators
- Mini interpreters
- Simple games
- Algorithm visualizations

### Package Manager
```bash
habu install list-utils
habu install algorithms
```

---

## Testing and Quality

### Expanded Test Suite
- Unit tests for all features
- Integration tests
- Regression tests
- Performance tests

### Fuzzing
Find bugs with random inputs.

### Benchmarking Suite
Track performance across versions.

---

## Platform Support

### Windows Support
Port to Windows with proper line endings and terminal support.

### WebAssembly Build
Compile to WASM for browser-based REPL.

### Mobile Support (Very Low Priority)
iOS/Android apps for education.

---

## Decision Criteria

### Should We Add Feature X?

1. **Aligns with philosophy?** (simple, educational, minimal)
2. **Implementation cost?** (LOC, complexity, maintenance)
3. **Benefit?** (users, use cases, essentiality)
4. **Can be in stdlib?** (prefer Lisp over C)

### When to Say No
- Too complex
- Rarely used
- Can be in stdlib
- Conflicts with philosophy
- High maintenance cost

---

## How to Contribute

1. Check existing issues
2. Discuss before implementing
3. Start with near-term features
4. Follow CONTRIBUTING.md
5. Write tests
6. Update documentation

---

## Version History

- **v1.0** (Nov 2024) - Initial release
  - Three progressive REPLs
  - Complete Lisp interpreter
  - Comprehensive documentation
  - Standard library
  - Tests, demos, benchmarks

---

**This roadmap is a living document.** Priorities may shift based on community feedback, contributor interest, and technical constraints.

**Your input matters!** Open issues to discuss features or suggest changes.

**Welcome to Habu Lisp development!** 🚀
