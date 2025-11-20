# Habu REPL - Lisp Compliance & Self-Hosting Gap Analysis

**Date**: November 20, 2024
**Current Version**: v1.0 (Complete Lisp Interpreter)
**Status**: Production-ready, but not spec-compliant

---

## Executive Summary

The Habu REPL implements a **complete, working Lisp interpreter** with all core features (quote, if, let, lambda, defun, recursion). However, it is **not compliant** with any major Lisp specification (Common Lisp, Scheme R5RS/R7RS, or Clojure).

**Key Findings:**
- ✅ **Core Lisp semantics**: Complete
- ❌ **Spec compliance**: None (by design - educational/minimal implementation)
- ❌ **Self-hosting capability**: Missing ~30 features
- 📊 **Estimated gap**: ~60-80% of Common Lisp, ~50-70% of Scheme R5RS

---

## Part 1: Current Implementation (v1.0)

### What Habu REPL Has ✅

#### Data Types (5 types)
- ✅ **Fixnum** (tagged integers)
- ✅ **Cons** (pairs/lists)
- ✅ **Symbol** (interned symbols)
- ✅ **String** (immutable strings)
- ✅ **Closure** (lexically-scoped functions)

#### Special Forms (5 forms)
- ✅ **`quote`** - Literal data
- ✅ **`if`** - Conditional (3-arg only)
- ✅ **`let`** - Lexical binding (1-level)
- ✅ **`lambda`** - Anonymous functions
- ✅ **`defun`** - Named functions (top-level only)

#### Built-in Operators (11 operators)
- ✅ **Arithmetic**: `+`, `-`, `*`, `/` (2-arg only)
- ✅ **Comparison**: `=`, `<`, `>` (2-arg only)
- ✅ **List ops**: `cons`, `car`, `cdr`, `list`

#### Primitives from C Runtime (13 primitives)
- ✅ `cons`, `car`, `cdr`, `get-tag`
- ✅ `make-symbol`, `symbol-name`
- ✅ `make-vector`, `vector-set`, `string-ref`, `string-length-raw`
- ✅ `make-string-from-vector`, `make-string-from-cstr`
- ✅ `print`, `println`, `print-value`, `readline`, `progn`

#### Features
- ✅ **Lexical closures** with proper environment capture
- ✅ **Recursion** via environment merging
- ✅ **First-class functions**
- ✅ **Pure functional** (no mutation)
- ✅ **Read-eval-print loop** with line editing

---

## Part 2: Gap Analysis - Common Lisp Compliance

### 2.1 Missing Core Language Features

#### Special Forms & Control Flow
- ❌ **`cond`** - Multi-way conditional (nested if workaround exists)
- ❌ **`and`, `or`** - Short-circuit logical operators
- ❌ **`progn`** - Sequential evaluation (C primitive exists, not exposed to Lisp)
- ❌ **`prog1`, `prog2`** - Return first/second value
- ❌ **`when`, `unless`** - One-armed conditionals
- ❌ **`case`, `typecase`** - Dispatch on value/type
- ❌ **`loop`** - Iteration construct
- ❌ **`do`, `dotimes`, `dolist`** - Iteration forms
- ❌ **`return`, `return-from`** - Non-local exit
- ❌ **`block`, `tagbody`, `go`** - Structured control flow
- ❌ **`catch`, `throw`** - Dynamic non-local exit
- ❌ **`unwind-protect`** - Cleanup forms

#### Variable Binding
- ❌ **`let*`** - Sequential binding (can only do parallel binding)
- ❌ **`letrec`** - Recursive local binding
- ❌ **`labels`, `flet`** - Local function definitions
- ❌ **`setq`, `setf`** - Variable assignment (pure functional only)
- ❌ **`multiple-value-bind`** - Multiple return values
- ❌ **`destructuring-bind`** - Pattern matching in bindings

#### Functions
- ❌ **Variadic functions** (`&rest`, `&optional`, `&key`)
- ❌ **Keyword arguments** (`&key`)
- ❌ **Default parameter values**
- ❌ **`apply`** - Apply function to list
- ❌ **`funcall`** - Call function
- ❌ **`mapcar`, `mapcan`, `mapc`** - Map functions (no stdlib integration)
- ❌ **`reduce`** - Fold operation (exists in stdlib but not built-in)
- ❌ **Multiple return values** (`values`, `values-list`)

#### Macros & Metaprogramming
- ❌ **`defmacro`** - Macro definitions
- ❌ **`macrolet`** - Local macros
- ❌ **Quasiquote** (`` ` ``, `,`, `,@`) - Template construction
- ❌ **`gensym`** - Generate unique symbols
- ❌ **`eval`** - Runtime evaluation (interpreter exists but not exposed)
- ❌ **Compile-time evaluation** (`eval-when`)

### 2.2 Missing Data Types

#### Numbers
- ❌ **Floating-point** (float, double)
- ❌ **Rationals** (1/3, 2/5)
- ❌ **Complex numbers**
- ❌ **Arbitrary precision integers** (bignums)
- ❌ **Characters** (separate from fixnums)

#### Composite Types
- ❌ **Arrays** (only vectors, no multidimensional)
- ❌ **Hash tables**
- ❌ **Structures** (`defstruct`)
- ❌ **CLOS objects** (`defclass`, `defmethod`)
- ❌ **Packages** and namespaces
- ❌ **Streams** (only basic I/O)

#### Other
- ❌ **Keywords** (`:foo` notation)
- ❌ **Pathnames**
- ❌ **Conditions** (error objects)

### 2.3 Missing Standard Library

#### Sequence Operations
- ❌ **`length`** - Get sequence length (exists in stdlib)
- ❌ **`elt`, `nth`** - Element access (nth in stdlib)
- ❌ **`reverse`** - Reverse sequence (exists in stdlib)
- ❌ **`append`** - Concatenate lists (exists in stdlib)
- ❌ **`member`, `find`, `position`** - Search (member? in stdlib)
- ❌ **`remove`, `delete`** - Remove elements
- ❌ **`sort`, `stable-sort`** - Sorting (sort in stdlib)
- ❌ **`subseq`** - Subsequence

#### Predicates
- ❌ **`null`, `atom`** - Type predicates (nil? in stdlib)
- ❌ **`numberp`, `consp`, `symbolp`** - Type tests (have fixnum?, cons?, symbol?)
- ❌ **`equal`, `eql`, `eq`** - Equality variants (only have =)
- ❌ **`zerop`, `plusp`, `minusp`** - Numeric predicates (have zero?, positive?, negative?)
- ❌ **`evenp`, `oddp`** - Parity tests (have even?, odd?)

#### Arithmetic
- ❌ **Variadic arithmetic** (`(+ 1 2 3 4)` - currently 2-arg only)
- ❌ **`1+`, `1-`** - Increment/decrement
- ❌ **`abs`, `min`, `max`** - Math functions (exist in stdlib)
- ❌ **`floor`, `ceiling`, `round`, `truncate`** - Rounding
- ❌ **`mod`, `rem`** - Modulo/remainder
- ❌ **`expt`, `sqrt`, `log`, `exp`** - Transcendental functions
- ❌ **`sin`, `cos`, `tan`** - Trigonometric functions

#### List Operations
- ❌ **`first`, `second`, ..., `tenth`** - Accessors
- ❌ **`last`, `butlast`** - Tail operations (last in stdlib)
- ❌ **`nthcdr`** - N-th cdr
- ❌ **`assoc`, `rassoc`** - Association list lookup
- ❌ **`pairlis`** - Make alist
- ❌ **`push`, `pop`** - Stack operations (no mutation)
- ❌ **`pushnew`, `adjoin`** - Set operations

#### String Operations
- ❌ **`string-upcase`, `string-downcase`** - Case conversion
- ❌ **`string=`, `string<`, etc.** - String comparison (have string=?)
- ❌ **`concatenate`** - String concatenation
- ❌ **`subseq`** - Substring
- ❌ **`char`** - Character access
- ❌ **String formatting** (`format`)

#### I/O
- ❌ **`read`, `print`, `prin1`, `princ`** - Full reader/printer (have basic print)
- ❌ **`read-line`, `read-char`** - Input functions (have readline)
- ❌ **`write-line`, `write-string`** - Output functions
- ❌ **`open`, `close`, `with-open-file`** - File I/O
- ❌ **`format`** - Formatted output

### 2.4 Summary: Common Lisp Compliance

**Estimated Compliance**: ~20-30% of ANSI Common Lisp

**What's Missing**:
- 🔴 **Critical**: Macros, quasiquote, variadic functions, proper error handling
- 🟡 **Important**: More data types (floats, hash tables), full stdlib
- 🟢 **Nice-to-have**: CLOS, packages, streams, format

---

## Part 3: Gap Analysis - Scheme R5RS Compliance

### 3.1 What Habu Has (Scheme Perspective)

✅ **Lexical scoping** (R5RS requirement)
✅ **First-class functions** (R5RS requirement)
✅ **Proper tail recursion** (❌ NOT IMPLEMENTED - required for R5RS!)
✅ **Closures** (R5RS requirement)
✅ **Quote, if, lambda, define** (R5RS requirement - we have defun)

### 3.2 Missing Scheme R5RS Features

#### Core Language
- ❌ **Proper tail-call optimization** (CRITICAL for R5RS)
- ❌ **`begin`** - Sequential evaluation (like progn)
- ❌ **`set!`** - Variable mutation
- ❌ **`define`** - Variable definition (we have defun for functions)
- ❌ **Internal definitions** (define inside lambda body)
- ❌ **Hygienic macros** (`define-syntax`, `syntax-rules`)

#### Conditionals
- ❌ **`cond`** with `else` clause
- ❌ **`case`** - Pattern matching
- ❌ **`and`, `or`** - Short-circuit evaluation

#### Binding
- ❌ **`let*`** - Sequential binding
- ❌ **`letrec`** - Recursive binding
- ❌ **`do`** - Iteration

#### Procedures
- ❌ **Variadic procedures** (rest arguments)
- ❌ **`apply`** - Apply function to list
- ❌ **Continuations** (`call-with-current-continuation` / `call/cc`)

#### Data Types
- ❌ **Booleans** (`#t`, `#f` - we use 1 and nil)
- ❌ **Characters** (`#\a`, `#\space`)
- ❌ **Vectors** (we have vectors but not full API)
- ❌ **Ports** (I/O streams)

#### Standard Procedures
- ❌ **Type predicates**: `boolean?`, `char?`, `vector?`, `procedure?`, `port?`
- ❌ **Equivalence**: `eq?`, `eqv?`, `equal?`
- ❌ **Pairs/Lists**: `null?`, `pair?`, `list?`, `length`, `append`, `reverse`, `memq`, `assq`
- ❌ **Symbols**: `symbol?`, `symbol->string`, `string->symbol`
- ❌ **Numbers**: Full numeric tower (exact/inexact, complex, rational)
- ❌ **Strings**: `make-string`, `string-length`, `string-ref`, `string-set!`, `substring`
- ❌ **Vectors**: `make-vector`, `vector-length`, `vector-ref`, `vector-set!`
- ❌ **I/O**: `read`, `write`, `display`, `newline`, `read-char`, `write-char`
- ❌ **eval**: `eval`, `scheme-report-environment`, `null-environment`

### 3.3 Summary: Scheme R5RS Compliance

**Estimated Compliance**: ~30-40% of Scheme R5RS

**Critical Blocker**: **No tail-call optimization** (TCO is REQUIRED for R5RS)

**What's Missing**:
- 🔴 **Critical**: Tail-call optimization, continuations, full numeric tower
- 🟡 **Important**: `begin`, `set!`, internal definitions, hygienic macros
- 🟢 **Nice-to-have**: Full standard library, ports, characters

---

## Part 4: Self-Hosting Requirements

### 4.1 What is Self-Hosting?

A **self-hosting compiler/interpreter** can compile/interpret itself. For Habu, this means:

```lisp
;; The Habu compiler (written in Habu Lisp) can compile itself
(compile-file "habu-compiler.lisp")  ; Produces habu-compiler.c

;; The Habu REPL (written in Habu Lisp) can be compiled by Habu
(compile-file "recursive-repl.lisp")  ; Produces habu-rec.c
```

**Current Status**: ❌ **Not Self-Hosting**
- Habu REPL is written in Habu Lisp ✅
- But the **bootstrap compiler** is written in Common Lisp (SBCL) ❌
- The compiler cannot compile itself ❌

### 4.2 What's Missing for Self-Hosting?

To make Habu self-hosting, we need to **rewrite the bootstrap compiler in Habu Lisp**. The bootstrap compiler (currently ~7,000 lines of Common Lisp) does:

1. **Parse Habu Lisp source** → AST
2. **Generate C code** from AST
3. **Emit C compilation commands**

#### Missing Language Features for Self-Hosting

##### Critical (Compiler Needs These)
- ❌ **File I/O** - Read source files, write C files
  - `open`, `close`, `read-line`, `write-line`
  - Current: Only have `readline` for REPL
- ❌ **String manipulation** - Build C code strings
  - `concatenate`, `format`, `string-append`
  - Current: Have basic strings, no manipulation
- ❌ **Hash tables** - Symbol tables, environment tracking
  - Current: Use association lists (O(n) lookup)
- ❌ **Vectors/Arrays** - Code generation buffers
  - Current: Have vectors but minimal API
- ❌ **Error handling** - Compilation errors
  - `error`, `catch`, `throw`, `handler-case`
  - Current: No error handling
- ❌ **Macros** - Code transformation
  - `defmacro`, quasiquote
  - Current: No macro system

##### Important (Makes Implementation Easier)
- ❌ **`format`** - String formatting for C code generation
- ❌ **`loop`** or better iteration - Process AST nodes
- ❌ **`case`/`typecase`** - Dispatch on node types
- ❌ **`defstruct`** - AST node definitions
- ❌ **Multiple return values** - Return (value, updated-env)
- ❌ **Package system** - Organize compiler modules

##### Nice-to-Have
- ❌ **CLOS** - Object-oriented AST nodes
- ❌ **Condition system** - Better error reporting
- ❌ **Reader macros** - Custom syntax

### 4.3 Estimated Implementation Effort

| Feature Category | Effort | Priority | Blocker? |
|-----------------|--------|----------|----------|
| File I/O | High | Critical | ✅ Yes |
| String manipulation | Medium | Critical | ✅ Yes |
| Hash tables | High | Critical | ✅ Yes |
| Error handling | High | Critical | ✅ Yes |
| Macros + quasiquote | Very High | Critical | ✅ Yes |
| Format | Medium | Important | ❌ No |
| Better iteration | Medium | Important | ❌ No |
| Structs | High | Important | ❌ No |
| Multiple values | Medium | Important | ❌ No |

### 4.4 Alternative: Partial Self-Hosting

Instead of full self-hosting, we could achieve **partial self-hosting**:

**Option A**: **Self-Interpreting REPL**
- ✅ Habu REPL can interpret itself (already possible!)
- ✅ Can load and run Habu Lisp programs
- ❌ Still need external compiler for C generation

**Option B**: **Bootstrapped Self-Hosting**
- ✅ Write simple "stage 1" compiler in Habu Lisp
- ✅ Use SBCL compiler to compile stage 1
- ✅ Use stage 1 to compile full "stage 2" compiler
- ✅ Stage 2 can compile itself

**Option C**: **Bytecode Interpreter**
- ✅ Write bytecode compiler in Habu Lisp
- ✅ Write bytecode VM in C (or Habu Lisp)
- ✅ Bytecode is easier to generate than C code
- ✅ Still self-hosting (compiles to bytecode)

---

## Part 5: Prioritized Implementation Roadmap

### Phase 1: Near-Term Enhancements (v1.1 - v1.3)
**Goal**: Improve usability and add common features
**Estimated Effort**: 2-4 weeks

#### v1.1 - Usability (1 week)
- ✅ Better error messages
- ✅ Multi-line input
- ✅ Command history improvements

#### v1.2 - Basic Language Extensions (1-2 weeks)
- ⭐ **Logical operators**: `and`, `or`, `not`
- ⭐ **More comparisons**: `<=`, `>=`, `!=`
- ⭐ **Sequential evaluation**: `progn` (expose existing primitive)
- ⭐ **Multi-way conditional**: `cond`
- ⭐ **Sequential binding**: `let*`

#### v1.3 - Variadic Functions (1 week)
- ⭐ **Variadic arithmetic**: `(+ 1 2 3 4)` → 10
- ⭐ **Variadic comparisons**: `(= 1 1 1)` → true
- ⭐ **Rest arguments**: `(defun sum (&rest args) ...)`

### Phase 2: Mid-Term Features (v2.0 - v2.5)
**Goal**: Add advanced features, move toward spec compliance
**Estimated Effort**: 2-3 months

#### v2.0 - Tail-Call Optimization (3-4 weeks)
- 🔴 **Critical for Scheme compliance**
- Trampoline or CPS transformation
- Enables deep recursion

#### v2.1 - Macros (4-6 weeks)
- 🔴 **Critical for self-hosting**
- `defmacro` - Macro definitions
- Quasiquote (`` ` ``, `,`, `,@`)
- `gensym` - Unique symbol generation
- Macro expansion phase

#### v2.2 - Data Types (2-3 weeks)
- Floating-point numbers (float type, parsing, arithmetic)
- Keywords (`:foo` notation)
- Characters (separate from fixnums)

#### v2.3 - Hash Tables (2 weeks)
- 🔴 **Critical for self-hosting**
- `make-hash-table`, `gethash`, `sethash`
- O(1) lookup for symbol tables

#### v2.4 - File I/O (2 weeks)
- 🔴 **Critical for self-hosting**
- `open`, `close`, `read-line`, `write-line`
- `with-open-file` macro
- Read/write text files

#### v2.5 - String Manipulation (1-2 weeks)
- 🔴 **Critical for self-hosting**
- `concatenate`, `subseq`
- `format` (simplified)
- String builders

### Phase 3: Self-Hosting (v3.0)
**Goal**: Write Habu compiler in Habu Lisp
**Estimated Effort**: 3-6 months

#### v3.0 - Self-Hosting Compiler (12+ weeks)
1. **Parser in Habu Lisp** (2 weeks)
   - Read Habu source files
   - Build AST
2. **C Code Generator in Habu Lisp** (4 weeks)
   - AST → C code
   - String building, formatting
3. **Compiler Infrastructure** (3 weeks)
   - Symbol tables (hash tables)
   - Error handling
   - Module system
4. **Bootstrap Process** (2 weeks)
   - Stage 1: SBCL compiles Habu compiler
   - Stage 2: Habu compiler compiles itself
5. **Testing & Validation** (1+ weeks)
   - Ensure self-compiled compiler works
   - Compile all REPL files
   - Performance benchmarking

### Phase 4: Advanced Features (v4.0+)
**Goal**: Production-ready, spec-compliant Lisp
**Estimated Effort**: 6-12 months

- Full Common Lisp or Scheme R7RS compliance
- CLOS or similar object system
- Condition system (error handling)
- Package system
- Bytecode compilation + VM
- Optimizing compiler
- Standard library expansion

---

## Part 6: Recommendations

### For Educational Use (Current State)
✅ **Current Habu REPL is perfect!**
- Minimal, understandable implementation
- All core features working
- Great for learning Lisp

**Recommendation**: Document current limitations, keep it simple.

### For Scheme Compliance (R5RS)
🔴 **Blocker**: Must implement tail-call optimization
**Priority Features**:
1. Tail-call optimization (REQUIRED)
2. `begin`, `set!`, internal definitions
3. Full numeric tower
4. Continuations (`call/cc`)

**Estimated Effort**: 3-4 months

### For Common Lisp Compliance (ANSI CL)
🟡 **Challenging**: CL spec is huge (>1000 pages)
**Priority Features**:
1. Macros + quasiquote (CRITICAL)
2. More data types (floats, hash tables, arrays)
3. Full standard library
4. Condition system

**Estimated Effort**: 12+ months

### For Self-Hosting
🔴 **Blockers**: File I/O, hash tables, macros, strings
**Recommended Path**: Partial self-hosting (Option B - Bootstrapped)

**Priority Features**:
1. File I/O (read source, write C code)
2. Hash tables (symbol tables)
3. String manipulation (C code generation)
4. Macros (code transformation)
5. Error handling

**Estimated Effort**: 6-9 months

### Pragmatic Approach
**Recommended**: Incremental improvements following roadmap
- Focus on v1.1-1.3 (usability, basic extensions)
- Add v2.0-2.5 features as needed (TCO, macros, data types)
- Consider self-hosting as long-term goal

**Don't Aim For**: Full CL/Scheme compliance
**Reason**: Habu's strength is simplicity and minimalism

---

## Part 7: Conclusion

### Current State: ✅ Complete Toy Lisp
Habu REPL is a **complete, working Lisp interpreter** with all core features. Perfect for:
- Learning Lisp programming
- Teaching language implementation
- Embedded scripting
- Rapid prototyping

### Spec Compliance: ❌ Not Compliant
- **Common Lisp**: ~20-30% compliant
- **Scheme R5RS**: ~30-40% compliant (no TCO!)
- **Educational Lisp**: 100% compliant ✅

### Self-Hosting: ❌ Not Achievable (Yet)
Missing 5 critical features:
1. File I/O
2. Hash tables
3. String manipulation
4. Macros
5. Error handling

**Estimated effort**: 6-9 months

### Recommendation: Incremental Enhancement
Follow the roadmap (v1.1 → v2.5 → v3.0) and add features as needed, prioritizing:
1. **Usability** (v1.1-1.3) - Immediate value
2. **TCO** (v2.0) - Enables deep recursion, required for Scheme
3. **Macros** (v2.1) - Enables metaprogramming, required for self-hosting
4. **Self-hosting infrastructure** (v2.2-2.5) - File I/O, hash tables, strings
5. **Self-hosting compiler** (v3.0) - Long-term goal

**Philosophy**: Keep it simple, minimal, and understandable. Don't sacrifice clarity for features.

---

**Document Version**: 1.0
**Last Updated**: November 20, 2024
**Author**: Gap Analysis for Habu REPL Project
