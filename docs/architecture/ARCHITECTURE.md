# Habu REPL - Architecture Overview

This document explains the architecture of the Habu REPL implementation, showing how a complete Lisp interpreter is built with minimal C runtime support.

## High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        User Input                            │
│                    (Lisp expressions)                        │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                      REPL Loop                               │
│                    (recursive-repl.lisp)                     │
│  • Read line from terminal                                   │
│  • Parse into S-expression (Reader)                          │
│  • Evaluate expression (Evaluator)                           │
│  • Print result                                              │
│  • Loop with environment threading                           │
└────────────────────────┬────────────────────────────────────┘
                         │
         ┌───────────────┴───────────────┐
         │                               │
         ▼                               ▼
┌──────────────────┐          ┌──────────────────────┐
│     Reader       │          │     Evaluator        │
│  (Pure Lisp)     │          │   (Pure Lisp)        │
│                  │          │                      │
│ • Tokenization   │          │ • Special forms      │
│ • Number parsing │          │ • Function calls     │
│ • Symbol parsing │          │ • Environment lookup │
│ • List parsing   │          │ • Closure creation   │
│ • Quote syntax   │          │ • Recursion          │
└────────┬─────────┘          └──────────┬───────────┘
         │                               │
         └───────────────┬───────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                     Environment                              │
│                    (Pure Lisp)                               │
│  • Association lists: ((sym . val) ...)                      │
│  • Lookup: Linear search                                     │
│  • Extend: Cons new binding                                  │
│  • Merge: Combine environments for recursion                 │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│                   C Runtime (Minimal)                        │
│                   runtime/*.c, runtime/*.h                   │
│  • Memory: GC, cons, make-vector, make-string                │
│  • Access: car, cdr, get-tag                                 │
│  • Arithmetic: +, -, *, /                                    │
│  • I/O: print-value, readline                                │
│  • ADDED: make-string-from-vector (for reader)               │
└─────────────────────────────────────────────────────────────┘
```

## Component Breakdown

### 1. REPL Loop

**File**: `recursive-repl.lisp` (top-level)

**Responsibilities**:
- Read input from terminal
- Call reader to parse
- Call evaluator to evaluate
- Print result
- Thread environment through iterations

**Key Function**:
```lisp
(defun repl-loop (env)
  (let ((line (readline "habu> ")))
    (if line
      (let ((expr (read-str (make-string-from-cstr line))))
        (let ((result-env (eval-toplevel expr env)))
          (let ((result (car result-env))
                (new-env (cdr result-env)))
            (progn
              (print-value result)
              (println)
              (repl-loop new-env)))))  ; Thread environment!
      ...)))
```

**Environment Threading**: The environment is passed through each iteration, preserving function definitions across REPL interactions.

### 2. Reader

**Location**: Integrated into each `*-repl.lisp` file

**Responsibilities**:
- Convert character stream to S-expressions
- Parse numbers, symbols, lists
- Handle quote syntax (`'expr`)
- Skip whitespace and comments

**Architecture**:
```
Input: "(+ 1 2)"
  │
  ▼
Tokenization → ['(', '+', '1', '2', ')']
  │
  ▼
Parsing → (+ 1 2)
  │
  ▼
S-expression
```

**Key Functions**:
```lisp
read-str          ; Entry point
  ├─ skip-whitespace
  ├─ read-expr     ; Main parser
  │   ├─ read-number
  │   ├─ read-symbol
  │   ├─ read-list
  │   └─ handle-quote
  └─ make-sym-from-chars  ; Symbol construction
```

**Implementation Details**:
- **Numbers**: Parse digit by digit, build integer
- **Symbols**: Parse alpha/operator chars, build symbol
- **Lists**: Recursive descent, handle nested structures
- **Quote**: Transform `'x` to `(quote x)`

### 3. Evaluator

**Location**: Integrated into each `*-repl.lisp` file

**Responsibilities**:
- Evaluate S-expressions
- Handle special forms (quote, if, let, lambda, defun)
- Apply functions
- Manage environment

**Evaluation Rules**:

```
eval(number, env) = number
eval(symbol, env) = env-lookup(symbol, env)
eval((quote x), env) = x
eval((if test then else), env) =
    eval(test, env) ? eval(then, env) : eval(else, env)
eval((let ((x v) ...) body), env) =
    eval(body, extend(env, x, eval(v, env), ...))
eval((lambda (x ...) body), env) =
    (closure env (x ...) body)
eval((defun name (x ...) body), env) =
    extend(env, name, (closure env (x ...) body))
eval((f arg ...), env) =
    apply(eval(f, env), map(lambda (a) eval(a, env), args))
```

**Special Forms**:
```lisp
eval-expr
  ├─ quote      ; Literal data
  ├─ if         ; Conditional
  ├─ let        ; Local bindings
  ├─ lambda     ; Create closure
  ├─ defun      ; Define function
  ├─ operators  ; +, -, *, /, =, <, >
  └─ apply      ; Function application
```

**Function Application**:
```lisp
(defun apply-lambda (closure arg-vals current-env)
  (let ((closure-env (car (cdr closure))))
    (let ((params (car (cdr (cdr closure)))))
      (let ((body (car (cdr (cdr (cdr closure))))))
        ; KEY: Merge current env with closure env for recursion
        (let ((combined-env (append-env current-env closure-env)))
          (let ((new-env (env-extend-list params arg-vals combined-env)))
            (eval-expr body new-env)))))))
```

### 4. Environment

**Representation**: Association list
```lisp
((symbol1 . value1)
 (symbol2 . value2)
 ...)
```

**Operations**:
```lisp
; Lookup symbol
(defun env-lookup (sym env)
  (if (nil? env) nil
    (let ((binding (car env)))
      (if (symbol=? sym (car binding))
        (cdr binding)
        (env-lookup sym (cdr env))))))

; Extend with single binding
(defun env-extend (sym val env)
  (cons (cons sym val) env))

; Extend with multiple bindings
(defun env-extend-list (syms vals env)
  (if (nil? syms) env
    (env-extend-list (cdr syms) (cdr vals)
                     (env-extend (car syms) (car vals) env))))

; Merge environments (for recursion)
(defun append-env (env1 env2)
  (if (nil? env1) env2
    (cons (car env1) (append-env (cdr env1) env2))))
```

**Scoping**:
- **Lexical**: Local variables (let, lambda parameters)
- **Dynamic**: Top-level functions (defun)

### 5. Closures

**Representation**:
```lisp
(closure environment parameters body)
```

**Example**:
```lisp
(let ((x 10))
  (lambda (y) (+ x y)))

; Creates:
(closure ((x . 10)) (y) (+ x y))
```

**Closure Capture**: Environment at lambda creation is captured and used during application.

### 6. C Runtime

**Minimal Primitives**:

```c
// Memory management
cons(car, cdr)           // Create pair
car(pair)                // Get first
cdr(pair)                // Get rest
make_vector(size)        // Create vector
make_string(chars, len)  // Create string
make_symbol(string)      // Create symbol

// Type inspection
get_tag(value)           // Get type tag (0-5)

// Arithmetic
+ - * /                  // Basic arithmetic

// I/O
print_value(value)       // Print value
println()                // Print newline
readline(prompt)         // Read line from terminal

// ADDED for REPL
make_string_from_vector(vec)  // Convert char vector to string
```

**Type Tags**:
```c
TAG_FIXNUM  = 0x0  // Immediate integer
TAG_CONS    = 0x1  // Cons cell (pair)
TAG_SYMBOL  = 0x2  // Symbol
TAG_VECTOR  = 0x3  // Vector
TAG_STRING  = 0x4  // String
TAG_CLOSURE = 0x5  // Closure (for print)
```

## Data Flow Examples

### Example 1: Simple Evaluation

```
Input: "(+ 2 3)"
  │
  ▼ Reader
(+ 2 3)
  │
  ▼ Evaluator
eval((+ 2 3), env)
  ├─ eval(+, env) → <primitive-op>
  ├─ eval(2, env) → 2
  └─ eval(3, env) → 3
  │
  ▼ Apply operator
5
  │
  ▼ Print
"5"
```

### Example 2: Function Definition and Call

```
Input: "(defun square (x) (* x x))"
  │
  ▼ Reader
(defun square (x) (* x x))
  │
  ▼ Evaluator (eval-toplevel)
Create closure: (closure env (x) (* x x))
Extend env: ((square . (closure ...)) . old-env)
Return: (result . new-env)
  │
  ▼ REPL continues with new-env

Input: "(square 5)"
  │
  ▼ Reader
(square 5)
  │
  ▼ Evaluator
eval((square 5), env)
  ├─ eval(square, env) → (closure ...)
  ├─ eval(5, env) → 5
  └─ apply-lambda(closure, [5], env)
      ├─ Merge env with closure-env
      ├─ Extend with (x . 5)
      └─ eval((* x x), new-env) → 25
  │
  ▼ Print
"25"
```

### Example 3: Recursion

```
Input: "(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))"
  │
  ▼ Create closure, add to env
env = ((fact . (closure env (n) ...)) . old-env)

Input: "(fact 5)"
  │
  ▼ eval((fact 5), env)
  ├─ lookup 'fact' → (closure old-env (n) ...)
  ├─ apply-lambda
  │   ├─ Merge current-env with closure-env
  │   │   → This gives access to 'fact' inside body!
  │   ├─ Extend with (n . 5)
  │   └─ eval body:
  │       eval((if (= n 0) 1 (* n (fact (- n 1)))), env')
  │       ├─ (= 5 0) → nil
  │       └─ eval((* 5 (fact 4)), env')
  │           └─ Recursive call to 'fact'
  │               (finds 'fact' in merged env!)
  └─ ... continues recursively
```

**Key Insight**: Environment merging in `apply-lambda` is what enables recursion. The current global environment (which includes the function being called) is merged with the closure's captured environment.

## Progressive Enhancement

The three REPLs show progressive feature addition:

### Enhanced REPL (235 lines)
```
Components:
  ✓ Reader (numbers, symbols, lists, quote)
  ✓ Basic Evaluator (quote, if, operators)
  ✓ No environment (global only)
  ✓ No functions

Features:
  ✓ Arithmetic
  ✓ Lists
  ✓ Conditionals
```

### Programmable REPL (282 lines)
```
Enhanced REPL +
  ✓ Environment (association lists)
  ✓ Let expressions
  ✓ Lambda expressions
  ✓ Closures
  ✓ Function application

Features:
  ✓ All Enhanced features
  ✓ Local variables
  ✓ Anonymous functions
  ✓ Higher-order functions
```

### Recursive REPL (320 lines)
```
Programmable REPL +
  ✓ Defun support
  ✓ Environment threading through REPL
  ✓ Environment merging for recursion
  ✓ Comparison operators (=, <, >)

Features:
  ✓ All Programmable features
  ✓ Named functions
  ✓ Full recursion
  ✓ Persistent definitions
  → COMPLETE LISP!
```

## Compilation Pipeline

```
┌─────────────────────┐
│ Lisp Source         │
│ (recursive-repl.lisp)│
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ SBCL Reader         │
│ (Parses Lisp syntax)│
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ Habu Compiler       │
│ (compiler.lisp)     │
│ • Transforms to IR  │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ C Backend           │
│ (c-backend.lisp)    │
│ • Generates C code  │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ C Source            │
│ (habu-rec.c)        │
│ ~19KB               │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ GCC/Clang           │
│ (C Compiler)        │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│ Native Executable   │
│ (habu-rec)          │
│ 73KB                │
└─────────────────────┘
```

## Memory Layout

```
Heap (4MB default):
┌────────────────────────────────────────┐
│ Cons Cells:                            │
│   [car | cdr] [car | cdr] ...          │
│                                        │
│ Vectors:                               │
│   [length | data...] ...               │
│                                        │
│ Strings:                               │
│   [length | chars...] ...              │
│                                        │
│ Symbols:                               │
│   [string-ptr] ...                     │
│                                        │
│ Closures (as lists):                   │
│   (closure env params body)            │
│   = nested cons cells                  │
└────────────────────────────────────────┘

Stack:
┌────────────────────────────────────────┐
│ C call stack for evaluation            │
│ (grows down, ~1MB typical)             │
│                                        │
│ Recursion depth limited by stack size  │
│ (~1000-10000 calls depending on OS)    │
└────────────────────────────────────────┘
```

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Notes |
|-----------|-----------|--------|
| cons, car, cdr | O(1) | Direct pointer ops |
| env-lookup | O(n) | Linear search through env |
| eval number/symbol | O(1) or O(n) | Constant or env lookup |
| eval list | O(n * m) | n = list length, m = eval cost per element |
| GC | O(heap) | Mark and sweep entire heap |

### Space Complexity

| Structure | Size | Notes |
|-----------|------|-------|
| Fixnum | 0 bytes | Immediate (in pointer) |
| Cons cell | 16 bytes | Two 8-byte pointers |
| Vector | 8 + 8*len | Length + elements |
| String | 8 + len | Length + characters |
| Symbol | 16 bytes | Pointer to string |
| Closure | ~48 bytes | Nested cons cells |

### Bottlenecks

1. **Environment lookup**: Linear search, O(n) where n = number of bindings
2. **GC**: Stop-the-world, scans entire heap
3. **No tail-call optimization**: Deep recursion uses stack
4. **Interpretation**: No bytecode compilation, tree-walking interpreter

## Design Decisions

### Why Association Lists for Environment?

**Pros**:
- Simple to implement
- Functional (immutable)
- Easy to extend and merge
- Clear semantics

**Cons**:
- O(n) lookup time
- More cons cells than hash table

**Choice**: Simplicity and clarity over performance. For learning and small programs, this is fine.

### Why No Tail-Call Optimization?

**Reason**: Would require significant C runtime changes (trampoline or CPS transformation).

**Impact**: Deep recursion (~1000+ calls) may overflow stack.

**Mitigation**: Use accumulator patterns to reduce depth.

### Why Only Integers?

**Reason**: Simplifies implementation significantly. No need for:
- Type discrimination (int vs float)
- Different arithmetic operations
- Float printing/parsing

**Impact**: Limited to integer mathematics.

**Benefit**: Smaller, simpler implementation.

### Why Environment Merging for Recursion?

**Problem**: When defun creates closure, function doesn't exist yet in environment.

**Solution**: At application time, merge current global environment (which now contains the function) with closure's captured environment.

**Result**: Recursive functions can find themselves during execution.

## Comparison with Other Architectures

### vs. Bytecode Interpreter

| Habu REPL | Bytecode Interpreter |
|-----------|---------------------|
| Tree-walking | Bytecode execution |
| No compilation phase | Compile to bytecode first |
| Simpler implementation | More complex |
| Slower execution | Faster execution |
| Smaller code size | Larger code size |

### vs. JIT Compiler

| Habu REPL | JIT Compiler |
|-----------|--------------|
| Pure interpretation | Dynamic compilation |
| No optimization | Heavy optimization |
| Simple | Very complex |
| Predictable performance | Variable warmup |
| ~100 lines | ~10,000+ lines |

### vs. Compiler to Native

| Habu REPL | Native Compiler |
|-----------|----------------|
| Interpreted | Compiled |
| Instant startup | Compilation delay |
| Interactive | Batch |
| Simple | Complex |
| Educational | Production |

## Extensibility

### Adding Operators

**Easy** - Just add cases to evaluator:
```lisp
(if (symbol=? first (make-symbol (quote "modulo")))
  (- (car args) (* (car (cdr args)) (/ (car args) (car (cdr args)))))
  ...)
```

### Adding Special Forms

**Moderate** - Add parsing and evaluation:
```lisp
(if (symbol=? first (make-symbol (quote "cond")))
  (eval-cond (cdr expr) env)
  ...)
```

### Adding C Primitives

**Avoid** - Breaks minimal runtime philosophy. Only add if absolutely necessary.

## Limitations

1. **No tail-call optimization** - Deep recursion limited
2. **No macros** - No code transformation
3. **Integers only** - No floating-point
4. **O(n) environment lookup** - Slow for many bindings
5. **No error handling** - Minimal error messages
6. **No module system** - Single namespace
7. **No string operations** - Strings only for symbols
8. **No file I/O** - REPL only

## Future Enhancements

See `REPL_FINAL_STATUS.md` for comprehensive list of possible enhancements.

---

**Summary**: Habu REPL demonstrates that a complete, working Lisp interpreter can be built in 320 lines of pure Lisp with minimal C runtime support (only 1 primitive added). The architecture prioritizes simplicity, clarity, and educational value over performance.

