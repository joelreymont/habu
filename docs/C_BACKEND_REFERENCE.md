# Habu C Backend - Quick Reference

## Overview

The C backend translates Habu Lisp expressions to C code that calls the runtime library. This allows generating standalone executables without SBCL.

## Usage

### Basic Workflow

```lisp
;; Load the compiler and C backend
(load "bootstrap/compiler.lisp")
(load "bootstrap/c-backend.lisp")
(in-package :habu-compiler)

;; Generate C code
(generate-c-standalone
  '(+ (quote 40) (quote 2))
  :output-file "/tmp/answer.c")

;; Compile and run
(compile-and-run-c "/tmp/answer.c")
;; => Result: 42
```

### From Command Line

```bash
cd /path/to/habu
sbcl --load your-program.lisp
```

## Supported Features

### Data Types

| Habu Type | C Representation | Example |
|-----------|------------------|---------|
| Fixnum | `fixnum_to_value(n)` | `(quote 42)` |
| Cons | `habu_cons(car, cdr)` | `(cons 1 2)` |
| List | Nested cons | `(list 1 2 3)` |
| String | `habu_make_string(str, len)` | `"hello"` |
| Vector | `habu_make_vector(len)` | `(make-vector 10)` |
| NIL | `NIL` | `(quote ())` |

### Arithmetic

```lisp
(+ a b)   ; Addition
(- a b)   ; Subtraction
(* a b)   ; Multiplication
(/ a b)   ; Division
```

Generated C:
```c
fixnum_to_value(value_to_fixnum(a) + value_to_fixnum(b))
```

### Comparisons

```lisp
(= a b)   ; Equal
(< a b)   ; Less than
(> a b)   ; Greater than
(<= a b)  ; Less than or equal
(>= a b)  ; Greater than or equal
```

Generated C:
```c
(value_to_fixnum(a) == value_to_fixnum(b) ? fixnum_to_value(1) : NIL)
```

### List Operations

```lisp
(cons a b)           ; Create cons cell
(car list)           ; Get first element
(cdr list)           ; Get rest
(list a b c)         ; Create list
```

### Conditionals

#### If

```lisp
(if condition then-expr else-expr)
```

Generated C:
```c
(is_nil(condition) ? else_expr : then_expr)
```

#### Cond

```lisp
(cond
  ((< x 0) (quote -1))
  ((= x 0) (quote 0))
  (t (quote 1)))
```

Generated C:
```c
(is_nil(condition1) ?
  (is_nil(condition2) ?
    default_expr
    : expr2)
  : expr1)
```

### Variables

#### Let Bindings

```lisp
(let ((x 10) (y 20))
  (+ x y))
```

Generated C:
```c
({
    habu_value_t x = fixnum_to_value(10);
    habu_value_t y = fixnum_to_value(20);
    fixnum_to_value(value_to_fixnum(x) + value_to_fixnum(y));
})
```

#### Setq (Mutation)

```lisp
(setq x (+ x 1))
```

Generated C:
```c
(x = fixnum_to_value(value_to_fixnum(x) + 1))
```

### Control Flow

#### Progn

```lisp
(progn
  (print (quote 1))
  (print (quote 2))
  (quote done))
```

Generated C:
```c
({
  habu_println_value(fixnum_to_value(1));
  habu_println_value(fixnum_to_value(2));
  fixnum_to_value(done);
})
```

#### While Loop

```lisp
(let ((i 0))
  (while (< i 10)
    (progn
      (print i)
      (setq i (+ i 1)))))
```

Generated C:
```c
({
    habu_value_t i = fixnum_to_value(0);
    ({
      while (!is_nil(fixnum_to_value(value_to_fixnum(i) < 10))) {
        habu_println_value(i);
        i = fixnum_to_value(value_to_fixnum(i) + 1);
      }
      NIL;
    });
})
```

### Functions

```lisp
(defun square (x)
  (* x x))

(square (quote 5))
```

Generated C:
```c
habu_value_t square(habu_value_t x) {
    return fixnum_to_value(value_to_fixnum(x) * value_to_fixnum(x));
}

// In main:
habu_value_t result = square(fixnum_to_value(5));
```

**Features:**
- Recursive calls supported
- Multiple parameters
- Forward declarations generated automatically
- Symbol names sanitized (hyphens → underscores)

### Vectors

```lisp
(let ((v (make-vector 5)))
  (progn
    (vector-set v 0 (quote 42))
    (vector-ref v 0)))
```

Generated C:
```c
({
    habu_value_t v = habu_make_vector(5);
    ({
      habu_vector_set(v, 0, fixnum_to_value(42));
      habu_vector_ref(v, 0);
    });
})
```

### I/O Operations

#### Print

```lisp
(print (quote 42))
```

Generated C:
```c
habu_println_value(fixnum_to_value(42))
```

#### File Operations

```lisp
;; Write file
(write-file "/tmp/test.txt" "Hello")

;; Read file
(read-file "/tmp/test.txt")
```

Generated C:
```c
habu_write_file(habu_make_string("/tmp/test.txt", 13),
                habu_make_string("Hello", 5))

habu_read_file(habu_make_string("/tmp/test.txt", 13))
```

## Symbol Name Sanitization

Lisp symbols with hyphens are converted to C identifiers with underscores:

| Lisp Name | C Name |
|-----------|--------|
| `my-function` | `my_function` |
| `sum-of-squares` | `sum_of_squares` |
| `x-coord` | `x_coord` |

## Generated Program Structure

```c
#include "habu.h"
#include "object.h"
#include <stdio.h>

// Forward declarations
habu_value_t my_function(habu_value_t);

// Function definitions
habu_value_t my_function(habu_value_t x) {
    return /* ... */;
}

int main(void) {
    habu_init(4 * 1024 * 1024);  // Initialize runtime

    habu_value_t result = /* user code */;

    // Print result
    if (is_fixnum(result)) {
        printf("Result: %lld\n", (long long)value_to_fixnum(result));
    } else {
        printf("Result: 0x%llx (tagged pointer)\n", (unsigned long long)result);
    }

    habu_shutdown();  // Cleanup
    return 0;
}
```

## Compilation

The C backend automatically compiles generated code:

```lisp
(compile-and-run-c "/tmp/program.c")
```

This runs:
```bash
clang -O2 -I runtime \
      /tmp/program.c \
      runtime/gc.o \
      runtime/runtime.o \
      runtime/region.o \
      runtime/io.o \
      -o program.out
./program.out
```

## Common Patterns

### Accumulator Pattern

```lisp
(let ((sum 0)
      (i 1))
  (while (<= i 10)
    (progn
      (setq sum (+ sum i))
      (setq i (+ i 1))))
  sum)
```

### Recursive Function

```lisp
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```

### Vector-Based Algorithm

```lisp
(let ((v (make-vector 10)))
  (progn
    ;; Initialize
    (let ((i 0))
      (while (< i 10)
        (progn
          (vector-set v i (* i 2))
          (setq i (+ i 1)))))
    ;; Use results
    (vector-ref v 5)))
```

### File Processing

```lisp
(let ((data (read-file "/tmp/input.txt")))
  (progn
    (print data)
    (write-file "/tmp/output.txt" data)
    (quote done)))
```

### Lambdas and Closures

#### Simple Lambda

```lisp
(let ((double (lambda (x) (* x (quote 2)))))
  (funcall double (quote 21)))  ; => 42
```

Generated C:
```c
habu_value_t lambda_1(habu_value_t X, habu_value_t env) {
    return fixnum_to_value(value_to_fixnum(X) * 2);
}

// In main:
habu_value_t double = habu_make_closure((void*)lambda_1, NIL);
// Call via funcall...
```

#### Closure with Variable Capture

```lisp
(let ((make-adder (lambda (n) (lambda (x) (+ x n)))))
  (let ((add5 (funcall make-adder (quote 5))))
    (funcall add5 (quote 10))))  ; => 15
```

Generated C:
```c
habu_value_t lambda_1(habu_value_t N, habu_value_t env) {
    // Capture N in environment vector
    habu_value_t env = habu_make_vector(fixnum_to_value(1));
    habu_vector_set(env, 0, N);
    return habu_make_closure((void*)lambda_2, env);
}

habu_value_t lambda_2(habu_value_t X, habu_value_t env) {
    // Extract captured N from environment
    habu_value_t N = habu_vector_ref(env, 0);
    return fixnum_to_value(value_to_fixnum(X) + value_to_fixnum(N));
}
```

**Features:**
- First-class functions via closures
- Automatic variable capture detection
- Environment vectors for captured variables
- Nested lambdas supported
- Higher-order functions (functions returning functions)

**Funcall:**
```lisp
(funcall fn arg1 arg2 ...)
```
Calls a closure with arguments. Works with both simple lambdas and closures with captures.

## Limitations

### Not Supported

- ❌ Macros (expand in SBCL first)
- ❌ First-class symbols
- ❌ Hash tables (yet)
- ❌ Multiple return values
- ❌ Exceptions/error handling

### C-Specific Limitations

- Requires GCC/Clang (uses statement expressions)
- Binary includes full runtime (~50 KB)
- Compilation slower than interpretation
- No REPL (batch mode only)

## Tips & Best Practices

### 1. Quote Your Literals

```lisp
;; Wrong
(+ 1 2)  ; Error: 1 and 2 are not expressions

;; Right
(+ (quote 1) (quote 2))
```

### 2. Use Progn for Sequences

```lisp
(let ((x 0))
  (progn  ; Multiple expressions
    (setq x 10)
    (print x)
    x))
```

### 3. Initialize Variables

```lisp
(let ((result (quote 0)))  ; Always initialize
  (setq result (+ result (quote 1))))
```

### 4. Use While for Loops

```lisp
(let ((i (quote 0)))
  (while (< i (quote 10))
    (progn
      (print i)
      (setq i (+ i (quote 1))))))
```

### 5. Prefer Cond Over Nested Ifs

```lisp
;; Better
(cond
  ((< x 0) (quote negative))
  ((= x 0) (quote zero))
  (t (quote positive)))

;; Worse
(if (< x 0)
    (quote negative)
    (if (= x 0)
        (quote zero)
        (quote positive)))
```

## Debugging

### Check Generated C Code

```lisp
(generate-c-standalone expr :output-file "/tmp/debug.c")
;; Then examine /tmp/debug.c
```

### Common Issues

**Problem:** `Unsupported expression for C backend`
**Solution:** Check if feature is implemented (see Limitations)

**Problem:** C compilation error
**Solution:** Check for invalid identifiers (reserved C keywords)

**Problem:** Wrong result
**Solution:** Verify operator precedence and type conversions

### Verbose Output

```lisp
;; The C backend prints:
;; - Generated C file path
;; - Compilation command
;; - Execution output
```

## Examples Repository

See test files in `bootstrap/`:
- `test-c-backend.lisp` - Basic features
- `test-defun.lisp` - Function definitions
- `test-control.lisp` - Loops and mutation
- `test-advanced.lisp` - Complex programs

## Further Reading

- `STANDALONE_MODE.md` - Complete guide to standalone operation
- `GC_RUNTIME.md` - Garbage collector details
- `PROGRESS_REPORT.md` - Current status and roadmap
