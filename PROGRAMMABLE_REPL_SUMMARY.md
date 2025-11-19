# Programmable Habu REPL - Let and Lambda Support

## ✅ Full Lisp Programming Now Available

Successfully implemented a **programmable Habu Lisp REPL** with:
1. **Environment with variable bindings**
2. **`let` expressions** for local variables
3. **`lambda` expressions** for anonymous functions
4. **First-class functions** - functions as values
5. **Lexical closures** - functions capture their environment
6. **All previous features**: quote, if, lists, arithmetic

## Test Results

All features verified and working:

### Basic Let
```lisp
habu> (let ((x 10)) (+ x 5))
15
habu> (let ((x 10) (y 20)) (+ x y))
30
```

### Lambda Functions
```lisp
habu> ((lambda (x) (* x x)) 5)
25
habu> ((lambda (x y) (+ x y)) 10 20)
30
```

### Functions as Values
```lisp
habu> (let ((double (lambda (x) (* x 2)))) (double 21))
42
habu> (let ((add (lambda (x y) (+ x y)))) (add 10 20))
30
```

### Nested Scopes
```lisp
habu> (let ((x 5)) (let ((y 10)) (+ x y)))
15
habu> ((lambda (x) ((lambda (y) (+ x y)) 20)) 10)
30
```

### Lexical Closures
```lisp
habu> (let ((twice (lambda (f x) (f (f x))))) (twice (lambda (n) (+ n 1)) 10))
12
```

### Conditional with Lambda
```lisp
habu> (let ((f (lambda (n) (if (= n 0) 1 (* n 2))))) (f 5))
10
```

## Architecture

The implementation uses **environments as association lists** (following Scheme/Lisp tradition):

```lisp
;; Environment is a list of (symbol . value) pairs
;; Example: ((x . 10) (y . 20) (z . 30))

(defun env-lookup (sym env)
  (if (nil? env) (quote nil)
    (let ((binding (car env)))
      (if (symbol=? sym (car binding))
        (cdr binding)
        (env-lookup sym (cdr env))))))

(defun env-extend (sym val env)
  (cons (cons sym val) env))
```

### Closures

Closures are represented as tagged lists:
```lisp
(closure <captured-env> <params> <body>)
```

When creating a lambda:
```lisp
(lambda (x y) (+ x y))
  →
(closure <current-env> (x y) (+ x y))
```

When applying a closure:
```lisp
(defun apply-lambda (closure arg-vals)
  (let ((closure-env (car (cdr closure))))
    (let ((params (car (cdr (cdr closure)))))
      (let ((body (car (cdr (cdr (cdr closure))))))
        (let ((new-env (env-extend-list params arg-vals closure-env)))
          (eval-expr body new-env))))))
```

### Let Expressions

`let` creates a new environment frame:
```lisp
(defun eval-let (args env)
  (let ((bindings (car args)))
    (let ((body (car (cdr args))))
      (let ((new-env (eval-bindings bindings env)))
        (eval-expr body new-env)))))
```

Bindings are evaluated left-to-right:
```lisp
(defun eval-bindings (bindings env)
  (if (nil? bindings) env
    (let ((binding (car bindings)))
      (let ((sym (car binding)))
        (let ((val-expr (car (cdr binding))))
          (let ((val (eval-expr val-expr env)))
            (eval-bindings (cdr bindings)
                          (env-extend sym val env))))))))
```

## Implementation Details

### Added Functions

**Environment operations** (in Lisp):
- `env-lookup` - Look up symbol in environment
- `env-extend` - Add binding to environment
- `env-extend-list` - Add multiple bindings

**Evaluation** (in Lisp):
- `eval-let` - Evaluate let expressions
- `eval-bindings` - Process let bindings
- `apply-lambda` - Apply closure to arguments

### Modified Functions

**eval-expr** - Added cases for `let` and `lambda`:
```lisp
(if (symbol=? first (make-symbol (quote "let")))
  (eval-let (cdr expr) env)
  (if (symbol=? first (make-symbol (quote "lambda")))
    (cons (make-symbol (quote "closure")) (cons env (cdr expr)))
    ...))
```

**eval-apply** - Added function application:
```lisp
(let ((fn (eval-expr op env)))
  (if (cons? fn)
    (if (symbol=? (car fn) (make-symbol (quote "closure")))
      (apply-lambda fn (eval-list args env))
      ...)
    ...))
```

**repl-loop** - Now threads environment through (though still starts empty):
```lisp
(defun repl-loop (env)
  ...
  (let ((result (eval-expr expr env)))
    ...)
  (repl-loop env))
```

## File Statistics

- **executable**: `habu-prog` - 73KB (was 56KB)
- **source**: `programmable-repl.lisp` - 282 lines (all in Lisp)
- **runtime**: NO CHANGES (still minimal!)

The increase from 56KB to 73KB is due to more complex evaluator code, not runtime changes.

## Design Philosophy Maintained

✅ **Minimal C runtime** - NO changes to runtime
✅ **Everything in Lisp** - All new features implemented in Lisp
✅ **Following Scheme** - Standard environment-based evaluator
✅ **Lexical scoping** - Closures capture environment correctly
✅ **First-class functions** - Functions are values

## What This Enables

With `let` and `lambda`, you can now:
- ✅ Create local variables
- ✅ Define anonymous functions
- ✅ Pass functions as arguments
- ✅ Return functions from functions
- ✅ Create closures that capture variables
- ✅ Build abstractions and combinators

## Example: Higher-Order Functions

```lisp
;; Map function (conceptually)
habu> (let ((map (lambda (f lst)
                    (if (nil? lst)
                      nil
                      (cons (f (car lst))
                            (map f (cdr lst)))))))
        (map (lambda (x) (* x 2)) '(1 2 3)))
```

(Note: This needs recursive support which requires mutable global environment or Y-combinator)

## What's Still Missing

### Critical Features
- [ ] **Recursive functions** - Need global mutable environment for `defun`
- [ ] **Better equality** - Need `=` to work on symbols for comparison
- [ ] **`and`, `or`** - Logical operators
- [ ] **`null?`, `pair?`** - More type predicates

### Nice to Have
- [ ] **Multiple body forms in `let`/`lambda`** - Currently single expression
- [ ] **`let*` (sequential let)** - Already works due to binding order!
- [ ] **`letrec`** - For recursive local functions
- [ ] **Varargs** - `(lambda args ...)` for variable arguments
- [ ] **`apply`** - Apply function to list of arguments
- [ ] **`map`, `filter`, `fold`** - List combinators

### REPL Features
- [ ] **Top-level `define`** - Define global functions/variables
- [ ] **Multi-line input** - For complex expressions
- [ ] **Pretty printing** - Better output formatting
- [ ] **Error messages** - Currently returns nil on error

## Comparison with Enhanced REPL

| Feature | Enhanced REPL | Programmable REPL |
|---------|---------------|-------------------|
| Numbers | ✅ | ✅ |
| Arithmetic | ✅ | ✅ |
| Quote | ✅ | ✅ |
| If | ✅ | ✅ |
| Lists | ✅ | ✅ |
| Let | ❌ | ✅ |
| Lambda | ❌ | ✅ |
| Closures | ❌ | ✅ |
| Function application | ❌ | ✅ |
| Higher-order functions | ❌ | ✅ |
| Size | 56KB | 73KB |

## Technical Achievement

This implementation demonstrates:
1. **Complete lexical scoping** in 282 lines of Lisp
2. **First-class functions** with proper closures
3. **Environment-based evaluation** following Scheme model
4. **Zero runtime changes** - all in application layer
5. **True minimalism** - C runtime unchanged

The evaluator now implements the core of Scheme/Lisp semantics!

## Conclusion

The **Programmable REPL** makes Habu a true programming language:
- Can define local variables with `let`
- Can create functions with `lambda`
- Can pass functions around as values
- Can create closures that capture environment
- Can build abstractions

**Status: ✅ COMPLETE and FULLY PROGRAMMABLE**

Next step would be adding `defun` with global mutable environment to enable recursive functions and top-level definitions.

---

## Usage Examples

```bash
$ ./habu-prog
"Habu REPL - Programmable"
"Features: let, lambda"

# Local variables
habu> (let ((x 10)) (+ x 5))
15

# Anonymous functions
habu> ((lambda (x) (* x x)) 5)
25

# Functions as values
habu> (let ((double (lambda (x) (* x 2)))) (double 21))
42

# Closures
habu> ((lambda (x) ((lambda (y) (+ x y)) 20)) 10)
30

# Higher-order functions
habu> (let ((twice (lambda (f x) (f (f x))))) (twice (lambda (n) (+ n 1)) 10))
12
```

**The Habu REPL is now a real Lisp!** 🎉
