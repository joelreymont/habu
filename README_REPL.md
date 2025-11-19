# Habu Lisp REPL - Quick Start Guide

## What is Habu Lisp?

Habu is a **complete, working Lisp interpreter** in under 75KB with all core language features implemented in pure Lisp with a minimal C runtime. Perfect for learning, experimentation, and embedded use.

## Quick Start

### Running the REPL

```bash
# Complete Lisp with all features
./habu-rec

# You'll see:
"Habu REPL - Recursive"
"Features: let, lambda, defun"
habu> _
```

### Your First Expressions

```lisp
# Simple arithmetic
habu> (+ 2 3)
5

# Lists
habu> '(1 2 3)
(1 2 3)

# Car and cdr
habu> (car '(1 2 3))
1

habu> (cdr '(1 2 3))
(2 3)

# Conditionals
habu> (if (> 5 3) 'yes 'no)
<symbol>
```

### Defining Functions

```lisp
# Simple function
habu> (defun square (x) (* x x))
<symbol>

habu> (square 5)
25

# Recursive function
habu> (defun factorial (n)
        (if (= n 0) 1
          (* n (factorial (- n 1)))))
<symbol>

habu> (factorial 5)
120
```

### Anonymous Functions (Lambda)

```lisp
# Basic lambda
habu> ((lambda (x) (* x x)) 5)
25

# Lambda with multiple arguments
habu> ((lambda (x y) (+ x y)) 10 20)
30

# Storing lambdas in variables (using let)
habu> (let ((double (lambda (x) (* x 2))))
        (double 21))
42
```

### Local Variables (Let)

```lisp
# Single binding
habu> (let ((x 10))
        (+ x 5))
15

# Multiple bindings
habu> (let ((x 10) (y 20))
        (+ x y))
30

# Nested let
habu> (let ((x 5))
        (let ((y 10))
          (+ x y)))
15
```

## Common Patterns

### Higher-Order Functions

```lisp
# Function that takes a function
habu> (defun twice (f x)
        (f (f x)))
<symbol>

habu> (defun add1 (n) (+ n 1))
<symbol>

habu> (twice add1 10)
12

# Composition
habu> (defun compose (f g)
        (lambda (x) (f (g x))))
<symbol>

habu> (let ((double (lambda (x) (* x 2)))
            (add1 (lambda (x) (+ x 1))))
        (let ((f (compose double add1)))
          (f 5)))
12
```

### List Processing

```lisp
# Map - apply function to each element
habu> (defun map (f lst)
        (if (= lst 0) nil
          (cons (f (car lst))
                (map f (cdr lst)))))
<symbol>

habu> (map (lambda (x) (* x x)) '(1 2 3 4 5))
(1 4 9 16 25)

# Filter - select elements
habu> (defun filter (pred lst)
        (if (= lst 0) nil
          (if (pred (car lst))
            (cons (car lst) (filter pred (cdr lst)))
            (filter pred (cdr lst)))))
<symbol>

habu> (filter (lambda (x) (> x 0)) '(-2 -1 0 1 2))
(1 2)

# Fold/Reduce - accumulate
habu> (defun fold (f init lst)
        (if (= lst 0) init
          (fold f (f init (car lst)) (cdr lst))))
<symbol>

habu> (fold + 0 '(1 2 3 4 5))
15
```

### Recursive Algorithms

```lisp
# Fibonacci
habu> (defun fib (n)
        (if (< n 2) n
          (+ (fib (- n 1)) (fib (- n 2)))))
<symbol>

habu> (fib 10)
55

# Sum of list
habu> (defun sum (lst)
        (if (= lst 0) 0
          (+ (car lst) (sum (cdr lst)))))
<symbol>

habu> (sum '(1 2 3 4 5))
15

# List length
habu> (defun length (lst)
        (if (= lst 0) 0
          (+ 1 (length (cdr lst)))))
<symbol>

habu> (length '(1 2 3 4 5))
5
```

## Available Operators

### Arithmetic
- `+` - Addition
- `-` - Subtraction
- `*` - Multiplication
- `/` - Division

### Comparison (return 1 for true, nil for false)
- `=` - Equality
- `<` - Less than
- `>` - Greater than

### List Operations
- `cons` - Construct pair
- `car` - First element
- `cdr` - Rest of list
- `list` - Create list from arguments

### Special Forms
- `quote` (or `'`) - Literal data
- `if` - Conditional
- `let` - Local bindings
- `lambda` - Anonymous function
- `defun` - Define function

## Loading the Standard Library

If you have the `stdlib.lisp` file, you can manually copy and paste functions into the REPL, or define them at the start of your session.

```lisp
# Define commonly used utilities first
habu> (defun not (x) (if x nil 1))
<symbol>

habu> (defun null? (x) (= x 0))
<symbol>

habu> (defun map (f lst) ...)
<symbol>

# Then use them
habu> (map square '(1 2 3))
(1 4 9)
```

## Example Programs

### Computing Powers

```lisp
habu> (defun power (base exp)
        (if (= exp 0) 1
          (* base (power base (- exp 1)))))
<symbol>

habu> (power 2 10)
1024
```

### Reversing a List

```lisp
habu> (defun reverse (lst)
        (reverse-helper lst nil))
<symbol>

habu> (defun reverse-helper (lst acc)
        (if (= lst 0) acc
          (reverse-helper (cdr lst)
                         (cons (car lst) acc))))
<symbol>

habu> (reverse '(1 2 3 4 5))
(5 4 3 2 1)
```

### Range Function

```lisp
habu> (defun range (start end)
        (if (> start end) nil
          (cons start (range (+ start 1) end))))
<symbol>

habu> (range 1 10)
(1 2 3 4 5 6 7 8 9 10)
```

### Sum of Squares

```lisp
habu> (defun square (x) (* x x))
<symbol>

habu> (defun sum-squares (a b)
        (+ (square a) (square b)))
<symbol>

habu> (sum-squares 3 4)
25
```

### Finding Maximum

```lisp
habu> (defun max-of-list (lst)
        (if (= (cdr lst) 0)
          (car lst)
          (let ((rest-max (max-of-list (cdr lst))))
            (if (> (car lst) rest-max)
              (car lst)
              rest-max))))
<symbol>

habu> (max-of-list '(3 7 2 9 1))
9
```

## Tips and Tricks

### 1. Use Let for Intermediate Values

```lisp
# Instead of:
habu> (defun quadratic (a b c x)
        (+ (+ (* a (* x x)) (* b x)) c))

# Use:
habu> (defun quadratic (a b c x)
        (let ((x2 (* x x)))
          (+ (* a x2) (+ (* b x) c))))
```

### 2. Build Complex Functions from Simple Ones

```lisp
habu> (defun double (x) (* 2 x))
<symbol>

habu> (defun quadruple (x) (double (double x)))
<symbol>

habu> (quadruple 5)
20
```

### 3. Use Helper Functions for Recursion

```lisp
# Public interface
habu> (defun factorial (n)
        (factorial-helper n 1))

# Private helper with accumulator
habu> (defun factorial-helper (n acc)
        (if (= n 0) acc
          (factorial-helper (- n 1) (* n acc))))
```

### 4. Test Functions Incrementally

```lisp
# Test with simple cases first
habu> (defun my-func (x) (* x x))
<symbol>

habu> (my-func 0)
0

habu> (my-func 1)
1

habu> (my-func 5)
25

# Then test edge cases
habu> (my-func -5)
25
```

## Common Gotchas

### 1. nil vs 0

In Habu, `nil` is represented as `0`. To check for nil:

```lisp
# Correct
habu> (defun null? (x) (= x 0))

# For lists, 0 means nil (empty list)
habu> (if (= lst 0) 'empty 'not-empty)
```

### 2. Quote vs Evaluate

```lisp
# Quoted - literal list
habu> '(1 2 3)
(1 2 3)

# Unquoted - tries to call function 1
habu> (1 2 3)
nil  # Error - 1 is not a function
```

### 3. True/False Values

```lisp
# 0 (nil) is false, everything else is true
habu> (if 0 'yes 'no)
<symbol>  # no

habu> (if 1 'yes 'no)
<symbol>  # yes

habu> (if 42 'yes 'no)
<symbol>  # yes
```

### 4. Function Application

```lisp
# Correct - function name is operator
habu> (+ 1 2)
3

# Wrong - need to use lambda or define function
habu> (f 1 2)
nil  # f not defined

# Correct - define first
habu> (defun f (x y) (+ x y))
<symbol>

habu> (f 1 2)
3
```

## Line Editing

The REPL supports readline-style line editing:

- **Left/Right arrows** - Move cursor
- **Ctrl-A** - Beginning of line
- **Ctrl-E** - End of line
- **Backspace** - Delete character
- **Ctrl-D** - Exit REPL (on empty line)

## Exiting the REPL

- Press **Ctrl-D** on an empty line
- Or type Ctrl-C to interrupt

```
habu> ^D

"Goodbye!"
```

## What's Included

### Core Features
✅ Numbers and arithmetic
✅ Symbols and quote
✅ Lists (cons, car, cdr)
✅ Conditionals (if)
✅ Local variables (let)
✅ Anonymous functions (lambda)
✅ Lexical closures
✅ Top-level definitions (defun)
✅ Full recursion
✅ Comparison operators

### What's NOT Included
❌ Macros
❌ Multiple values
❌ Continuations
❌ Tail-call optimization
❌ Garbage collection visibility
❌ String manipulation
❌ File I/O (beyond source)
❌ Numbers other than integers
❌ Vector/array indexing

## Performance Notes

- **Startup**: Instant
- **Recursion**: Works but not tail-optimized (deep recursion may overflow)
- **Speed**: Interpreted, not compiled (slower than native code)
- **Memory**: GC happens automatically

## Getting Help

### In the REPL

Try these test expressions:

```lisp
# Basic arithmetic
habu> (+ 1 2 3)
6

# List operations
habu> (cons 1 (cons 2 (cons 3 nil)))
(1 2 3)

# Functions
habu> (defun test () 42)
<symbol>

habu> (test)
42
```

### Documentation

- `ENHANCED_REPL_SUMMARY.md` - Basic REPL features
- `PROGRAMMABLE_REPL_SUMMARY.md` - Let and lambda
- `RECURSIVE_REPL_SUMMARY.md` - Defun and recursion
- `REPL_PROGRESSION.md` - How we got here
- `REPL_FINAL_STATUS.md` - Complete feature list

## Example Session

```lisp
$ ./habu-rec
"Habu REPL - Recursive"
"Features: let, lambda, defun"

habu> (defun factorial (n)
        (if (= n 0) 1
          (* n (factorial (- n 1)))))
<symbol>

habu> (factorial 5)
120

habu> (defun map (f lst)
        (if (= lst 0) nil
          (cons (f (car lst))
                (map f (cdr lst)))))
<symbol>

habu> (map factorial '(1 2 3 4 5))
(1 2 6 24 120)

habu> (defun sum (lst)
        (if (= lst 0) 0
          (+ (car lst) (sum (cdr lst)))))
<symbol>

habu> (sum '(1 2 3 4 5))
15

habu> ^D

"Goodbye!"
```

## Learn More

Habu Lisp implements core Scheme/Lisp semantics. To learn more about Lisp:

- **SICP** (Structure and Interpretation of Computer Programs)
- **The Little Schemer**
- **Practical Common Lisp**

## Have Fun!

Habu Lisp is a complete, working Lisp in your pocket. Use it to:

- Learn Lisp programming
- Implement algorithms
- Experiment with functional programming
- Build small utilities
- Have fun with code!

**Welcome to Lisp!** 🎉

---

*Habu Lisp REPL - 73KB of pure Lisp goodness*
