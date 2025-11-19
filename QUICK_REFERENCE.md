# Habu Lisp - Quick Reference Card

## Running the REPL

```bash
./habu-rec          # Complete Lisp (recommended)
./habu-prog         # Let + Lambda only
./habu-enhanced     # Basic evaluation only
```

## Special Forms

```lisp
(quote expr)        ; Literal data (also: 'expr)
(if test then else) ; Conditional evaluation
(let ((x val) ...) body) ; Local bindings
(lambda (x y ...) body)  ; Anonymous function
(defun name (x y ...) body) ; Define function
```

## Arithmetic Operators

```lisp
(+ 2 3)            ; Addition → 5
(- 10 3)           ; Subtraction → 7
(* 4 5)            ; Multiplication → 20
(/ 15 3)           ; Division → 5
```

## Comparison Operators (habu-rec only)

```lisp
(= x y)            ; Equal → 1 or nil
(< x y)            ; Less than → 1 or nil
(> x y)            ; Greater than → 1 or nil
```

## List Operations

```lisp
(cons 1 '(2 3))    ; Construct → (1 2 3)
(car '(1 2 3))     ; First element → 1
(cdr '(1 2 3))     ; Rest of list → (2 3)
(list 1 2 3)       ; Create list → (1 2 3)
'(1 2 3)           ; Quote list → (1 2 3)
```

## Truth Values

```lisp
nil                ; False (also represented as 0)
0                  ; False (nil)
1                  ; True
42                 ; True (any non-zero number)
```

## Examples

### Simple Arithmetic
```lisp
habu> (+ (* 2 3) (/ 10 2))
11
```

### Lists
```lisp
habu> (car '(apple banana cherry))
<symbol>

habu> (cdr '(1 2 3 4))
(2 3 4)

habu> (cons 0 '(1 2 3))
(0 1 2 3)
```

### Conditionals
```lisp
habu> (if (> 5 3) 'yes 'no)
<symbol>  ; yes

habu> (if nil 'true-branch 'false-branch)
<symbol>  ; false-branch
```

### Local Variables (habu-prog, habu-rec)
```lisp
habu> (let ((x 10) (y 20))
        (+ x y))
30

habu> (let ((a 5))
        (let ((b 10))
          (* a b)))
50
```

### Anonymous Functions (habu-prog, habu-rec)
```lisp
habu> ((lambda (x) (* x x)) 7)
49

habu> ((lambda (x y) (+ x y)) 10 20)
30

habu> (let ((double (lambda (n) (* 2 n))))
        (double 21))
42
```

### Named Functions (habu-rec only)
```lisp
habu> (defun square (x)
        (* x x))
<symbol>

habu> (square 8)
64

habu> (defun factorial (n)
        (if (= n 0) 1
          (* n (factorial (- n 1)))))
<symbol>

habu> (factorial 5)
120
```

### Higher-Order Functions
```lisp
habu> (defun twice (f x)
        (f (f x)))
<symbol>

habu> (defun add1 (n)
        (+ n 1))
<symbol>

habu> (twice add1 10)
12

habu> (defun compose (f g)
        (lambda (x) (f (g x))))
<symbol>

habu> (let ((double (lambda (x) (* 2 x)))
            (add1 (lambda (x) (+ x 1))))
        (let ((f (compose double add1)))
          (f 5)))
12
```

### List Processing
```lisp
habu> (defun length (lst)
        (if (= lst 0) 0
          (+ 1 (length (cdr lst)))))
<symbol>

habu> (length '(1 2 3 4 5))
5

habu> (defun sum (lst)
        (if (= lst 0) 0
          (+ (car lst) (sum (cdr lst)))))
<symbol>

habu> (sum '(10 20 30 40))
100

habu> (defun map (f lst)
        (if (= lst 0) nil
          (cons (f (car lst))
                (map f (cdr lst)))))
<symbol>

habu> (map square '(1 2 3 4 5))
(1 4 9 16 25)
```

## Common Patterns

### Helper Function with Accumulator
```lisp
(defun reverse (lst)
  (reverse-helper lst nil))

(defun reverse-helper (lst acc)
  (if (= lst 0) acc
    (reverse-helper (cdr lst)
                    (cons (car lst) acc))))
```

### Range Generation
```lisp
(defun range (start end)
  (if (> start end) nil
    (cons start (range (+ start 1) end))))

; Usage:
(range 1 5)  ; → (1 2 3 4 5)
```

### Filter Pattern
```lisp
(defun filter (pred lst)
  (if (= lst 0) nil
    (if (pred (car lst))
      (cons (car lst) (filter pred (cdr lst)))
      (filter pred (cdr lst)))))

; Usage:
(defun positive? (n) (> n 0))
(filter positive? '(-2 -1 0 1 2))  ; → (1 2)
```

### Fold/Reduce Pattern
```lisp
(defun fold (f init lst)
  (if (= lst 0) init
    (fold f (f init (car lst)) (cdr lst))))

; Usage:
(fold + 0 '(1 2 3 4 5))  ; → 15
(fold * 1 '(2 3 4))      ; → 24
```

## Tips

### 1. Check for nil
```lisp
(if (= lst 0) ...)  ; Check if list is empty
(if (= x nil) ...)  ; Check if value is nil
```

### 2. Build lists incrementally
```lisp
(cons 1 (cons 2 (cons 3 nil)))  ; → (1 2 3)
```

### 3. Use let for clarity
```lisp
; Instead of nested calls:
(+ (* a a) (* b b))

; Use let:
(let ((a2 (* a a))
      (b2 (* b b)))
  (+ a2 b2))
```

### 4. Test incrementally
```lisp
; Define function
(defun my-func (x) (* x x))

; Test simple cases
(my-func 0)  ; → 0
(my-func 1)  ; → 1
(my-func 5)  ; → 25

; Test edge cases
(my-func -5) ; → 25
```

## REPL Commands

- **Ctrl-D** (on empty line) - Exit REPL
- **Ctrl-C** - Interrupt/exit
- **Ctrl-A** - Beginning of line
- **Ctrl-E** - End of line
- **Left/Right arrows** - Move cursor
- **Backspace** - Delete character

## Common Errors

### Unbalanced parentheses
```lisp
; Wrong:
(+ 1 2

; Right:
(+ 1 2)
```

### Forgetting to quote lists
```lisp
; Wrong (tries to call function 1):
(car (1 2 3))

; Right:
(car '(1 2 3))
```

### Using undefined functions
```lisp
; Wrong (if > not available in habu-enhanced):
(if (> 5 3) ...)

; Right (use habu-rec for comparisons):
./habu-rec
(if (> 5 3) ...)
```

## Standard Library Functions

Load these from `stdlib.lisp`:

### Predicates
```lisp
null?    ; Check for nil
pair?    ; Check for cons cell
zero?    ; Check for zero
positive? negative? ; Check sign
even? odd? ; Check parity
```

### Utilities
```lisp
abs      ; Absolute value
min max  ; Min/max of two numbers
square   ; Square a number
append   ; Concatenate lists
reverse  ; Reverse a list
nth      ; Get nth element
take drop ; Take/drop n elements
```

### Higher-Order
```lisp
map      ; Apply function to list
filter   ; Select matching elements
fold     ; Accumulate from left
reduce   ; Fold without init
compose  ; Function composition
twice    ; Apply function twice
```

### Algorithms
```lisp
factorial  ; n!
fibonacci  ; Fibonacci number
gcd        ; Greatest common divisor
power      ; Exponentiation
sort       ; Insertion sort
```

## Feature Comparison

| Feature | Enhanced | Prog | Rec |
|---------|----------|------|-----|
| Arithmetic | ✓ | ✓ | ✓ |
| Lists | ✓ | ✓ | ✓ |
| Quote | ✓ | ✓ | ✓ |
| If | ✓ | ✓ | ✓ |
| Let | ✗ | ✓ | ✓ |
| Lambda | ✗ | ✓ | ✓ |
| Defun | ✗ | ✗ | ✓ |
| Comparisons | ✗ | ✗ | ✓ |
| Recursion | ✗ | ✗ | ✓ |

**Recommendation**: Use `habu-rec` for complete Lisp!

---

**Habu Lisp** - Complete Lisp in 73KB | 320 lines of code
Quick Reference v1.0
