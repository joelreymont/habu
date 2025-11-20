# Habu Lisp - Example REPL Session

This document shows a complete REPL session demonstrating various features of Habu Lisp.

## Session Transcript

```
$ ./habu-rec
"Habu REPL - Recursive"
"Features: let, lambda, defun"

habu> ; Basic arithmetic
habu> (+ 2 3)
5

habu> (* 7 8)
56

habu> (/ 100 4)
25

habu> ; List operations
habu> '(1 2 3 4 5)
(1 2 3 4 5)

habu> (car '(apple banana cherry))
<symbol>

habu> (cdr '(10 20 30))
(20 30)

habu> (cons 0 '(1 2 3))
(0 1 2 3)

habu> ; Conditionals
habu> (if (> 5 3) 'yes 'no)
<symbol>

habu> (if (= 10 10) 42 0)
42

habu> ; Local variables with let
habu> (let ((x 10) (y 20))
        (+ x y))
30

habu> (let ((radius 5))
        (let ((area (* radius radius)))
          (* 3 area)))
75

habu> ; Anonymous functions
habu> ((lambda (x) (* x x)) 7)
49

habu> ((lambda (x y) (+ x y)) 15 27)
42

habu> ; Closures
habu> (let ((multiplier 3))
        (let ((triple (lambda (n) (* multiplier n))))
          (triple 14)))
42

habu> ; Define functions
habu> (defun square (x)
        (* x x))
<symbol>

habu> (square 9)
81

habu> ; Recursive functions
habu> (defun factorial (n)
        (if (= n 0) 1
          (* n (factorial (- n 1)))))
<symbol>

habu> (factorial 5)
120

habu> (factorial 10)
3628800

habu> ; Fibonacci
habu> (defun fib (n)
        (if (< n 2) n
          (+ (fib (- n 1)) (fib (- n 2)))))
<symbol>

habu> (fib 0)
0

habu> (fib 1)
1

habu> (fib 5)
5

habu> (fib 10)
55

habu> ; List processing
habu> (defun length (lst)
        (if (= lst 0) 0
          (+ 1 (length (cdr lst)))))
<symbol>

habu> (length '(a b c d e))
5

habu> (defun sum (lst)
        (if (= lst 0) 0
          (+ (car lst) (sum (cdr lst)))))
<symbol>

habu> (sum '(1 2 3 4 5))
15

habu> (sum '(10 20 30 40))
100

habu> ; Higher-order functions
habu> (defun twice (f x)
        (f (f x)))
<symbol>

habu> (defun add1 (n)
        (+ n 1))
<symbol>

habu> (twice add1 10)
12

habu> ; Map function
habu> (defun map (f lst)
        (if (= lst 0) nil
          (cons (f (car lst))
                (map f (cdr lst)))))
<symbol>

habu> (map square '(1 2 3 4 5))
(1 4 9 16 25)

habu> (map add1 '(10 20 30))
(11 21 31)

habu> ; Filter function
habu> (defun positive? (n)
        (> n 0))
<symbol>

habu> (defun filter (pred lst)
        (if (= lst 0) nil
          (if (pred (car lst))
            (cons (car lst) (filter pred (cdr lst)))
            (filter pred (cdr lst)))))
<symbol>

habu> (filter positive? '(-2 -1 0 1 2 3))
(1 2 3)

habu> ; Fold/reduce
habu> (defun fold (f init lst)
        (if (= lst 0) init
          (fold f (f init (car lst)) (cdr lst))))
<symbol>

habu> (fold + 0 '(1 2 3 4 5))
15

habu> (fold * 1 '(2 3 4))
24

habu> ; Reverse a list
habu> (defun reverse (lst)
        (reverse-helper lst nil))
<symbol>

habu> (defun reverse-helper (lst acc)
        (if (= lst 0) acc
          (reverse-helper (cdr lst) (cons (car lst) acc))))
<symbol>

habu> (reverse '(1 2 3 4 5))
(5 4 3 2 1)

habu> ; Range function
habu> (defun range (start end)
        (if (> start end) nil
          (cons start (range (+ start 1) end))))
<symbol>

habu> (range 1 10)
(1 2 3 4 5 6 7 8 9 10)

habu> ; Combining functions
habu> (sum (map square (range 1 5)))
55

habu> ; Sum of squares: 1^2 + 2^2 + 3^2 + 4^2 + 5^2 = 1 + 4 + 9 + 16 + 25 = 55

habu> ; Compose functions
habu> (defun compose (f g)
        (lambda (x) (f (g x))))
<symbol>

habu> (let ((double (lambda (x) (* 2 x)))
            (increment (lambda (x) (+ x 1))))
        (let ((double-then-inc (compose increment double)))
          (double-then-inc 5)))
11

habu> ; (5 * 2) + 1 = 11

habu> ; More complex example: sum of even squares
habu> (defun even? (n)
        (= (- n (* 2 (/ n 2))) 0))
<symbol>

habu> (filter even? (range 1 10))
(2 4 6 8 10)

habu> (map square (filter even? (range 1 10)))
(4 16 36 64 100)

habu> (sum (map square (filter even? (range 1 10))))
220

habu> ; 2^2 + 4^2 + 6^2 + 8^2 + 10^2 = 4 + 16 + 36 + 64 + 100 = 220

habu> ; Power function
habu> (defun power (base exp)
        (if (= exp 0) 1
          (* base (power base (- exp 1)))))
<symbol>

habu> (power 2 0)
1

habu> (power 2 8)
256

habu> (power 3 4)
81

habu> ; Greatest Common Divisor (Euclid's algorithm)
habu> (defun gcd (a b)
        (if (= b 0) a
          (gcd b (- a (* b (/ a b))))))
<symbol>

habu> (gcd 48 18)
6

habu> (gcd 100 35)
5

habu> ; Sum of squares (algebraic)
habu> (defun sum-of-squares (a b)
        (+ (* a a) (* b b)))
<symbol>

habu> (sum-of-squares 3 4)
25

habu> ; Pythagorean theorem: 3^2 + 4^2 = 9 + 16 = 25

habu> ; Exit with Ctrl-D
habu> ^D

"Goodbye!"
$
```

## Key Observations

### Expression Evaluation
- All arithmetic operations work as expected
- Operators are prefix notation: `(+ 2 3)` not `2 + 3`
- Nested expressions evaluate inside-out

### Lists
- Quote prevents evaluation: `'(1 2 3)` is data, not a function call
- `cons` builds lists from front
- `car` gets first element, `cdr` gets rest
- Empty list is `nil` or `0`

### Functions
- `lambda` creates anonymous functions
- `defun` creates named functions
- Functions are first-class values
- Closures capture their environment

### Recursion
- Functions can call themselves
- Base case required to terminate
- No tail-call optimization (avoid deep recursion)

### Higher-Order Functions
- Functions can take other functions as arguments
- Functions can return functions
- Enables powerful abstractions like `map`, `filter`, `fold`

### Composing Solutions
- Small functions compose into larger solutions
- Functional style: `(sum (map square (filter even? (range 1 10))))`
- Reads right-to-left: range → filter → map → sum

## Common Patterns Demonstrated

### List Processing Pattern
```lisp
(defun process-list (lst)
  (if (= lst 0) base-case
    (combine (car lst) (process-list (cdr lst)))))
```

### Accumulator Pattern
```lisp
(defun compute (input)
  (helper input initial-accumulator))

(defun helper (input acc)
  (if (done? input) acc
    (helper (next input) (update acc input))))
```

### Higher-Order Pattern
```lisp
(defun operate (f data)
  (if (= data 0) nil
    (cons (f (car data))
          (operate f (cdr data)))))
```

## Try It Yourself!

Start the REPL and try these challenges:

### Challenge 1: Product of List
Write a function that multiplies all numbers in a list.

```lisp
(defun product (lst)
  ; Your code here
  )

; Should work:
(product '(2 3 4))  ; → 24
```

### Challenge 2: Maximum of List
Write a function that finds the maximum value in a list.

```lisp
(defun maximum (lst)
  ; Your code here
  )

; Should work:
(maximum '(3 7 2 9 1))  ; → 9
```

### Challenge 3: Nth Element
Write a function that returns the nth element of a list (0-indexed).

```lisp
(defun nth-elem (n lst)
  ; Your code here
  )

; Should work:
(nth-elem 0 '(a b c))  ; → a
(nth-elem 2 '(a b c))  ; → c
```

### Challenge 4: Append Lists
Write a function that concatenates two lists.

```lisp
(defun append-lists (lst1 lst2)
  ; Your code here
  )

; Should work:
(append-lists '(1 2) '(3 4))  ; → (1 2 3 4)
```

## Solutions

<details>
<summary>Click to reveal solutions</summary>

```lisp
; Challenge 1: Product
(defun product (lst)
  (if (= lst 0) 1
    (* (car lst) (product (cdr lst)))))

; Challenge 2: Maximum
(defun maximum (lst)
  (if (= (cdr lst) 0) (car lst)
    (let ((rest-max (maximum (cdr lst))))
      (if (> (car lst) rest-max)
        (car lst)
        rest-max))))

; Challenge 3: Nth Element
(defun nth-elem (n lst)
  (if (= n 0) (car lst)
    (nth-elem (- n 1) (cdr lst))))

; Challenge 4: Append Lists
(defun append-lists (lst1 lst2)
  (if (= lst1 0) lst2
    (cons (car lst1) (append-lists (cdr lst1) lst2))))
```

</details>

---

**Happy Lisping!** 🎉

For more information:
- **Quick Reference**: See `QUICK_REFERENCE.md`
- **Tutorial**: See `README_REPL.md`
- **Standard Library**: See `stdlib.lisp`
- **Examples**: See `examples.lisp`
