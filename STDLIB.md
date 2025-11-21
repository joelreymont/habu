# Habu Standard Library (stdlib.lisp)

## Overview

The Habu standard library provides core utility functions implemented in pure Habu Lisp. All functions are designed to compile to ARM64 machine code and use only the operations supported by the compiler.

## Loading the Standard Library

```lisp
(load "stdlib.lisp")
```

## Boolean and Logic Operations

### `(not x)`
Logical negation.
- Returns `nil` if x is truthy, `1` otherwise

### `(null? x)`
Test if x is nil (zero).
- Returns `1` if x is zero, `nil` otherwise

### `(pair? x)`
Alias for `cons?` - test if x is a cons cell.

## Numeric Predicates

### `(zero? n)`
Test if n equals zero.

### `(positive? n)`
Test if n is greater than zero.

### `(negative? n)`
Test if n is less than zero.

### `(even? n)`
Test if n is even.

### `(odd? n)`
Test if n is odd.

## Numeric Utilities

### `(abs n)`
Absolute value of n.

### `(min a b)`
Minimum of two numbers.

### `(max a b)`
Maximum of two numbers.

### `(square x)`
Square of x (x²).

### `(cube x)`
Cube of x (x³).

## List Utilities

### `(length lst)`
Count the number of elements in a list.
- Tail-recursive implementation
- Returns 0 for empty list

```lisp
(length '(1 2 3))  ; => 3
```

### `(append lst1 lst2)`
Concatenate two lists.

```lisp
(append '(1 2) '(3 4))  ; => '(1 2 3 4)
```

### `(reverse lst)`
Reverse a list.
- Uses tail-recursive helper `reverse-helper`
- Runs in O(n) time and space

```lisp
(reverse '(1 2 3))  ; => '(3 2 1)
```

### `(nth n lst)`
Get the nth element of a list (0-indexed).

```lisp
(nth 0 '(a b c))  ; => a
(nth 2 '(a b c))  ; => c
```

### `(last lst)`
Get the last element of a list.

```lisp
(last '(1 2 3))  ; => 3
```

### `(take n lst)`
Take first n elements from list.

```lisp
(take 2 '(1 2 3 4))  ; => '(1 2)
```

### `(drop n lst)`
Drop first n elements from list, return the rest.

```lisp
(drop 2 '(1 2 3 4))  ; => '(3 4)
```

## Higher-Order Functions

### `(map f lst)`
Apply function f to each element of lst, return list of results.

```lisp
(defun double (x) (* 2 x))
(map double '(1 2 3))  ; => '(2 4 6)
```

### `(filter pred lst)`
Return list of elements satisfying predicate.

```lisp
(filter positive? '(-1 0 1 2))  ; => '(1 2)
```

### `(fold f init lst)`
Fold list from left using binary function f with initial value.

```lisp
(fold + 0 '(1 2 3 4))  ; => 10
(fold * 1 '(1 2 3 4))  ; => 24
```

### `(reduce f lst)`
Reduce non-empty list using binary function f.
- First element is used as initial accumulator

```lisp
(reduce + '(1 2 3 4))  ; => 10
(reduce max '(3 1 4 2))  ; => 4
```

## List Predicates

### `(member? x lst)`
Test if element x is in list.
- Returns truthy value if found, `nil` otherwise

```lisp
(member? 2 '(1 2 3))  ; => 1
(member? 4 '(1 2 3))  ; => nil
```

### `(all? pred lst)`
Test if all elements satisfy predicate.

```lisp
(all? positive? '(1 2 3))  ; => 1
(all? positive? '(1 -2 3))  ; => nil
```

### `(any? pred lst)`
Test if any element satisfies predicate.

```lisp
(any? positive? '(-1 2 -3))  ; => 1
(any? positive? '(-1 -2 -3))  ; => nil
```

## Numeric Algorithms

### `(factorial n)`
Factorial of n.
- Recursive implementation
- factorial(5) = 120

```lisp
(factorial 5)  ; => 120
```

### `(fibonacci n)`
Nth Fibonacci number.
- Recursive implementation (exponential time)
- fibonacci(10) = 55

```lisp
(fibonacci 10)  ; => 55
```

### `(gcd a b)`
Greatest common divisor using Euclidean algorithm.

```lisp
(gcd 12 8)  ; => 4
```

### `(power base exp)`
Raise base to exp power (non-negative integer exponent).

```lisp
(power 2 10)  ; => 1024
```

## List Construction

### `(range start end)`
Create list of integers from start to end (inclusive).

```lisp
(range 1 5)  ; => '(1 2 3 4 5)
```

### `(repeat n x)`
Create list of n copies of x.

```lisp
(repeat 3 'a)  ; => '(a a a)
```

### `(replicate n x)`
Alias for `repeat`.

## List Processing

### `(sum lst)`
Sum of all elements in list.

```lisp
(sum '(1 2 3 4))  ; => 10
```

### `(product lst)`
Product of all elements in list.

```lisp
(product '(1 2 3 4))  ; => 24
```

### `(count pred lst)`
Count elements satisfying predicate.

```lisp
(count even? '(1 2 3 4 5 6))  ; => 3
```

### `(zip lst1 lst2)`
Zip two lists into list of pairs.

```lisp
(zip '(1 2 3) '(a b c))  ; => '((1 . a) (2 . b) (3 . c))
```

## Sorting

### `(insert x lst)`
Insert element x into sorted list, maintaining order.

```lisp
(insert 5 '(1 3 7 9))  ; => '(1 3 5 7 9)
```

### `(sort lst)`
Sort list using insertion sort.
- O(n²) time complexity
- Stable sort

```lisp
(sort '(3 1 4 1 5 9))  ; => '(1 1 3 4 5 9)
```

## Functional Composition

### `(compose f g)`
Function composition: (compose f g)(x) = f(g(x))

```lisp
(defun add1 (x) (+ x 1))
(defun double (x) (* 2 x))
(let ((f (compose double add1)))
  (f 5))  ; => 12  (double of 6)
```

### `(twice f)`
Apply function twice: (twice f)(x) = f(f(x))

```lisp
(let ((add4 (twice (lambda (x) (+ x 2)))))
  (add4 10))  ; => 14
```

### `(flip f)`
Flip arguments of binary function.

```lisp
(let ((sub-from-10 (flip - 10)))
  (sub-from-10 3))  ; => 7  (10 - 3)
```

## Utility Functions

### `(identity x)`
Return x unchanged.
- Useful as default function parameter

### `(const x)`
Return function that always returns x.

### `(apply2 f x y)`
Apply binary function f to x and y.

## Example Usage

```lisp
;;; Load the standard library
(load "stdlib.lisp")

;;; List operations
(reverse '(1 2 3))             ; => '(3 2 1)
(append '(1 2) '(3 4))         ; => '(1 2 3 4)
(filter positive? '(-1 1 -2 2))  ; => '(1 2)

;;; Numeric algorithms
(factorial 5)                  ; => 120
(fibonacci 10)                 ; => 55
(power 2 10)                   ; => 1024

;;; Higher-order functions
(map square '(1 2 3 4))        ; => '(1 4 9 16)
(fold + 0 '(1 2 3 4))          ; => 10
(filter even? '(1 2 3 4 5 6))  ; => '(2 4 6)

;;; List construction
(range 1 10)                   ; => '(1 2 3 4 5 6 7 8 9 10)
(repeat 5 'x)                  ; => '(x x x x x)

;;; Sorting
(sort '(3 1 4 1 5 9 2 6))     ; => '(1 1 2 3 4 5 6 9)
```

## Implementation Notes

- All functions use **tail recursion** where possible for efficiency
- Functions rely only on compiler-supported operations:
  - Arithmetic: `+`, `-`, `*`, `/`, `mod`
  - Comparison: `=`, `<`, `>`, `<=`, `>=`, `!=`
  - List operations: `cons`, `car`, `cdr`
  - Predicates: `nil?`, `cons?`, `fixnum?`
  - Control flow: `if`, `let`, `lambda`, `defun`
- No mutation (no `setq` or `set!`)
- Pure functional style throughout

## Testing

Run the test suite:

```bash
# Load and run tests in SBCL (for testing compilation)
sbcl --load stdlib.lisp --load test-stdlib.lisp

# Or compile and run natively (once compiler ready)
./habu test-stdlib.lisp
```

Test suite includes:
- 70+ test cases covering all functions
- Expected output validation
- Edge case testing (empty lists, zero, negative numbers)

## Future Enhancements

Planned additions:
- String operations (when string support added to compiler)
- Vector operations (when vector support added)
- Association list utilities (alist-get, alist-put)
- Tree operations (tree-map, tree-fold)
- More efficient sorting algorithms (quicksort, mergesort)
- Lazy sequences and infinite lists

## Performance Characteristics

| Function | Time Complexity | Space Complexity |
|----------|----------------|------------------|
| length | O(n) | O(1) stack |
| append | O(n) | O(n) |
| reverse | O(n) | O(n) |
| map | O(n) | O(n) |
| filter | O(n) | O(n) |
| fold | O(n) | O(1) stack |
| member? | O(n) | O(1) stack |
| factorial | O(n) | O(n) stack |
| fibonacci | O(2ⁿ) | O(n) stack |
| sort (insertion) | O(n²) | O(n) |
| range | O(n) | O(n) |

Note: Stack space usage is for non-tail-recursive functions. Tail-recursive functions (marked with TCO) use O(1) stack space when compiled with tail-call optimization enabled.
