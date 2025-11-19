# Control Flow in Habu

## Overview

Habu provides several iteration and control flow constructs for loops and early exits.

## Iteration Constructs

### dotimes

Simple counting loop that executes body a fixed number of times:

```lisp
(dotimes (var count [result-form])
  body...)
```

- `var`: Loop variable (counts from 0 to count-1)
- `count`: Number of iterations (evaluated once)
- `result-form`: Optional form evaluated after loop completes (default: nil)
- Returns: Value of result-form

Example:
```lisp
(dotimes (i 5)
  (print i))  ; Prints 0 1 2 3 4

(dotimes (i 10 (* i 2))
  (+ i 1))  ; Returns 20 (final i is 10, doubled)
```

### dolist

Iterates over elements of a list:

```lisp
(dolist (var list [result-form])
  body...)
```

- `var`: Loop variable bound to each element
- `list`: List to iterate over (evaluated once)
- `result-form`: Optional form evaluated after loop (default: nil)
- Returns: Value of result-form

Example:
```lisp
(dolist (x '(1 2 3 4))
  (print x))  ; Prints each element

(dolist (x mylist total)
  (setq total (+ total x)))  ; Returns sum
```

### do

General iteration with multiple variables and step forms:

```lisp
(do ((var1 init1 step1)
     (var2 init2 step2)
     ...)
    (test-form result-form)
  body...)
```

- Each `var` is initialized to `init` and stepped by `step`
- Loop continues while `test-form` is false
- When test succeeds, evaluates `result-form` and returns
- Body forms evaluated for side effects

Example:
```lisp
(do ((i 0 (+ i 1))
     (sum 0 (+ sum i)))
    ((>= i 10) sum)
  (print i))  ; Prints 0..9, returns 45
```

## Early Exit Constructs

### block

Establishes a named block that can be exited early:

```lisp
(block name
  body...)
```

- `name`: Symbol naming this block (not evaluated)
- Returns: Value of last form, or value from return-from

Example:
```lisp
(block search
  (dolist (x list)
    (when (> x 100)
      (return-from search x)))
  nil)  ; Not found
```

### return-from

Returns from a named block:

```lisp
(return-from name [value])
```

- `name`: Block name to return from (not evaluated)
- `value`: Value to return (default: nil)
- Never returns - transfers control to block

Example:
```lisp
(defun find-first-positive (lst)
  (block search
    (dolist (x lst)
      (when (> x 0)
        (return-from search x)))
    nil))
```

### return

Convenience macro for returning from implicit nil block:

```lisp
(return [value])
```

Equivalent to `(return-from nil value)`. Used in loops that implicitly create nil blocks.

## Phase 1 Implementation

### Transformation Strategy

For Phase 1, these constructs are implemented as source transformations:

**dotimes** expands to:
```lisp
(dotimes (i count result)
  body...)

;; Expands to:
(let ((#:count count)
      (#:result result))
  (let loop ((i 0))
    (if (>= i #:count)
        #:result
        (progn
          body...
          (loop (+ i 1))))))
```

**dolist** expands to:
```lisp
(dolist (var list result)
  body...)

;; Expands to:
(let ((#:list list)
      (#:result result))
  (let loop ((#:remaining #:list))
    (if (null? #:remaining)
        #:result
        (let ((var (car #:remaining)))
          body...
          (loop (cdr #:remaining))))))
```

**block/return-from** expand to catch/throw:
```lisp
(block name body...)
;; Expands to:
(catch 'name body...)

(return-from name value)
;; Expands to:
(throw 'name value)
```

### Benefits

- Reuses existing infrastructure (let, named-let, catch/throw)
- Simple to implement and understand
- No new runtime support needed
- Easy to optimize in Phase 2

## Common Patterns

### Early loop termination:
```lisp
(block nil
  (dotimes (i n)
    (when (found? i)
      (return-from nil i))))
```

### Accumulation:
```lisp
(let ((sum 0))
  (dolist (x list sum)
    (setq sum (+ sum x))))
```

### Nested loops with labeled exit:
```lisp
(block outer
  (dotimes (i 10)
    (dotimes (j 10)
      (when (= (* i j) 42)
        (return-from outer (cons i j))))))
```

### Searching:
```lisp
(defun find-if (predicate list)
  (block nil
    (dolist (x list)
      (when (predicate x)
        (return x)))
    nil))
```

## Phase 2 Enhancements

Future improvements:
- Direct loop compilation (no named-let overhead)
- Loop unrolling and optimization
- SIMD vectorization for numeric loops
- Proper lexical block/return-from (not catch/throw)
- Loop fusion and strength reduction

## Limitations (Phase 1)

- dotimes/dolist compile to recursive functions (stack usage)
- block/return-from use catch/throw (dynamic, not lexical)
- No loop or iterate macro (yet)
- No parallel iteration constructs
- Limited optimization compared to Phase 2
