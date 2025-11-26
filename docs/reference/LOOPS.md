# Loop Constructs in Habu

## Overview

Habu supports Common Lisp-style loop constructs for iteration. Phase 1 uses inline code generation with direct jumps, avoiding the need for first-class closures.

## Basic Usage

```lisp
;; Iterate a fixed number of times
(dotimes (i 5)
  (print i))  ; prints 0, 1, 2, 3, 4

;; Iterate over a list
(dolist (x '(1 2 3))
  (print x))  ; prints 1, 2, 3
```

## API Reference

### dotimes

Iterate a fixed number of times with an index variable.

```lisp
(dotimes (var count-form [result-form])
  body...)
```

**Arguments:**
- `var`: Variable name bound to iteration index (0 to count-1)
- `count-form`: Expression evaluating to number of iterations
- `result-form`: Optional expression evaluated after loop (default: nil)
- `body...`: Forms to execute each iteration

**Returns:** Value of `result-form` (or nil if omitted)

**Behavior:**
- Evaluates `count-form` once before loop starts
- `var` is bound to 0, 1, 2, ..., count-1
- If count ≤ 0, body is not executed
- After loop, evaluates and returns `result-form`

**Examples:**
```lisp
;; Basic iteration
(dotimes (i 3)
  (print i))
; prints: 0, 1, 2
; returns: nil

;; With result form
(let ((sum 0))
  (dotimes (i 5 sum)
    (setq sum (+ sum i))))
; returns: 10 (0+1+2+3+4)

;; Using variable in computation
(dotimes (i 4)
  (* i i))  ; computes squares but doesn't collect them
; returns: nil

;; Zero iterations
(dotimes (i 0)
  (print "won't print"))
; returns: nil
```

### dolist

Iterate over elements of a list.

```lisp
(dolist (var list-form [result-form])
  body...)
```

**Arguments:**
- `var`: Variable name bound to each list element
- `list-form`: Expression evaluating to a list
- `result-form`: Optional expression evaluated after loop (default: nil)
- `body...`: Forms to execute for each element

**Returns:** Value of `result-form` (or nil if omitted)

**Behavior:**
- Evaluates `list-form` once before loop starts
- `var` is bound to each element in order
- For empty list, body is not executed
- After loop, evaluates and returns `result-form`

**Examples:**
```lisp
;; Basic iteration
(dolist (x '(1 2 3))
  (print x))
; prints: 1, 2, 3
; returns: nil

;; With result form
(let ((sum 0))
  (dolist (x '(10 20 30) sum)
    (setq sum (+ sum x))))
; returns: 60

;; Processing list elements
(let ((result nil))
  (dolist (x '(1 2 3) (reverse result))
    (setq result (cons (* x x) result))))
; returns: (1 4 9)

;; Empty list
(dolist (x nil)
  (print "won't print"))
; returns: nil
```

---

## Implementation Details

### Phase 1: Inline Code Generation

**Strategy:**
- Parse loop constructs as special forms
- Generate direct machine code with jumps (no runtime calls)
- Use label-based control flow

**Code Generation Pattern (dotimes):**
```assembly
; x86_64 pseudo-assembly
; Evaluate count-form -> RAX
; mov RBX, RAX          ; save count
; mov RCX, 0            ; i = 0
loop_start:
; cmp RCX, RBX          ; i < count?
; jge loop_end
; push RCX              ; bind i to stack
; <body code>
; pop RCX               ; restore i
; inc RCX               ; i++
; jmp loop_start
loop_end:
; <result-form code>
```

**Code Generation Pattern (dolist):**
```assembly
; x86_64 pseudo-assembly
; Evaluate list-form -> RAX
; mov RBX, RAX          ; current list
loop_start:
; cmp RBX, 0            ; list == nil?
; je loop_end
; car RBX -> RAX        ; get element
; push RAX              ; bind var
; <body code>
; pop RAX
; cdr RBX -> RBX        ; advance to next
; jmp loop_start
loop_end:
; <result-form code>
```

### Optimization

**Count Optimization:**
- When count is constant 0, skip loop entirely
- When count is constant positive, can unroll small loops

**List Optimization:**
- When list is nil, skip loop entirely
- Can optimize empty list check

**Variable Usage:**
- If var is never referenced in body, skip binding

### Limitations (Phase 1)

1. **No early exit:** Can't use return, break, or continue
2. **No nested binding:** Result form can't access loop variable
3. **No implicit block:** Can't use return-from (yet)
4. **Fixed structure:** Body must be compile-time constant

### Future Enhancements (Phase 2)

- `loop` macro with full Common Lisp syntax
- `do` and `do*` for general iteration
- Early exit with `return` or `loop-finish`
- Implicit block for `return-from`
- Collection clauses (collect, append, sum, etc.)

---

## Common Patterns

### Accumulation with dotimes

```lisp
;; Sum of squares
(let ((sum 0))
  (dotimes (i 10 sum)
    (setq sum (+ sum (* i i)))))
; => 285

;; Build list in reverse
(let ((result nil))
  (dotimes (i 5 result)
    (setq result (cons i result))))
; => (4 3 2 1 0)
```

### Accumulation with dolist

```lisp
;; Sum list elements
(let ((sum 0))
  (dolist (x '(1 2 3 4 5) sum)
    (setq sum (+ sum x))))
; => 15

;; Filter list (remove negatives)
(let ((result nil))
  (dolist (x '(-1 2 -3 4 -5) (reverse result))
    (when (> x 0)
      (setq result (cons x result)))))
; => (2 4)
```

### Side Effects

```lisp
;; Print table
(dotimes (i 5)
  (dotimes (j 5)
    (print (* i j))))

;; Process list with side effects
(dolist (filename file-list)
  (process-file filename))
```

### Counting

```lisp
;; Count positive numbers
(let ((count 0))
  (dolist (x '(-1 2 -3 4 5) count)
    (when (> x 0)
      (setq count (+ count 1)))))
; => 3
```

---

## Design Rationale

### Why Inline Code Generation?

1. **Performance:** Direct jumps are faster than function calls
2. **Simplicity:** No closure creation needed
3. **Compatibility:** Works with current Phase 1 architecture
4. **Clarity:** Generated code is straightforward

### Why Not Runtime Functions?

Phase 1 has limited first-class closure support:
- Closures work but require eval'd wrappers
- Passing functions as values is complex
- Inline generation is cleaner for common loops

Phase 2 will add full closure support and more sophisticated loop constructs.

### Design Decisions

- **Index starts at 0:** Following Common Lisp convention
- **Result form optional:** Default to nil for consistency
- **Count evaluated once:** Avoid surprising re-evaluation
- **List traversal with cdr:** Standard Lisp iteration pattern

---

## Examples

### Factorial

```lisp
(let ((n 5)
      (result 1))
  (dotimes (i n result)
    (setq result (* result (+ i 1)))))
; => 120
```

### List Length

```lisp
(let ((count 0))
  (dolist (x '(a b c d e) count)
    (setq count (+ count 1))))
; => 5
```

### List Reversal

```lisp
(let ((result nil))
  (dolist (x '(1 2 3 4 5) result)
    (setq result (cons x result))))
; => (5 4 3 2 1)
```

### List Mapping (squares)

```lisp
(let ((result nil))
  (dolist (x '(1 2 3 4 5) (reverse result))
    (setq result (cons (* x x) result))))
; => (1 4 9 16 25)
```

### Finding Maximum

```lisp
(let ((max nil))
  (dolist (x '(3 7 2 9 1) max)
    (when (or (not max) (> x max))
      (setq max x))))
; => 9
```

---

## Testing

Test coverage includes:
- Basic dotimes (0, 1, 5 iterations)
- Basic dolist (empty, single, multiple elements)
- Result forms for both
- Nested loops (dotimes in dotimes, dolist in dolist)
- Variable binding and scoping
- Side effects and accumulation
- Both x86_64 and ARM64 architectures

---

## Performance

**Expected Performance:**
- `dotimes`: Tight loop with minimal overhead
- `dolist`: One cdr operation per iteration
- No function call overhead
- No heap allocation for loop control

**Phase 1 Characteristics:**
- Direct jumps (fast)
- Stack-based variable binding
- Predictable code size
- Good for typical iteration patterns

**Phase 2 Improvements:**
- Register allocation for loop variables
- Loop unrolling for small constant counts
- Strength reduction optimizations
- Better integration with optimizer
