# Tail-Call Optimization Design

## Overview

Tail-call optimization (TCO) is a critical compiler optimization that allows recursive functions to execute in constant stack space by converting tail calls into jumps instead of function calls.

## What is a Tail Call?

A function call is in **tail position** if it's the last operation before returning from the function:

```lisp
; Tail call - last thing before return
(defun factorial-tail (n acc)
  (if (zerop n)
      acc
      (factorial-tail (1- n) (* n acc))))  ; TAIL CALL

; NOT a tail call - multiplication happens after return
(defun factorial (n)
  (if (zerop n)
      1
      (* n (factorial (1- n)))))  ; NOT tail call - * happens after
```

## Tail Positions in Habu Lisp

A call is in tail position in these contexts:

1. **Function body**: The body of a lambda/defun
2. **If branches**: Both then and else branches of `if`
3. **Progn/begin**: The last expression in a sequence
4. **Let body**: The body expression after bindings
5. **Cond clauses**: The result expressions in each clause
6. **When/unless body**: The body (if single-valued)

## Implementation Strategy

### Phase 1: Self-Recursive Tail Calls (Named-Let)

Start with the simplest case - self-recursive calls in named-let:

```lisp
(let loop ((n 1000000))
  (if (zerop n)
      0
      (loop (1- n))))  ; Self tail-call
```

**Implementation**:
- Mark named-let as a potential TCO target
- Add tail-position tracking to code generation
- In tail position, emit jump instead of call
- Reuse existing stack frame

### Phase 2: Defun Self-Recursion

Extend to defun self-recursive calls:

```lisp
(defun countdown (n)
  (if (zerop n)
      0
      (countdown (1- n))))  ; Self tail-call
```

**Implementation**:
- Track function name during compilation
- Detect self-calls in tail position
- Emit tail-call sequence instead of regular call

### Phase 3: Mutual Recursion

Support tail calls between different functions:

```lisp
(defun even (n)
  (if (zerop n) t (odd (1- n))))

(defun odd (n)
  (if (zerop n) nil (even (1- n))))
```

**Challenge**: Need to know target function address at compile time.

## Code Generation Strategy

### Current Call Sequence (x86_64)

```asm
; Evaluate arguments
mov rax, arg1
push rax
mov rax, arg2
push rax

; Call (implicit in inline expansion)
<body code>

; Cleanup
add rsp, 16
```

### Tail-Call Sequence (x86_64)

For self tail-call:
```asm
; Evaluate arguments into temporaries
mov rax, arg1
mov rbx, arg2

; Replace current frame arguments
mov [rsp], rax
mov [rsp+8], rbx

; Jump to function start
jmp .function_start
```

Benefits:
- No stack growth
- Constant stack space for any recursion depth
- Same performance as iteration

## IR Extension

Add tail-position flag to compilation context:

```lisp
(defstruct compile-ctx
  (tail-p nil)        ; Are we in tail position?
  (fn-name nil)       ; Current function name (for self-calls)
  (fn-start nil))     ; Label for function start (for TCO)
```

## Testing Strategy

1. **Simple recursion**: Countdown from large number
2. **Accumulator pattern**: Factorial with accumulator
3. **Mutual recursion**: even/odd predicates
4. **Nested tail calls**: Multiple levels of tail recursion
5. **Non-tail recursion**: Verify we don't TCO when inappropriate

## Performance Benefits

- **Stack space**: O(1) instead of O(n) for n recursive calls
- **Speed**: Jump is faster than call/return
- **Practicality**: Enables functional programming patterns

## Example: Factorial Comparison

**Without TCO** (O(n) stack):
```lisp
(defun fact (n)
  (if (zerop n) 1 (* n (fact (1- n)))))

; Stack grows: fact(5) -> fact(4) -> fact(3) -> fact(2) -> fact(1) -> fact(0)
```

**With TCO** (O(1) stack):
```lisp
(defun fact-tail (n acc)
  (if (zerop n) acc (fact-tail (1- n) (* n acc))))

; Stack constant: fact-tail jumps to itself, reusing frame
```

## Implementation Plan

1. ✅ Document design (this file)
2. Add compile context with tail-position flag
3. Implement TCO for named-let
4. Add tests for named-let TCO
5. Implement TCO for defun self-recursion
6. Add tests for defun TCO
7. Document and commit

Future (Phase 3):
8. Implement mutual recursion TCO
9. Add tests for mutual recursion
10. Optimize tail-call sequence

## References

- Scheme R5RS/R6RS/R7RS: Proper tail recursion requirement
- SICP: Iterative processes via recursion
- Appel, "Compiling with Continuations": CPS and tail calls
