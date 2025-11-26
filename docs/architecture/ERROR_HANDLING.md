# Error Handling in Habu

## Overview

Habu implements a catch/throw error handling system similar to Common Lisp. This provides non-local exits and basic exception handling capabilities.

## Phase 1: Bootstrap Implementation

For Phase 1, error handling uses SBCL's catch/throw mechanism via FFI trampolines. This allows rapid development while maintaining a clean architecture.

### Catch

Establishes a catch point with a tag:

```lisp
(catch tag body)
```

- `tag`: A symbol or fixnum identifying this catch point
- `body`: Expression(s) to evaluate
- Returns: The value of body, or the value thrown to this tag

Example:
```lisp
(catch 'my-tag
  (+ 1 2 3))  ; Returns 6 normally
```

### Throw

Performs a non-local exit to a matching catch:

```lisp
(throw tag value)
```

- `tag`: Must match a catch tag in the dynamic scope
- `value`: Value to return from the matching catch
- Never returns (transfers control to catch)

Example:
```lisp
(catch 'error
  (if (< x 0)
      (throw 'error "negative value")
      (process x)))
```

### Error Handling Patterns

#### Simple error recovery:
```lisp
(defun safe-divide (a b)
  (catch 'div-by-zero
    (if (= b 0)
        (throw 'div-by-zero 0)
        (/ a b))))
```

#### Nested catch handlers:
```lisp
(catch 'outer
  (catch 'inner
    (throw 'outer 42))  ; Skips inner, caught by outer
  (unreachable))
```

#### Multiple throw points:
```lisp
(catch 'error
  (if condition1 (throw 'error "error1"))
  (if condition2 (throw 'error "error2"))
  (normal-result))
```

## Implementation Details

### Runtime Support (Phase 1)

The runtime provides two functions:
- `runtime-catch`: Establishes catch point and calls body function
- `runtime-throw`: Searches for matching tag and performs non-local exit

Both are implemented as FFI trampolines to SBCL's catch/throw.

### Compiled Code

The compiler generates:
1. For `(catch tag body)`:
   - Evaluate tag into register
   - Create closure for body
   - Call runtime-catch trampoline

2. For `(throw tag value)`:
   - Evaluate tag and value into registers
   - Call runtime-throw trampoline (never returns)

### Calling Convention

**x86_64:**
- catch: RDI = tag, RSI = body-function → RAX = result
- throw: RDI = tag, RSI = value → never returns

**ARM64:**
- catch: X0 = tag, X1 = body-function → X0 = result
- throw: X0 = tag, X1 = value → never returns

## Phase 2: Standalone Implementation

In Phase 2, catch/throw will be implemented using:
- Explicit handler stack (allocated on heap or special stack)
- setjmp/longjmp style register saving
- Direct stack unwinding without SBCL dependency

This will enable:
- Stack traces on error
- Custom condition types
- More sophisticated error recovery

## Limitations (Phase 1)

- Tags must be compile-time constants (symbols or fixnums)
- No dynamic-wind or unwind-protect yet
- Limited debugging information
- Depends on SBCL runtime

## Future Enhancements

- `unwind-protect` for cleanup code
- `handler-case` / `handler-bind` for condition handling
- Custom condition types and hierarchy
- Stack traces and debugging information
- Error message formatting

## Examples

### Basic usage:
```lisp
(defun find-positive (lst)
  (catch 'found
    (dolist (x lst)
      (when (> x 0)
        (throw 'found x)))
    nil))  ; Not found

(find-positive '(-1 -2 3 -4))  ; Returns 3
```

### Error recovery:
```lisp
(defun safe-read-file (path)
  (catch 'file-error
    (let ((handle (file-open path "r")))
      (if (= handle 0)
          (throw 'file-error nil)
          (let ((contents (file-read handle)))
            (file-close handle)
            contents)))))
```

### Validation:
```lisp
(defun validate-age (age)
  (catch 'invalid
    (when (< age 0)
      (throw 'invalid "Age cannot be negative"))
    (when (> age 150)
      (throw 'invalid "Age unrealistic"))
    age))
```
