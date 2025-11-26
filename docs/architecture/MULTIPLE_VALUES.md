# Multiple Return Values in Habu

## Overview

Habu supports Common Lisp-style multiple return values, allowing functions to return more than one value efficiently without heap allocation.

## Basic Usage

```lisp
;; Return multiple values
(defun divide-with-remainder (a b)
  (values (/ a b) (mod a b)))

;; Capture multiple values
(multiple-value-bind (quotient remainder)
    (divide-with-remainder 17 5)
  (list quotient remainder))  ; => (3 2)

;; Only first value is used in most contexts
(+ (values 10 20 30) 5)  ; => 15 (only 10 is used)
```

## API Reference

### values

Return multiple values from a function or expression.

```lisp
(values)           ; Return 0 values (nil as primary)
(values x)         ; Return 1 value
(values x y)       ; Return 2 values
(values x y z ...) ; Return N values
```

**Arguments:**
- Zero or more values to return

**Returns:**
- Primary value (first argument, or 0 if no arguments)
- Secondary values stored in runtime values array

**Examples:**
```lisp
(values)           ; => nil (0 values)
(values 42)        ; => 42
(values 1 2 3)     ; => 1 (+ stores 2, 3 for multiple-value-bind)
```

### multiple-value-bind

Bind multiple return values to variables.

```lisp
(multiple-value-bind (var1 var2 ... varN) values-form
  body...)
```

**Arguments:**
- `(var1 var2 ...)`: List of variable names to bind
- `values-form`: Expression that may return multiple values
- `body...`: Forms to evaluate with bindings in scope

**Returns:** Value of last form in body

**Behavior:**
- Evaluates `values-form`
- Binds each variable to corresponding return value
- Variables with no corresponding value are bound to nil
- Extra values are ignored

**Examples:**
```lisp
(multiple-value-bind (a b c)
    (values 1 2 3)
  (list a b c))  ; => (1 2 3)

(multiple-value-bind (x y z)
    (values 10 20)
  (list x y z))  ; => (10 20 nil)

(multiple-value-bind (x y)
    (values 1 2 3 4)
  (+ x y))  ; => 3 (extra values ignored)
```

### nth-value (Future)

```lisp
(nth-value n values-form)
```

Get the Nth value (0-indexed) from a multiple-value form.

**Status:** Not yet implemented

---

## Implementation Details

### Phase 1: Bootstrap Implementation

**Runtime Representation:**
- Global values array: Fixed-size array (16 slots max in Phase 1)
- Values count: Number of values returned
- Primary value: Always in RAX (x86_64) or X0 (ARM64)

**Memory Layout:**
```
*multiple-values* structure:
  [count: 8 bytes]           - Number of values
  [values: N*8 bytes]        - Array of value pointers
```

**Calling Convention:**
- Primary return value: RAX/X0 (standard return register)
- Secondary values: Written to global *multiple-values* array
- Count: Written to *multiple-values-count*

**Code Generation:**

For `(values a b c)`:
```assembly
; x86_64
; Evaluate a, b, c
; Store b, c in *multiple-values* array
; Set *multiple-values-count* to 3
; Return a in RAX
```

For `(multiple-value-bind (x y z) form body)`:
```assembly
; Evaluate form (gets primary in RAX)
; Read secondary values from *multiple-values* array
; Bind x=primary, y=values[0], z=values[1]
; Evaluate body
```

### Optimization

**Single Value Case:**
- When `values` has 1 argument, no array write needed
- Just return the value directly
- Set count to 1

**Zero Values Case:**
- Return 0 (nil) as primary
- Set count to 0
- No array writes

**Primary Value Only:**
- Most forms ignore secondary values
- Only `multiple-value-bind` and similar forms read the array
- This makes the common case (single value) very efficient

### Limitations (Phase 1)

1. **Fixed maximum:** 16 values max (Phase 1 limitation)
2. **Global state:** Values array is global (not thread-safe)
3. **No nested preservation:** Nested calls overwrite values array
4. **Limited forms:** Only `values` and `multiple-value-bind` initially

### Future Enhancements (Phase 2)

- `multiple-value-call`: Call function with multiple values as arguments
- `multiple-value-list`: Convert multiple values to list
- `multiple-value-setq`: Assign multiple values to variables
- `nth-value`: Get specific value by index
- Dynamic values array (no 16-value limit)
- Stack-based values passing (more efficient)
- Thread-local values storage

---

## Common Patterns

### Returning Status and Value

```lisp
(defun safe-divide (a b)
  (if (= b 0)
      (values nil nil)  ; error: return two nils
      (values t (/ a b))))  ; success: return t and result

(multiple-value-bind (success result)
    (safe-divide 10 2)
  (if success
      result
      0))  ; => 5
```

### Destructuring Results

```lisp
(defun parse-name (full-name)
  "Parse 'First Last' into two values"
  (let ((space-pos (position 32 full-name)))  ; space char
    (if space-pos
        (values (substring full-name 0 space-pos)
                (substring full-name (+ space-pos 1)))
        (values full-name nil))))

(multiple-value-bind (first last)
    (parse-name "John Doe")
  (list last first))  ; => ("Doe" "John")
```

### Hash Table Lookup

```lisp
(defun lookup-with-status (key table)
  "Return value and whether key was found"
  (let ((value (gethash key table)))
    (if (/= value 0)
        (values value t)
        (values nil nil))))
```

### Floor/Ceiling with Remainder

```lisp
(defun floor-rem (n divisor)
  "Return quotient and remainder"
  (values (/ n divisor) (mod n divisor)))

(multiple-value-bind (hours minutes)
    (floor-rem 125 60)
  (list hours minutes))  ; => (2 5)
```

---

## Interaction with Other Features

### With funcall

```lisp
(defun apply-multiple (fn)
  (multiple-value-bind (a b)
      (funcall fn)
    (+ a b)))

(apply-multiple (lambda () (values 10 20)))  ; => 30
```

### With let

```lisp
;; Only primary value is bound
(let ((x (values 1 2 3)))
  x)  ; => 1
```

### With cond/if

```lisp
;; Only primary value is tested
(if (values t nil)
    'yes
    'no)  ; => 'yes
```

### With progn

```lisp
;; Last form's multiple values are returned
(progn
  (+ 1 2)
  (values 10 20 30))  ; Returns all three values
```

---

## Design Rationale

### Why Multiple Values?

1. **Efficiency:** No heap allocation for temporary return values
2. **Clarity:** Express intent clearly (vs. returning lists)
3. **Common Lisp compatibility:** Standard feature in CL
4. **Useful patterns:** Status + value, quotient + remainder, etc.

### Why Global Array (Phase 1)?

1. **Simplicity:** Easy to implement with FFI trampolines
2. **Performance:** Fast access, no allocation
3. **Sufficient:** Works for non-nested cases
4. **Upgradeable:** Can move to stack-based in Phase 2

### Design Decisions

- **Primary value in register:** Standard calling convention, fast
- **Secondary values in array:** Simple, works with FFI
- **Count tracking:** Allows variable number of values
- **Implicit truncation:** Extra values ignored (CL semantics)
- **Implicit padding:** Missing values become nil (CL semantics)

---

## Examples

### Basic Examples

```lisp
;; Simple return
(defun get-coords ()
  (values 10 20))

(multiple-value-bind (x y) (get-coords)
  (+ x y))  ; => 30
```

### Practical Examples

```lisp
;; File operations with status
(defun read-config-file (path)
  (let ((contents (read-file path)))
    (if (/= contents 0)
        (values (parse contents) t)
        (values nil nil))))

(multiple-value-bind (config success)
    (read-config-file "/etc/config")
  (if success
      (use-config config)
      (use-defaults)))
```

### Error Handling

```lisp
(defun safe-operation (x)
  (catch 'error
    (if (< x 0)
        (throw 'error (values nil 'negative-input))
        (values (* x x) 'success))))

(multiple-value-bind (result status)
    (safe-operation -5)
  (list result status))  ; => (nil negative-input)
```

---

## Testing

Test coverage includes:
- Single value return
- Multiple value return (2, 3, 5 values)
- Zero values return
- `multiple-value-bind` with matching arity
- `multiple-value-bind` with fewer variables (truncation)
- `multiple-value-bind` with more variables (padding with nil)
- Primary value usage in normal contexts
- Nested `multiple-value-bind`
- Integration with funcall, let, cond

---

## Performance

**Expected Performance:**
- `(values x)`: Same as returning x directly
- `(values x y z)`: Small overhead for array writes
- `multiple-value-bind`: Array reads + let binding cost
- Primary-only usage: No overhead

**Phase 1 Characteristics:**
- Fast for common case (single value)
- Minimal overhead for multiple values (few array writes)
- Global state limits concurrent use
- Good enough for self-hosting compiler

**Phase 2 Improvements:**
- Stack-based values passing (no global state)
- Register-based for 2-3 values (no array access)
- Thread-safe implementation
- Better performance for nested calls
