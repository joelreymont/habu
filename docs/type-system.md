# Habu Type System Specification

## Overview

Habu uses a **declared type system with occurrence typing**, inspired by Typed Racket. Types are mandatory for function signatures but can use `t` as an escape hatch. The system is designed to catch type confusion bugs at compile time, particularly around the tagged pointer representation.

## Design Goals

1. **Catch tag confusion bugs** - Prevent mixing fixnum/pointer operations
2. **Gradual adoption** - `t` type allows untyped code to coexist
3. **Future inference** - Design supports adding HM-style inference later
4. **Zero runtime cost** - Types are erased after compilation

## Type Lattice

```
                    t (top/any)
                   / \
              fixnum   object
                      /  |  \  \  \  \
                   cons sym vec str closure keyword
                    |
                   nil (bottom for object, also its own type)
```

### Primitive Types

| Type | Tag | Description |
|------|-----|-------------|
| `fixnum` | bit0=1 | 63-bit signed integer |
| `cons` | 0 | Cons cell (car/cdr pair) |
| `symbol` | 2 | Interned symbol |
| `vector` | 4 | Fixed-size array |
| `string` | 6 | Character string |
| `closure` | 8 | Function closure |
| `keyword` | 10 | Keyword symbol |
| `nil` | 0 | The nil value (special case) |

### Compound Types

| Syntax | Meaning |
|--------|---------|
| `(or T1 T2 ...)` | Union type |
| `(list T)` | Equivalent to `(or (cons T (list T)) nil)` |
| `(vector T)` | Vector containing elements of type T |
| `(-> (T1 T2 ...) R)` | Function type |

### Common Aliases

```lisp
(deftype list (T) (or (cons T (list T)) nil))
(deftype bool () (or nil t))
(deftype any () t)
```

## Syntax

### Function Declarations

```lisp
(defun function-name ((param1 type1) (param2 type2) ...) -> return-type
  body...)
```

Example:
```lisp
(defun string-ref ((s string) (idx fixnum)) -> fixnum
  (let ((len (string-length s)))
    (if (< idx len)
        (char-code-at s idx)
        (error "index out of bounds"))))
```

### Variable Declarations

```lisp
(let (((x fixnum) 42)
      ((s string) "hello"))
  body...)

;; Or with type on separate line for clarity
(let ((x 42))
  (declare (type fixnum x))
  body...)
```

### Type Assertions

```lisp
(the fixnum expr)  ; Assert expr has type fixnum
```

## Occurrence Typing

Types are narrowed in conditional branches based on type predicates:

```lisp
(defun safe-car ((x (or cons nil))) -> t
  (if (consp x)
      (car x)      ; x is known to be cons here
      nil))        ; x is known to be nil here

(defun process ((x t)) -> fixnum
  (cond
    ((fixnump x) (+ x 1))      ; x: fixnum
    ((stringp x) (string-length x))  ; x: string
    ((consp x) (length x))     ; x: cons
    (t 0)))
```

### Type Predicates

| Predicate | Narrows to |
|-----------|------------|
| `fixnump` | `fixnum` |
| `consp` | `cons` |
| `symbolp` | `symbol` |
| `vectorp` | `vector` |
| `stringp` | `string` |
| `closurep` | `closure` |
| `keywordp` | `keyword` |
| `null` | `nil` |

## Primitive Type Signatures

These signatures would have caught the `logand` bug:

```lisp
;; Bitwise operations require fixnum operands
(defprimitive logand ((a fixnum) (b fixnum)) -> fixnum)
(defprimitive logior ((a fixnum) (b fixnum)) -> fixnum)
(defprimitive logxor ((a fixnum) (b fixnum)) -> fixnum)
(defprimitive lognot ((a fixnum)) -> fixnum)
(defprimitive ash ((n fixnum) (count fixnum)) -> fixnum)

;; Arithmetic
(defprimitive + ((a fixnum) (b fixnum)) -> fixnum)
(defprimitive - ((a fixnum) (b fixnum)) -> fixnum)
(defprimitive * ((a fixnum) (b fixnum)) -> fixnum)
(defprimitive / ((a fixnum) (b fixnum)) -> fixnum)
(defprimitive mod ((a fixnum) (b fixnum)) -> fixnum)

;; Comparisons return t or nil (bool)
(defprimitive < ((a fixnum) (b fixnum)) -> (or nil t))
(defprimitive > ((a fixnum) (b fixnum)) -> (or nil t))
(defprimitive = ((a fixnum) (b fixnum)) -> (or nil t))

;; Object operations
(defprimitive eq ((a t) (b t)) -> (or nil t))
(defprimitive cons ((car t) (cdr t)) -> cons)
(defprimitive car ((c cons)) -> t)
(defprimitive cdr ((c cons)) -> t)

;; String operations
(defprimitive string-length ((s string)) -> fixnum)
(defprimitive string-ref ((s string) (idx fixnum)) -> fixnum)  ; char code
(defprimitive make-string ((len fixnum)) -> string)

;; Vector operations
(defprimitive make-vector ((len fixnum)) -> vector)
(defprimitive vector-ref ((v vector) (idx fixnum)) -> t)
(defprimitive vector-set ((v vector) (idx fixnum) (val t)) -> nil)
(defprimitive vector-length ((v vector)) -> fixnum)

;; Type predicates
(defprimitive fixnump ((x t)) -> (or nil t))
(defprimitive consp ((x t)) -> (or nil t))
(defprimitive symbolp ((x t)) -> (or nil t))
(defprimitive vectorp ((x t)) -> (or nil t))
(defprimitive stringp ((x t)) -> (or nil t))
(defprimitive null ((x t)) -> (or nil t))
```

## Type Checking Rules

### Subtyping

```
nil <: (or T1 ... Tn)  when nil <: Ti for some i
T <: (or T1 ... Tn)    when T <: Ti for some i
T <: t                 for all T
fixnum <: fixnum
cons <: cons
... (each primitive type is subtype of itself)
```

### Function Application

```
If f : (-> (T1 T2 ... Tn) R)
and e1 : S1, e2 : S2, ..., en : Sn
and Si <: Ti for all i
then (f e1 e2 ... en) : R
```

### Conditionals

```
If condition uses (predicate x) where predicate narrows T to T'
then in true branch, x : T'
and in false branch, x : T - T' (type difference)
```

## Escape Hatches

### The `t` Type

Any value can be assigned to `t`, and `t` can be narrowed via predicates:

```lisp
(defun process ((x t)) -> fixnum
  (if (fixnump x)
      x        ; x: fixnum, valid return
      0))      ; fallback
```

### Unsafe Cast

For interop with untyped code or raw memory:

```lisp
(unsafe-cast fixnum ptr-value)  ; Trust me, this is a fixnum
```

**Warning**: `unsafe-cast` bypasses type checking. Use only when absolutely necessary.

## Error Messages

Type errors should provide clear diagnostics:

```
Type error at string-ref:3:5
  Expected: fixnum
  Got: vector
  In expression: (logand v 15)

  Hint: logand requires fixnum arguments.
  The value 'v' has type 'vector' (tag 4).
  Did you mean to extract a fixnum field from the vector?
```

## Implementation Notes

### Compile-Time Type Environment

```lisp
;; Type environment maps variables to types
(defstruct type-env
  (bindings nil))  ; alist of (name . type)

;; Function type database
(defvar *fn-types* (make-hash-table))  ; name -> (-> (args...) return)
```

### IR Annotations

Types flow through IR compilation:

```lisp
;; Before (untyped)
(add-ir left right)

;; After (typed)
(add-ir left right :type fixnum :left-type fixnum :right-type fixnum)
```

### Codegen Type-Directed Selection

The type system enables correct codegen selection:

```lisp
(defun codegen-band (ir)
  (let ((left-type (ir-left-type ir))
        (right-type (ir-right-type ir)))
    (cond
      ;; Both fixnum: direct AND (after untagging)
      ((and (eq left-type 'fixnum) (eq right-type 'fixnum))
       (emit-fixnum-band ...))
      ;; Pointer AND fixnum: extract pointer bits
      ((and (object-type-p left-type) (eq right-type 'fixnum))
       (emit-ptr-band ...))
      (t
       (error "Invalid BAND operand types: ~S ~S" left-type right-type)))))
```

## Future Extensions

### Type Inference (Phase 2)

Add Hindley-Milner inference for local bindings:

```lisp
(let ((x 42))        ; x inferred as fixnum
  (let ((y (+ x 1))) ; y inferred as fixnum
    y))
```

### Polymorphism (Phase 3)

Generic functions with type parameters:

```lisp
(defun map (('a -> 'b) f ((list 'a) xs)) -> (list 'b)
  (if (null xs)
      nil
      (cons (f (car xs)) (map f (cdr xs)))))
```

### Refinement Types (Phase 4)

Dependent types for bounds checking:

```lisp
(defun vector-ref ((v vector) (idx (refine fixnum (< idx (vector-length v))))) -> t
  ...)
```

## Migration Strategy

1. **Phase 1**: Add type annotations to primitives, catch obvious bugs
2. **Phase 2**: Require types on all `defun`, allow `t` escape
3. **Phase 3**: Add inference for local variables
4. **Phase 4**: Add polymorphism for data structures
5. **Phase 5**: Full CL type system compatibility mode
