# CLOS (Common Lisp Object System) in Habu

## Overview

Habu implements a simplified subset of CLOS, the Common Lisp Object System. This provides object-oriented programming capabilities including classes, instances, and slot access.

## Current Features

### `defclass` - Class Definition

Define a new class with slots:

```lisp
(defclass class-name (superclasses)
  slot1
  slot2
  ...)
```

**Parameters:**
- `class-name` - Symbol naming the class
- `superclasses` - List of parent classes (currently ignored, reserved for future inheritance)
- `slot1, slot2, ...` - Slot definitions (symbols or lists with options)

**Slot Options:**
- Simple slot: `slot-name`
- Typed slot: `(slot-name :type type-specifier)`

**Generated Functions:**

When you define a class, `defclass` automatically generates:

1. **Constructor**: `make-class-name`
   - Takes one argument per slot (in definition order)
   - Returns a new instance

2. **Predicate**: `class-name-p`
   - Takes one argument
   - Returns `t` if argument is an instance of the class, `nil` otherwise

3. **Accessors**: `class-name-slot-name`
   - One accessor per slot
   - Takes an instance as argument
   - Returns the slot value
   - Includes type checking (errors if not an instance)

**Return Value:**
Returns the class name symbol.

### Examples

#### Simple Class

```lisp
;; Define a person class
(defclass person ()
  name
  age)

;; Creates these functions:
;; - (make-person name age)
;; - (person-p obj)
;; - (person-name obj)
;; - (person-age obj)

;; Create an instance
(define alice (make-person "Alice" 30))

;; Check type
(person-p alice)      ; => t
(person-p "Alice")    ; => nil

;; Access slots
(person-name alice)   ; => "Alice"
(person-age alice)    ; => 30
```

#### Typed Slots

```lisp
;; Define a point with typed coordinates
(defclass point ()
  (x :type fixnum)
  (y :type fixnum))

;; Create instance
(define origin (make-point 0 0))
(define pt (make-point 10 20))

;; Access with type safety
(point-x pt)  ; => 10
(point-y pt)  ; => 20

;; Type errors are caught at runtime
; (make-point "not-a-number" 0)  ; Error: type mismatch
```

#### Nested Classes

```lisp
;; Classes can contain other class instances
(defclass rectangle ()
  top-left
  bottom-right)

(define p1 (make-point 0 0))
(define p2 (make-point 100 50))
(define rect (make-rectangle p1 p2))

;; Access nested slots
(point-x (rectangle-top-left rect))  ; => 0
```

## Implementation Details

### Runtime Representation

Classes are implemented as vectors with the class name as the first element:

```
#('class-name slot1-value slot2-value ...)
```

This is the same representation used by `defstruct`, allowing code reuse and integration with the existing type system.

### Type System Integration

- Class types are registered with the type checker
- Predicates are registered for occurrence typing
- Type annotations on slots enable compile-time checking
- Accessor functions include runtime type validation

### Compile-Time Code Generation

`defclass` is a special form that expands at compile time to:

```lisp
(progn
  (defun make-class-name (slot1 slot2 ...)
    (vector 'class-name slot1 slot2 ...))

  (defun class-name-p (obj)
    (and (vectorp obj)
         (eq (aref obj 0) 'class-name)))

  (defun class-name-slot1 (obj)
    (if (class-name-p obj)
        (aref obj 1)
        (error "Not a class-name instance")))

  ; ... more accessors ...

  'class-name)  ; Return class name
```

### Class Metadata

The compiler maintains a `class_metadata` hash map that stores:
- Class name → array of slot names

This enables future features like:
- Generic `slot-value` accessor
- Reflection and introspection
- Dynamic slot access

## Comparison with Common Lisp

### Supported

- ✅ `defclass` with simple slots
- ✅ Typed slots (`:type` option)
- ✅ Automatic accessor generation
- ✅ Automatic predicate generation
- ✅ Type checking and occurrence typing

### Not Yet Implemented

- ❌ Inheritance (superclasses ignored)
- ❌ Slot options: `:initform`, `:initarg`, `:accessor`, `:reader`, `:writer`
- ❌ `make-instance` with keyword arguments
- ❌ Generic `slot-value` accessor
- ❌ `defmethod` and `defgeneric` for method dispatch
- ❌ Method combinations (`:before`, `:after`, `:around`)
- ❌ `defstruct`-style keyword constructors
- ❌ Class redefinition
- ❌ Change-class protocol
- ❌ Metaclasses

## Future Directions

### Inheritance

Planned syntax:
```lisp
(defclass employee (person)
  employee-id
  department)
```

Would inherit slots from `person` and add new ones.

### Slot Options

Planned `:initform` for default values:
```lisp
(defclass counter ()
  (count :initform 0 :type fixnum))
```

### Method Dispatch

Planned `defmethod` and `defgeneric`:
```lisp
(defgeneric draw (shape))

(defmethod draw ((s circle))
  (draw-circle s))

(defmethod draw ((s rectangle))
  (draw-rectangle s))
```

### make-instance

Planned keyword argument support:
```lisp
(make-instance 'person :name "Bob" :age 25)
```

## Best Practices

### Naming Conventions

- Class names: lowercase with hyphens (`person`, `bank-account`)
- Predicates automatically use `-p` suffix (`person-p`)
- Accessors use `class-slot` format (`person-name`)

### Type Annotations

Use `:type` for better error messages and type checking:

```lisp
(defclass sized-box ()
  (width :type fixnum)
  (height :type fixnum)
  (contents :type vector))
```

### Encapsulation

Since accessors are functions, you can wrap them for validation:

```lisp
(defclass bounded-value ()
  (value :type fixnum))

(defun make-bounded (v)
  (when (or (< v 0) (> v 100))
    (error "Value out of bounds"))
  (make-bounded-value v))
```

## Testing

Run the test suite:
```bash
./zig-out/bin/habu test-clos.habu
```

The test file demonstrates:
- Class definition
- Instance creation
- Predicate checking
- Accessor usage
- Typed slots
- Nested instances

## Performance

- Class instance creation: O(n) where n is number of slots
- Slot access: O(1) vector indexing
- Type checking: O(1) predicate check
- No overhead compared to `defstruct`

## See Also

- `defstruct` - Similar simpler structure definition
- `deftype` - Algebraic data types with pattern matching
- Type system documentation
