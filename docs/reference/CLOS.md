# Habu CLOS Implementation

Habu implements a subset of the Common Lisp Object System (CLOS) for object-oriented programming.

## Quick Start

```lisp
;; Define a class
(defclass point ()
  ((x :initarg :x :initform 0)
   (y :initarg :y :initform 0)))

;; Create an instance
(defparameter *p* (make-instance 'point :x 3 :y 4))

;; Access slots
(slot-value *p* 'x)  ;; => 3
(slot-value *p* 'y)  ;; => 4

;; Modify slots
(setf (slot-value *p* 'x) 10)
(slot-value *p* 'x)  ;; => 10
```

## defclass

### Syntax

```lisp
(defclass class-name (superclasses)
  ((slot-name :initarg keyword :initform default-value)
   ...))
```

### Slot Options

| Option | Description |
|--------|-------------|
| `:initarg` | Keyword argument for `make-instance` |
| `:initform` | Default value if not provided |

### Examples

```lisp
;; Simple class with two slots
(defclass person ()
  ((name :initarg :name :initform "Unknown")
   (age :initarg :age :initform 0)))

;; Class with computed default
(defclass counter ()
  ((value :initarg :value :initform 0)
   (step :initarg :step :initform 1)))
```

## make-instance

Create a new instance of a class.

### Syntax

```lisp
(make-instance 'class-name :initarg1 value1 :initarg2 value2 ...)
```

### Examples

```lisp
;; Create with all initargs
(make-instance 'person :name "Alice" :age 30)

;; Create with defaults
(make-instance 'person)  ; name="Unknown", age=0

;; Partial initialization
(make-instance 'person :name "Bob")  ; age=0
```

## slot-value

Access or modify slot values.

### Reading Slots

```lisp
(slot-value instance 'slot-name)
```

### Writing Slots

```lisp
(setf (slot-value instance 'slot-name) new-value)
```

### Examples

```lisp
(defparameter *p* (make-instance 'person :name "Alice" :age 30))

;; Read
(slot-value *p* 'name)  ;; => "Alice"
(slot-value *p* 'age)   ;; => 30

;; Write
(setf (slot-value *p* 'age) 31)
(slot-value *p* 'age)   ;; => 31
```

## class-of

Returns the class name of an object.

```lisp
(defparameter *p* (make-instance 'person))
(class-of *p*)  ;; => PERSON
```

## typep

Check if an object is of a given type.

```lisp
(defparameter *p* (make-instance 'person))
(typep *p* 'person)  ;; => T
(typep *p* 'counter) ;; => NIL
(typep 42 'fixnum)   ;; => T
```

## Generic Functions and Methods

### defgeneric

Declare a generic function.

```lisp
(defgeneric describe-object (obj))
(defgeneric area (shape))
```

### defmethod

Define a method specialized on a class.

```lisp
(defmethod describe-object ((p person))
  (format nil "Person: ~A, age ~A"
          (slot-value p 'name)
          (slot-value p 'age)))

(defmethod area ((r rectangle))
  (* (slot-value r 'width) (slot-value r 'height)))

(defmethod area ((c circle))
  (* 3.14159 (slot-value c 'radius) (slot-value c 'radius)))
```

### Implicit Generic

If you define a method without first defining the generic, it is created automatically:

```lisp
;; This works without explicit defgeneric
(defmethod speak ((a animal))
  "generic sound")

(defmethod speak ((d dog))
  "woof!")
```

### Method Dispatch

Methods are dispatched based on the class of the first argument:

```lisp
(defclass animal () ())
(defclass dog () ())
(defclass cat () ())

(defmethod speak ((a animal)) "...")
(defmethod speak ((d dog)) "woof")
(defmethod speak ((c cat)) "meow")

(speak (make-instance 'dog))  ;; => "woof"
(speak (make-instance 'cat))  ;; => "meow"
```

## Complete Example

```lisp
;; Define shape classes
(defclass shape ()
  ((color :initarg :color :initform 'black)))

(defclass rectangle ()
  ((width :initarg :width :initform 0)
   (height :initarg :height :initform 0)))

(defclass circle ()
  ((radius :initarg :radius :initform 0)))

;; Define generic function
(defgeneric area (shape))

;; Implement methods
(defmethod area ((r rectangle))
  (* (slot-value r 'width) (slot-value r 'height)))

(defmethod area ((c circle))
  (let ((r (slot-value c 'radius)))
    (* 314 r r)))  ; Using 314 for pi*100

;; Use the classes
(defparameter *rect* (make-instance 'rectangle :width 10 :height 5))
(defparameter *circ* (make-instance 'circle :radius 7))

(area *rect*)  ;; => 50
(area *circ*)  ;; => 15386
```

## Implementation Details

### Class Storage

Classes are stored in `*class-env*` at compile time:

```lisp
;; class-name -> (slot-name1 slot-name2 ...)
*class-env* = ((PERSON NAME AGE)
               (COUNTER VALUE STEP))
```

### Instance Representation

Instances are represented as tagged vectors:
- Tag: Based on class
- Slot 0: Class name symbol
- Slots 1-N: Slot values

```
Instance structure:
+--------+------------+--------+--------+
| Header | Class-name | Slot-1 | Slot-2 | ...
+--------+------------+--------+--------+
```

### Method Storage

Methods are stored in `*method-env*`:

```lisp
;; generic-name -> ((class1 . fn1) (class2 . fn2) ...)
*method-env* = ((AREA (RECTANGLE . AREA/RECTANGLE)
                      (CIRCLE . AREA/CIRCLE)))
```

### Generated Code

`defclass` generates:
1. A predicate function (`person-p`)
2. A constructor wrapper
3. Compile-time class registration

`defmethod` generates:
1. A specialized function (`method-name/class-name`)
2. Dispatcher function (if not exists)
3. Method registration in `*method-env*`

## Limitations

1. **No inheritance**: Superclass list is ignored
2. **Single dispatch**: Only first argument determines method
3. **No method combination**: No `:before`, `:after`, `:around`
4. **No slot accessors**: Must use `slot-value`
5. **No class redefinition**: Cannot redefine classes
6. **No metaclasses**: Fixed class representation

## Comparison with Full CLOS

| Feature | Habu | Full CLOS |
|---------|------|-----------|
| defclass | Basic | Full |
| Inheritance | No | Multiple |
| Slot accessors | Manual | :accessor |
| Method dispatch | Single | Multiple |
| Method combination | No | Yes |
| Metaclasses | No | Yes |
| change-class | No | Yes |
| slot-boundp | No | Yes |

## Future Enhancements

- Single inheritance support
- Automatic slot accessors (`:accessor`, `:reader`, `:writer`)
- `slot-boundp` and `slot-makunbound`
- `:before`, `:after` method qualifiers
- `call-next-method`
