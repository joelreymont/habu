# Types First - Make Invalid States Unrepresentable

**USE TYPES TO PREVENT BUGS AT COMPILE TIME. RUNTIME CRASHES ARE TYPE SYSTEM FAILURES.**

## Core Principle

Every bug that reaches runtime is a failure to use the type system properly. When you encounter a crash:
1. **DO NOT** just add a nil check or guard
2. **DO** ask: "What type would have prevented this?"
3. **DO** define that type and refactor to use it

## Mandatory Practices

### 1. Tagged vs Untagged Values MUST Be Distinct Types

```lisp
;; WRONG - same type for different representations
(defun make-vector (size) ...)      ; size is... tagged? untagged?
(defun vector-length (vec) ...)     ; returns... tagged? untagged?

;; RIGHT - explicit types
(deftype tagged-fixnum ...)         ; value with bit0=1, actual = val >> 1
(deftype untagged-int ...)          ; raw machine integer
(deftype tagged-ptr ...)            ; pointer | tag in low bits

(defun make-vector (size : tagged-fixnum) -> tagged-ptr ...)
(defun vector-length (vec : tagged-ptr) -> tagged-fixnum ...)
```

### 2. Optional Values MUST Use Maybe Type

```lisp
;; WRONG - nil as "not found"
(defun find-interned (name table)
  (if found symbol nil))            ; caller might forget to check!

;; RIGHT - explicit maybe type
(defun find-interned (name table) -> (maybe symbol)
  (if found (just symbol) (nothing)))

;; Caller MUST handle both cases via match
(match (find-interned name table)
  (just (sym) (use sym))
  (nothing () (handle-not-found)))
```

### 3. Function Contracts MUST Be Explicit

Before writing any function, declare:
- Input types (what tags/representations are expected)
- Output types (what tags/representations are returned)
- Preconditions (what must be true for valid input)

### 4. Memory Layout Types

Define ADTs for memory layouts so fields have known types:

```lisp
(deftype vector-layout :record
  (length tagged-fixnum)    ; NOT untagged!
  (data (array tagged-value)))

(deftype string-layout :record
  (length tagged-fixnum)
  (data (array byte)))
```

### 5. When Debugging Crashes

1. Identify the type confusion (e.g., nil where vector expected)
2. Define ADT that makes the invalid state impossible
3. Update all producers and consumers to use the ADT
4. The crash becomes a compile-time error

## Examples of Type-First Fixes

### The make-vector / make-string-from-vector Bug

**Problem**: `make-vector` stores untagged length, `make-string-from-vector` might expect tagged.

**Type-First Fix**:
```lisp
;; Define vector header type
(deftype vector-header :record
  (length untagged-int))    ; Document: length is UNTAGGED at offset 0

;; Now both codegen paths MUST agree on the type
;; Compiler catches any mismatch
```

### The nil Vector Crash

**Problem**: `make-string-from-vector` receives nil, crashes on untag.

**Type-First Fix**:
```lisp
;; Don't accept raw pointers that could be nil
(defun make-string-from-vector (vec : (non-nil tagged-ptr vector-tag))
  ...)

;; Or use maybe at the source
(defun string-upcase (s) -> (maybe string)
  (if (null s)
      (nothing)
      (just (make-string-from-vector (make-vector ...)))))
```

## The Habu Type Hierarchy

```
value
├── tagged-value (any tagged Habu value)
│   ├── tagged-fixnum (bit0=1)
│   └── tagged-ptr (bit0=0, tag in bits 1-3)
│       ├── nil-ptr (value 0)
│       ├── cons-ptr (tag 0)
│       ├── symbol-ptr (tag 2)
│       ├── vector-ptr (tag 4)
│       ├── string-ptr (tag 6)
│       ├── closure-ptr (tag 8)
│       └── keyword-ptr (tag 10)
├── untagged-int (raw machine integer)
└── untagged-ptr (raw machine pointer)
```

## Remember

**"I'll just add a nil check"** = WRONG. You're hiding a type error.

**"What type makes nil impossible here?"** = RIGHT. Fix the design.

The goal is ZERO runtime type errors. Every crash is a missing type.
