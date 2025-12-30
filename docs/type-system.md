# Habu Type System

A comprehensive guide to Habu's type system, from basics to dependent types.

## Table of Contents

1. [What is a Type System?](#what-is-a-type-system)
2. [The Basics: Simple Types](#the-basics-simple-types)
3. [Union Types](#union-types)
4. [Function Types](#function-types)
5. [Occurrence Typing](#occurrence-typing)
6. [Dependent Types](#dependent-types)
7. [Refinement Types](#refinement-types)
8. [Quantitative Types (QTT)](#quantitative-types-qtt)
9. [Gradual Typing: The Escape Hatch](#gradual-typing-the-escape-hatch)
10. [Architecture](#architecture)
11. [Examples](#examples)
12. [Further Reading](#further-reading)

---

## What is a Type System?

A **type system** is like a spell-checker for your code. Just as a spell-checker catches misspelled words before you send an email, a type system catches certain kinds of bugs before your program runs.

### The Problem Types Solve

Consider this code:

```lisp
(+ "hello" 5)  ; Adding a string to a number?!
```

Without types, this would crash at runtime. With types, the compiler catches it immediately:

```
Type error: + expects (fixnum, fixnum), got (string, fixnum)
```

### Types as Contracts

Think of types as contracts. When you write:

```lisp
(defun add ((a fixnum) (b fixnum)) -> fixnum
  (+ a b))
```

You're making a promise: "Give me two integers, and I'll give you back an integer." The type checker ensures you keep that promise.

---

## The Basics: Simple Types

Habu has these **primitive types**:

| Type | What it holds | Examples |
|------|---------------|----------|
| `fixnum` | 63-bit integers | `42`, `-7`, `0` |
| `cons` | A pair of values | `(1 . 2)`, `(a b c)` |
| `symbol` | An identifier | `foo`, `my-var` |
| `string` | Text | `"hello"`, `""` |
| `vector` | Fixed-size array | `#(1 2 3)` |
| `closure` | A function | `(lambda (x) x)` |
| `keyword` | Keyword symbol | `:key`, `:test` |
| `nil` | Empty/false | `nil`, `()` |

### Type Hierarchy

```
                       any (top type)
                        │
        ┌───────────────┼───────────────┐
        │               │               │
     fixnum          object         closure
                        │
        ┌───┬───┬───┬───┼───┬───┐
        │   │   │   │   │   │   │
      cons sym vec str kwd nil  t
```

- **`any`** (also written `t`): The "top" type - matches anything
- **`nil`**: The "bottom" for objects - represents empty/false

---

## Union Types

What if a variable could be one of several types?

```lisp
;; x could be a fixnum OR a string
(defun describe ((x (or fixnum string))) -> string
  (if (fixnump x)
      (format nil "number: ~a" x)
      (format nil "text: ~a" x)))
```

The `(or T1 T2 ...)` syntax creates a **union type**. The value must be one of the listed types.

### Common Union Types

```lisp
;; Boolean: either nil (false) or anything truthy
(or nil t)

;; A list is either empty (nil) or a cons
(or nil cons)

;; Optional value: either the value or nil
(or string nil)  ; "string or nothing"
```

---

## Function Types

Functions have types too! The arrow `->` type describes inputs and output:

```lisp
;; Takes two fixnums, returns a fixnum
(-> (fixnum fixnum) fixnum)

;; Takes a string, returns its length
(-> (string) fixnum)

;; Takes a predicate and list, returns filtered list
(-> ((-> (t) (or nil t)) (list t)) (list t))
```

### Reading Function Types

```
(-> (T1 T2 ... Tn) R)
     └─────┬─────┘  │
     input types   return type
```

Example:
```lisp
(defun string-length ((s string)) -> fixnum ...)
;; Type: (-> (string) fixnum)
;; "Takes a string, returns a fixnum"
```

---

## Occurrence Typing

Habu narrows types based on runtime checks. This is called **occurrence typing** (pioneered by Typed Racket).

### How It Works

```lisp
(defun process ((x (or fixnum string))) -> fixnum
  (if (fixnump x)
      ;; HERE: x is known to be fixnum (narrowed!)
      (+ x 1)
      ;; HERE: x is known to be string (the other possibility)
      (string-length x)))
```

The type checker tracks what you've tested:

```
                    x : (or fixnum string)
                           │
                    (fixnump x)?
                    /            \
                  yes             no
                  /                \
          x : fixnum           x : string
```

### Type Predicates

These predicates trigger narrowing:

| Predicate | Narrows to |
|-----------|------------|
| `(fixnump x)` | `fixnum` |
| `(consp x)` | `cons` |
| `(stringp x)` | `string` |
| `(symbolp x)` | `symbol` |
| `(vectorp x)` | `vector` |
| `(null x)` | `nil` |

---

## Dependent Types

This is where Habu gets interesting! **Dependent types** let types depend on *values*.

### The Problem with Simple Types

Consider `nth` which gets the nth element of a list:

```lisp
(nth 5 '(a b c))  ; Index 5, but only 3 elements!
```

With simple types, we can only say:
```lisp
(defun nth ((n fixnum) (xs (list t))) -> t ...)
```

This can't prevent out-of-bounds access.

### Pi Types (Π): Dependent Functions

**Pi types** let the return type depend on the input value:

```lisp
;; Vector indexed by length
(pi (n : nat) (vector a n))
;; "For any natural number n, a vector of length n"
```

Think of it as: "Given a value `n`, the type is `(vector a n)`"

```
Regular function:   (-> A B)         ; B is fixed
Dependent function: (pi (x : A) B)   ; B can mention x
```

### Sigma Types (Σ): Dependent Pairs

**Sigma types** are pairs where the second type depends on the first value:

```lisp
;; A vector together with its length
(sigma (n : nat) (vector a n))
;; "A pair of: a number n, and a vector of exactly n elements"
```

This is powerful for encoding invariants:

```lisp
;; A non-empty list: the length AND the list
(sigma (n : (refine nat x (> x 0))) (vector a n))
```

### Visualization

```
┌─────────────────────────────────────────────────┐
│               SIMPLE TYPES                       │
│                                                  │
│   (-> A B)     Input: A, Output: B (fixed)      │
│   (A . B)      First: A, Second: B (fixed)      │
└─────────────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────┐
│              DEPENDENT TYPES                     │
│                                                  │
│   (pi (x : A) B)    Output B can use x          │
│   (sigma (x : A) B) Second B can use x          │
│                                                  │
│   Example:                                       │
│   (pi (n : nat) (vector string n))              │
│   = "for any n, return a vector of n strings"   │
└─────────────────────────────────────────────────┘
```

---

## Refinement Types

**Refinement types** add logical predicates to types:

```lisp
(refine T x P)
;; "Values of type T where predicate P holds"
```

### Examples

```lisp
;; Positive integers
(refine fixnum x (> x 0))

;; Non-empty lists
(refine (list a) xs (not (null xs)))

;; Valid array index
(refine fixnum i (and (>= i 0) (< i (length arr))))
```

### How It Works

The type checker uses an **SMT solver** (Z3) to verify predicates:

```
┌──────────────────────────────────────────────────┐
│                  Your Code                        │
│   (defun safe-div ((a fixnum)                    │
│                    (b (refine fixnum x (> x 0))))│
│       (/ a b))                                   │
└───────────────────────┬──────────────────────────┘
                        │
                        ▼
┌──────────────────────────────────────────────────┐
│               Type Checker                        │
│   "Is (> x 0) satisfiable? Can we prove         │
│    this predicate is always true here?"          │
└───────────────────────┬──────────────────────────┘
                        │
                        ▼
┌──────────────────────────────────────────────────┐
│            Z3 SMT Solver                          │
│   ✓ Valid: predicate always holds               │
│   ✗ Invalid: found counterexample               │
│   ? Unknown: couldn't determine                  │
└──────────────────────────────────────────────────┘
```

### Subtyping with Refinements

Refinement types form a subtyping hierarchy:

```lisp
;; Stronger refinement is subtype of weaker
(refine fixnum x (and (> x 0) (< x 10)))  ; 1-9
    <:
(refine fixnum x (> x 0))                  ; positive
    <:
fixnum                                      ; any integer
```

"Stronger constraints → smaller set → subtype"

---

## Quantitative Types (QTT)

**Quantitative Type Theory** tracks *how many times* values are used:

| Quantity | Meaning | Use Case |
|----------|---------|----------|
| `0` | Never used at runtime | Proofs, type indices |
| `1` | Used exactly once | Linear resources |
| `ω` (many) | Used any number of times | Normal values |

### Why Track Usage?

1. **Proof Erasure**: 0-quantity terms disappear at runtime (no overhead for types!)
2. **Resource Management**: 1-quantity ensures files, connections are closed exactly once
3. **Optimization**: Compiler knows what can be inlined/erased

### Syntax

```lisp
;; 0-quantity: erased at runtime (proof-only)
(pi (0 n : nat) (vector a n))

;; 1-quantity: must use exactly once
(pi (1 handle : file) (io unit))

;; ω-quantity (default): unrestricted
(pi (x : fixnum) fixnum)
```

### Example: Safe Resource Handling

```lisp
;; File handle must be used exactly once (closed exactly once)
(defun with-file ((1 path : string)
                  (1 f : (-> (1 handle : file) a))) -> a
  (let ((1 h (open path)))
    (f h)))  ; f must close h
```

---

## Gradual Typing: The Escape Hatch

What if you don't want to type everything? The `any` type is your escape hatch.

### The `any` Type

```lisp
any  ; or equivalently: t
```

`any` means "I don't know/care about the type." It:
- Accepts any value
- Can be passed where any type is expected
- Defers checking to runtime

### When to Use `any`

```lisp
;; Prototyping - figure out types later
(defun work-in-progress ((x any)) -> any
  (process x somehow))

;; Interop with untyped code
(defun call-legacy ((args any)) -> any
  (legacy-function args))

;; Dynamic dispatch when type isn't known statically
(defun eval ((expr any) (env any)) -> any
  ...)
```

### Gradual Guarantees

Habu provides **gradual guarantees**:

1. **Fully typed code**: All type errors caught at compile time
2. **Fully untyped code**: Runs like dynamic Lisp (errors at runtime)
3. **Mixed code**: Boundary checks at typed/untyped interfaces

```
┌─────────────────┐     ┌─────────────────┐
│   Typed Code    │────▶│  Untyped Code   │
│ (compile-time   │     │  (runtime       │
│  checking)      │◀────│   checking)     │
└─────────────────┘     └─────────────────┘
         │                      │
         └──────┬───────────────┘
                │
         Contract Boundary
         (runtime type check)
```

### Blame Tracking

When typed/untyped code interact, Habu tracks **blame** for errors:

```lisp
;; Typed function
(defun typed-add ((a fixnum) (b fixnum)) -> fixnum
  (+ a b))

;; Untyped code calls it wrong
(typed-add "oops" 5)
;; Error: Contract violation
;; typed-add expected fixnum, got string
;; Blame: caller (untyped code)
```

---

## Architecture

Here's how the type system is implemented:

```
┌──────────────────────────────────────────────────────────────┐
│                        SOURCE CODE                            │
│                    (defun foo ...)                            │
└─────────────────────────────┬────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                          PARSER                               │
│              S-expressions → Internal AST                     │
└─────────────────────────────┬────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                    TYPE CHECKER (BiChecker)                   │
│  ┌────────────────────────────────────────────────────────┐  │
│  │ Bidirectional Type Checking                             │  │
│  │  • infer(expr) → Type    (bottom-up: what type is it?) │  │
│  │  • check(expr, Type) → ok/error   (top-down: is it T?) │  │
│  └────────────────────────────────────────────────────────┘  │
│                              │                                │
│  ┌───────────────┐  ┌───────┴───────┐  ┌─────────────────┐   │
│  │  Normalizer   │  │  Conversion   │  │    Inference    │   │
│  │ (simplify     │  │  (are types   │  │  (unification,  │   │
│  │  type-level   │  │   equal?)     │  │   constraints)  │   │
│  │  computation) │  │               │  │                 │   │
│  └───────────────┘  └───────────────┘  └─────────────────┘   │
│                              │                                │
│  ┌──────────────────────────────────────────────────────────┐│
│  │                 SMT Solver (Z3)                           ││
│  │  • Refinement predicate checking                         ││
│  │  • Subtyping with predicates                             ││
│  │  • Counterexample generation                             ││
│  └──────────────────────────────────────────────────────────┘│
└─────────────────────────────┬────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                    CONTRACT COMPILER                          │
│       Insert runtime checks at typed/untyped boundaries       │
└─────────────────────────────┬────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                       QTT ERASURE                             │
│              Remove 0-quantity (proof) terms                  │
└─────────────────────────────┬────────────────────────────────┘
                              │
                              ▼
┌──────────────────────────────────────────────────────────────┐
│                    BYTECODE / NATIVE                          │
│                 Final executable code                         │
└──────────────────────────────────────────────────────────────┘
```

### Key Components

| Component | File | Purpose |
|-----------|------|---------|
| Type ADT | `src/types/type.zig` | Type representation (Pi, Sigma, Refinement, etc.) |
| Term ADT | `src/types/term.zig` | Type-level computation terms |
| BiChecker | `src/types/bicheck.zig` | Bidirectional type checking |
| Normalizer | `src/types/normalize.zig` | Reduce type-level terms |
| Conversion | `src/types/conversion.zig` | Type equality checking |
| SMT | `src/types/smt.zig` | Z3 integration for refinements |
| Contract | `src/types/contract.zig` | Runtime check generation |
| Blame | `src/types/blame.zig` | Error tracking for contracts |

---

## Examples

### Example 1: Safe Division

```lisp
;; Division that can't fail (b is guaranteed positive)
(defun safe-div ((a fixnum) (b (refine fixnum x (> x 0)))) -> fixnum
  (/ a b))

;; Usage
(safe-div 10 2)   ; OK: 2 > 0
(safe-div 10 0)   ; Type error: 0 doesn't satisfy (> x 0)
```

### Example 2: Length-Indexed Vectors

```lisp
;; Vector type indexed by length
(deftype (vector a n) ...)

;; Concatenate vectors - lengths add up!
(defun concat ((m : nat) (n : nat)
               (xs : (vector a m))
               (ys : (vector a n))) -> (vector a (+ m n))
  ...)

;; Safe head - only works on non-empty vectors
(defun head ((n : (refine nat x (> x 0)))
             (xs : (vector a n))) -> a
  (vector-ref xs 0))
```

### Example 3: Gradual Typing in Action

```lisp
;; Start untyped
(defun process (x)
  (if (numberp x)
      (+ x 1)
      x))

;; Add types gradually
(defun process ((x any)) -> any  ; First pass: any
  ...)

(defun process ((x (or fixnum string))) -> (or fixnum string)  ; Refined
  ...)

(defun process ((x fixnum)) -> fixnum  ; Fully typed
  (+ x 1))
```

### Example 4: Linear Resources

```lisp
;; File must be closed exactly once
(defun with-open-file ((1 path : string)
                       (f : (-> (1 file) a))) -> a
  (let ((1 handle (open-file path)))
    (unwind-protect
      (f handle)
      (close handle))))

;; This ensures the file is:
;; - Opened exactly once
;; - Passed to f
;; - Closed exactly once (even on error)
```

---

## Further Reading

### Type Theory Foundations

- **"Types and Programming Languages"** by Benjamin Pierce - The definitive textbook
- **"Software Foundations"** (online) - Interactive Coq-based introduction
- **"Practical Foundations for Programming Languages"** by Robert Harper

### Dependent Types

- **"The Little Typer"** by Friedman & Christiansen - Gentle introduction
- **"Type-Driven Development with Idris"** by Edwin Brady - Practical dependent types
- **"Certified Programming with Dependent Types"** (online) - Coq/formal methods

### Quantitative Type Theory

- **"I Got Plenty o' Nuttin'"** by Conor McBride - QTT paper
- **"Idris 2: Quantitative Type Theory in Practice"** by Brady - Implementation details

### Gradual Typing

- **"Gradual Typing for Functional Languages"** by Siek & Taha - Original paper
- **Typed Racket documentation** - Practical gradual typing

### Refinement Types

- **"Liquid Types"** by Rondon, Kawaguchi & Jhala - Refinement type inference
- **"Refinement Types for ML"** by Freeman & Pfenning - Foundations
- **"LiquidHaskell"** - Refinement types for Haskell (practical)

### SMT Solvers

- **"SAT/SMT by Example"** by Dennis Yurichev - Free online book
- **Z3 Tutorial** - Official Z3 documentation
- **"Decision Procedures"** by Kroening & Strichman - Theory behind SMT

---

## Quick Reference

### Type Syntax

```lisp
;; Primitive types
fixnum, cons, symbol, string, vector, closure, keyword, nil

;; Union type
(or T1 T2 ...)

;; Function type
(-> (T1 T2 ...) R)

;; List type
(list T)

;; Pi type (dependent function)
(pi (x : A) B)
(forall (x : A) B)  ; alias

;; Sigma type (dependent pair)
(sigma (x : A) B)
(exists (x : A) B)  ; alias

;; Refinement type
(refine T x P)      ; {x : T | P}

;; Quantified types (usage)
(pi (0 x : A) B)    ; erased
(pi (1 x : A) B)    ; linear
(pi (x : A) B)      ; unrestricted (default)

;; Escape hatch
any                 ; or: t
```

### Type Predicates

```lisp
(fixnump x)    ; x is fixnum?
(consp x)      ; x is cons?
(stringp x)    ; x is string?
(symbolp x)    ; x is symbol?
(vectorp x)    ; x is vector?
(null x)       ; x is nil?
(keywordp x)   ; x is keyword?
```

### Type Assertions

```lisp
(the T expr)           ; assert expr has type T
(unsafe-cast T expr)   ; bypass type checking (dangerous!)
```

Examples with compound types:

```lisp
;; Refinement type - checks predicate at runtime
(the (refine fixnum x (> x 0)) 5)    ; => 5 (valid: 5 > 0)
(the (refine fixnum x (> x 0)) -5)   ; => type mismatch error

;; Union type
(the (or fixnum nil) 42)             ; => 42

;; List type
(the (list fixnum) '(1 2 3))         ; => (1 2 3)

;; Function type
(the (-> (fixnum) fixnum) (lambda (x) (+ x 1)))  ; => #<closure>
```
