# Habu Type System Analysis

This document captures analysis of type system options for achieving C-like performance and FFI capabilities in Habu.

## 1. Dependent Types Analysis

### Overview

Dependent types allow types to depend on values, enabling compile-time verification of properties that traditional type systems cannot express.

### Spectrum of Dependent Typing

| System | Expressiveness | Complexity | Examples |
|--------|---------------|------------|----------|
| Simple types | Low | Low | C, Go |
| Parametric polymorphism | Medium | Medium | Haskell, ML |
| Refinement types | High | Medium | Liquid Haskell, F* |
| Full dependent types | Very High | Very High | Idris, Agda, Coq |

### Refinement Types (Recommended for Habu)

Refinement types extend base types with logical predicates:

```
{x : int | x > 0}           -- positive integers
{a : array | length(a) > 0} -- non-empty arrays
{v : vec | sorted(v)}       -- sorted vectors
```

**Benefits for Performance:**
1. **Bounds check elimination**: `{i : int | 0 <= i < length(a)}` proves array access is safe
2. **Division safety**: `{d : int | d != 0}` eliminates division-by-zero checks
3. **Null elimination**: `{p : ptr | p != null}` removes null checks
4. **Overflow prevention**: `{x : i32 | x < MAX_I32 - y}` proves addition won't overflow

**Benefits for FFI:**
1. **C struct mapping**: Types with exact size/alignment guarantees
2. **Pointer validity**: Track allocation state in types
3. **Ownership**: Linear/affine types for memory safety
4. **ABI compliance**: Ensure calling conventions are respected

### Implementation Approaches

#### Approach A: Gradual Typing (Typed Racket Style)

```lisp
;; Untyped code works as-is
(defun add (x y) (+ x y))

;; Typed code gets checked
(defun/typed add ((x fixnum) (y fixnum)) -> fixnum
  (+ x y))

;; Contracts at boundaries
(define/contract buffer-ref
  (-> (vectorof byte) (and/c integer? (>=/c 0)) byte)
  (lambda (buf i) (aref buf i)))
```

**Pros:**
- Incremental adoption - existing code works
- No "big bang" rewrite needed
- Types are documentation that's checked
- Can start untyped, add types for hot paths

**Cons:**
- Runtime contract checks at typed/untyped boundaries
- Can't optimize across boundaries
- "Blamed" errors can be confusing

#### Approach B: Optional Type Annotations (Julia Style)

```lisp
;; Without types - fully dynamic
(defun add (x y) (+ x y))

;; With types - specialized compilation
(defun add ((x fixnum) (y fixnum))
  (declare (values fixnum))
  (the fixnum (+ x y)))
```

**Pros:**
- Simpler than contracts
- Direct performance benefit
- Familiar to CL programmers (already have `declare`, `the`)

**Cons:**
- Types are hints, not guarantees (unless we enforce)
- No automatic inference
- Less expressive than refinements

#### Approach C: Full Refinement Types (Liquid Haskell Style)

```lisp
;; Refinement annotations
(defun vector-sum ((v (vec fixnum))
                   (lo (idx v))
                   (hi (and (idx v) (>= lo))))
  (declare (returns fixnum))
  ...)

;; SMT solver verifies bounds
(defun binary-search ((v (sorted-vec comparable))
                      (target comparable))
  (declare (returns (or (idx v) null)))
  ...)
```

**Pros:**
- Maximum safety guarantees
- Can verify complex invariants
- Enables aggressive optimization

**Cons:**
- Requires SMT solver integration (Z3)
- Steep learning curve
- Annotations can be verbose
- Inference is incomplete

### Recommendation: Phased Implementation

**Phase 1: Honor CL Declarations (Immediate)**
- Parse existing `declare` forms
- Use for optimization (unbox fixnums, inline primitives)
- No new syntax needed

**Phase 2: Simple Refinements (Near-term)**
- Add `(declare (refine x (> x 0)))` syntax
- Check at function boundaries
- Use for bounds check elimination

**Phase 3: Full Contracts (Long-term)**
- Typed/untyped boundaries with blame tracking
- Dependent function types
- Optional SMT verification

### Implementation Location

**Both pipelines should share the type checker:**

```
Source -> [Expand] -> [Type Check] -> [Compile] -> IR -> ARM64
              ^            ^
              |            |
         expand.lisp   types.lisp (new shared file)
```

The type checker should be:
1. A pure source-to-source pass (like expand.lisp)
2. Shared between SBCL bootstrap and native Habu
3. Optional - untyped code compiles without it

### FFI Considerations

For C interop, we need:

```lisp
;; Foreign type declarations
(defctype size_t :uint64)
(defctype c-string (ptr :char :null-terminated t))

;; Struct with exact layout
(defcstruct point
  (x :double :offset 0)
  (y :double :offset 8))

;; Function with C calling convention
(defcfun ("malloc" c-malloc) (ptr :void)
  (size size_t))
```

This requires:
1. Exact control over memory layout
2. Understanding of C ABI (already have for ARM64)
3. Type annotations that map to C types

---

## 2. Refinement Types vs CL Type Annotations

### CL Type Annotation System

Common Lisp has a built-in type annotation system:

```lisp
;; Type declarations
(declare (type fixnum x))
(declare (ftype (function (fixnum fixnum) fixnum) add))

;; Type specifiers
(integer 0 255)              ; bounded integers
(simple-array fixnum (100))  ; typed arrays
(values fixnum &optional)    ; return types
```

### CL Type Specifiers

| Specifier | Meaning |
|-----------|---------|
| `(integer low high)` | Bounded integers |
| `(float low high)` | Bounded floats |
| `(simple-array element-type dims)` | Typed arrays with dimensions |
| `(member a b c)` | Enumeration types |
| `(satisfies predicate)` | Predicate-based types |
| `(and type1 type2)` | Intersection types |
| `(or type1 type2)` | Union types |
| `(not type)` | Complement types |

### What CL Types CAN Express

1. **Bounded numeric ranges**: `(integer 0 255)` expresses non-negative bytes
2. **Non-null values**: `(and string (not null))`
3. **Array dimensions**: `(simple-array t (10 20))` for 10x20 arrays
4. **Enumerations**: `(member :red :green :blue)`
5. **Predicate types**: `(satisfies evenp)` for even numbers

### What CL Types CANNOT Express

1. **Relational constraints**: `{x : int | x < y}` - cannot reference other variables
2. **Data structure invariants**: `{v : vector | sorted?(v)}` - complex predicates
3. **Length relationships**: `{result : list | length(result) = length(input)}`
4. **Arithmetic relationships**: `{q : int | q * d <= n}` - inter-variable math

### Compatibility Analysis

Refinement types are a **strict superset** of CL types:

| CL Type | Refinement Equivalent |
|---------|----------------------|
| `(integer 0 255)` | `{x : fixnum \| 0 <= x && x <= 255}` |
| `(satisfies evenp)` | `{x : fixnum \| even?(x)}` |
| `(member :a :b)` | `{x : symbol \| x = :a \|\| x = :b}` |
| `(simple-array fixnum (n))` | `{a : array \| elem-type(a) = fixnum && len(a) = n}` |

**Key insight**: Every CL type specifier can be expressed as a refinement type, but not vice versa.

### Sufficiency Analysis

**For C-like performance, CL types are MOSTLY sufficient:**

| Optimization | CL Type Support | Refinement Adds |
|--------------|-----------------|-----------------|
| Unbox fixnums | Yes (`fixnum`) | Nothing |
| Inline array access | Yes (`simple-array`) | Nothing |
| Eliminate null checks | Yes (`(not null)`) | Nothing |
| Eliminate bounds checks | Partial (`(integer 0 n)`) | Full relational (`i < len`) |
| Prove no overflow | Partial (bounded integers) | Full arithmetic relations |
| Verify sorting | No | Yes (`sorted?` predicate) |

**For FFI, CL types are MOSTLY sufficient:**

| FFI Need | CL Type Support | Refinement Adds |
|----------|-----------------|-----------------|
| Fixed-size integers | Yes (`(signed-byte 32)`) | Nothing |
| Pointer types | Partial (need extension) | Validity tracking |
| Struct layout | Need extension | Nothing extra |
| Array bounds | Yes | Safer guarantees |
| Null-terminated strings | Need extension | Length proofs |

### Recommendation

**Use CL types as foundation, add refinements incrementally:**

```lisp
;; Phase 1: CL-compatible (works today)
(defun safe-divide (x y)
  (declare (type fixnum x y))
  (declare (type (integer 1 *) y))  ; y > 0
  (floor x y))

;; Phase 2: Simple refinements (optional extension)
(defun binary-search (vec target)
  (declare (type (simple-vector fixnum) vec))
  (declare (refine (> (length vec) 0)))  ; non-empty
  ...)

;; Phase 3: Full refinements (advanced use)
(defun matrix-multiply (a b)
  (declare (refinement a (array-2d m k)))
  (declare (refinement b (array-2d k n)))
  (declare (refinement :returns (array-2d m n)))
  ...)
```

### Implementation Priority

1. **Immediate**: Parse and honor `(declare (type ...))` for optimization
2. **Near-term**: Add `(declare (refine ...))` for simple predicates
3. **Long-term**: Full dependent types with SMT solving

### Conclusion

CL type annotations provide 80% of the benefit for 20% of the complexity. They should be the foundation. Refinement types can be added as an optional extension for users who need stronger guarantees, but should not be required for good performance.

---

## Summary

| Feature | CL Types | Refinement Types | Full Dependent |
|---------|----------|------------------|----------------|
| Complexity | Low | Medium | High |
| Performance benefit | High | Higher | Highest |
| FFI support | Good | Better | Best |
| Learning curve | Familiar | Moderate | Steep |
| Implementation effort | Low | Medium | Very High |

**Recommended path for Habu:**
1. Start with CL types (already in spec, familiar syntax)
2. Add simple refinements for bounds/null checks
3. Consider full dependent types only if needed for specific use cases

This approach provides good performance and FFI capabilities while maintaining Lisp's dynamic nature where desired.
