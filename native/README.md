# Habu0 Type System

This directory contains the type system for habu0 self-hosting.

## Files

- **types.lisp** - Type system implementation compatible with habu0 primitives
- **types-test.lisp** - Test suite demonstrating usage
- **arm64-asm.lisp** - ARM64 assembly encoding (planned, using types.lisp)

## Overview

The type system provides:
- `habu-deftype` macro for defining algebraic data types (ADTs)
- `habu-match` macro for exhaustive pattern matching
- Automatic generation of constructors, predicates, and accessors

## Usage

### Defining Types

```lisp
;; Sum type with prefix
(habu-deftype ir-node :prefix ir
  (lit value)              ; Constructor: ir-lit
  (var offset)             ; Constructor: ir-var
  (add left right)         ; Constructor: ir-add
  (if cond then else))     ; Constructor: ir-if
```

This generates:
- **Constructors**: `(ir-lit 42)` → `(:IR-LIT 42)`
- **Predicates**: `ir-lit-p`, `ir-var-p`, etc., plus type-level `ir-node-p`
- **Accessors**: `ir-lit-value`, `ir-add-left`, `ir-add-right`, etc.

### Pattern Matching

```lisp
(defun eval-ir (node)
  (habu-match ir-node node
    (lit (value)
         value)
    (var (offset)
         (lookup-var offset))
    (add (left right)
         (+ (eval-ir left) (eval-ir right)))
    (if (cond then else)
        (if (eval-ir cond)
            (eval-ir then)
            (eval-ir else)))))
```

### Key Features

- **Prefix support**: Use `:prefix` to avoid repeating common prefixes
- **Short patterns**: With prefix, match patterns use short names (`lit` not `ir-lit`)
- **Runtime checks**: Missing match arms result in runtime error
- **Simple representation**: Types are just tagged lists (easy to inspect/debug)

## Habu0 Compatibility

The type system uses only habu0-compatible primitives:

**List operations**: cons, car, cdr, null, consp, assoc, member, reverse, nth

**String operations**: string-concat, symbol-name, make-symbol-from-string

**Control flow**: if, cond, progn, let, defun, defmacro

**No dependencies**: Hash-tables, format, CLOS, defstruct

## Implementation Notes

### Type Registry

Types are registered in `*habu-type-registry*` (an alist):
```lisp
'((type-name . (:kind :sum
                :prefix ir
                :variants (ir-lit ir-var ir-add ir-if)
                :short-names (lit var add if)))
  ...)
```

### Tagged Lists

Types are represented as cons cells:
```lisp
(:IR-LIT . (42))           ; ir-lit value
(:IR-ADD . (left right))   ; ir-add left right
```

The keyword tag enables O(1) dispatch with `eq`.

### Match Expansion

`habu-match` expands to nested `if`/`eq` chains:
```lisp
(habu-match ir-node x
  (lit (v) ...)
  (var (o) ...))

;; Expands to:
(let ((#:VAL x))
  (if (eq (car #:VAL) :IR-LIT)
      (let ((v (nth 0 (cdr #:VAL))))
        ...)
      (if (eq (car #:VAL) :IR-VAR)
          (let ((o (nth 0 (cdr #:VAL))))
            ...)
          (error "match: unhandled case"))))
```

## SBCL Bootstrap

For testing with SBCL, types.lisp includes shims for habu0 primitives:
- `make-symbol-from-string` → `make-symbol`
- `string-concat` → `concatenate`
- etc.

These shims are conditionally compiled with `#+sbcl` and won't be included in native habu0 builds.

## Testing

Run tests with SBCL:
```bash
sbcl --load native/types.lisp --load native/types-test.lisp --eval '(run-tests)'
```

Expected output:
```
"Testing constructors:"
(:IR-LIT 42)
(:IR-VAR 0)
(:IR-ADD (:IR-LIT 1) (:IR-LIT 2))
"Testing predicates:"
T
NIL
T
T
"Testing accessors:"
42
(:IR-LIT 1)
(:IR-LIT 2)
"Testing match:"
42
42
100
"All tests complete!"
```

## Future Work

- Compile-time exhaustiveness checking (currently runtime only)
- Record types (product types with named fields)
- Enum types (simple value sets)
- Type aliases
- Better error messages (with context)

## Design Rationale

### Why not defstruct?

defstruct is a CL facility not available in habu0. Tagged lists provide:
- Simple implementation (no complex bootstrapping)
- Easy debugging (human-readable)
- Natural pattern matching
- Minimal memory overhead

### Why keywords for tags?

Keywords are:
- Self-evaluating (no quoting needed)
- Globally unique (no package conflicts)
- Fast to compare with `eq`
- Human-readable in REPL/debugging

### Why separate macros (habu-deftype, habu-match)?

Avoids naming conflicts with CL:DEFTYPE during SBCL bootstrap testing. In pure habu0, these would just be `deftype` and `match`.
