# Habu Minimal Runtime - Implementation Complete

## Overview

Successfully implemented a truly minimal C runtime for Habu Lisp, following the SBCL philosophy where the C layer provides only fundamental primitives and everything else is implemented in Lisp.

## What Was Removed from C Runtime

### Type Predicates (Now in Lisp)
- `fixnum?` - Check if value is a fixnum
- `cons?` - Check if value is a cons cell
- `symbol?` - Check if value is a symbol
- `string?` - Check if value is a string
- `vector?` - Check if value is a vector
- `nil?` - Check if value is nil

### Comparison Functions (Now in Lisp)
- `string=?` - Compare two strings for equality
- `symbol=?` - Compare two symbols for equality

## What Was Added to C Runtime

### New Primitive
- `habu_get_tag(val)` - Returns the type tag (0-5) of any value
  - Exposes the underlying tagged pointer system to Lisp code
  - Enables type checking in Lisp without C support

### Tag Constants
```c
TAG_FIXNUM  = 0x0 (0)
TAG_CONS    = 0x1 (1)
TAG_SYMBOL  = 0x2 (2)
TAG_VECTOR  = 0x3 (3)
TAG_STRING  = 0x4 (4)
TAG_CLOSURE = 0x5 (5)
```

### New Helper Primitive
- `string-length-raw` - Get length of Habu string objects
  - Used by Lisp code to implement string comparison

## Lisp Implementations

### Type Predicates (`predicates.lisp`)
```lisp
(defun fixnum? (x)
  (= (get-tag x) (quote 0)))

(defun cons? (x)
  (= (get-tag x) (quote 1)))

(defun symbol? (x)
  (= (get-tag x) (quote 2)))

(defun vector? (x)
  (= (get-tag x) (quote 3)))

(defun string? (x)
  (= (get-tag x) (quote 4)))

(defun nil? (x)
  (= x (quote 0)))
```

### String Comparison (`comparisons.lisp`)
```lisp
(defun string-compare-loop (s1 s2 idx len)
  (if (>= idx len)
      (quote 1)
      (if (= (string-ref s1 idx) (string-ref s2 idx))
          (string-compare-loop s1 s2 (+ idx (quote 1)) len)
          (quote nil))))

(defun string=? (s1 s2)
  (if (string? s1)
      (if (string? s2)
          (let ((len1 (string-length-raw s1)))
            (let ((len2 (string-length-raw s2)))
              (if (= len1 len2)
                  (string-compare-loop s1 s2 (quote 0) len1)
                  (quote nil))))
          (quote nil))
      (quote nil)))

(defun symbol=? (sym1 sym2)
  (if (symbol? sym1)
      (if (symbol? sym2)
          (string=? (symbol-name sym1) (symbol-name sym2))
          (quote nil))
      (quote nil)))
```

## Minimal C Runtime - Final Architecture

The C runtime now contains ONLY these categories of functions:

### 1. Memory Management
- `habu_init()` - Initialize GC and heap
- `habu_gc_alloc_slow()` - Allocate with GC
- `habu_gc_collect()` - Trigger garbage collection
- GC root management functions

### 2. Object Creation
- `habu_cons(car, cdr)` - Create cons cell
- `habu_make_vector(size)` - Create vector
- `habu_make_string(data, len)` - Create string
- `habu_make_symbol(name)` - Create symbol
- `habu_make_closure(code, env)` - Create closure

### 3. Field Access
- `habu_car(cons)` - Get car of cons
- `habu_cdr(cons)` - Get cdr of cons
- `habu_vector_ref(vec, idx)` - Get vector element
- `habu_string_ref(str, idx)` - Get character at index
- `habu_symbol_name(sym)` - Get symbol name
- `habu_string_length_raw(str)` - Get string length
- `habu_closure_code(closure)` - Get closure code pointer
- `habu_closure_env(closure)` - Get closure environment

### 4. Type Introspection
- `habu_get_tag(val)` - **NEW** Get type tag (0-5)

### 5. Arithmetic
- `+`, `-`, `*`, `/` - Basic arithmetic
- `=`, `<`, `>`, `<=`, `>=` - Comparisons

### 6. I/O
- `habu_print_value(val)` - Print value
- `habu_fgets_line()` - Read line from stdin
- File I/O functions (open, close, read, write)

## Compiler Backend Improvements

### Enhanced `sanitize-c-name`
Improved C name generation to handle Lisp naming conventions:
- `-` → `_` (hyphen to underscore)
- `?` → `_P` (predicate suffix)
- `=` → `_EQ` (equality)
- `+` → `_PLUS`
- `*` → `_STAR`
- `/` → `_SLASH`
- `<` → `_LT`
- `>` → `_GT`
- `!` → `_BANG`

### New Code Generation
- Added support for `get-tag` primitive
- Added support for `string-length-raw` primitive
- Removed code generation for type predicates (now Lisp functions)
- Removed code generation for string/symbol comparisons (now Lisp functions)

## Files Created

- `predicates.lisp` - Type predicates implemented in Lisp
- `comparisons.lisp` - String/symbol comparisons in Lisp
- `repl-truly-minimal.lisp` - Complete REPL with minimal runtime
- `habu-minimal.c` - Generated C code from minimal REPL
- `habu-minimal` - Compiled executable (56KB)

## Build and Test

```bash
# Compile the minimal runtime REPL
sbcl --script /tmp/compile-truly-minimal.lisp

# Build executable
gcc -o habu-minimal habu-minimal.c runtime/*.c -Iruntime -O2

# Check size
ls -lh habu-minimal
# Output: 56K habu-minimal
```

## Benefits of Minimal Runtime

### 1. Following SBCL Best Practices
- Minimal C code to maintain
- Type system exposed to Lisp level
- Maximum flexibility for future changes

### 2. Self-Hosting Foundation
- Type checking in Lisp (can modify without recompiling C)
- String operations in Lisp
- Easy to extend and experiment

### 3. Educational Value
- Clear separation of concerns
- Demonstrates tagged pointer architecture
- Shows how Lisp can be built from minimal primitives

### 4. Code Quality
- Less C code = fewer bugs
- More code in Lisp = easier to understand and modify
- Better match with Lisp philosophy

## Next Steps

1. **Fix REPL bugs** - The reader/evaluator have some issues to fix
2. **Optimize Lisp implementations** - String comparison could use optimizations
3. **Add more Lisp implementations** - Move more functions from C to Lisp
4. **Performance testing** - Compare minimal runtime vs full runtime
5. **Documentation** - Document all primitives and their contracts

## Commit

```
commit 512312a
Author: Claude
Date: 2025-11-19

Implement minimal C runtime following SBCL model

- Add get-tag primitive to expose type tag checking to Lisp
- Remove type predicates from C runtime
- Remove comparison functions from C runtime
- Implement type predicates in Lisp using get-tag
- Implement string/symbol comparisons in Lisp
- Improve sanitize-c-name for special characters

The C runtime is now absolutely minimal with only fundamental
primitives. All type checking and comparisons are in Lisp.
```

## Conclusion

Successfully transformed Habu's runtime from a traditional C-heavy approach to a minimal runtime philosophy matching SBCL's design. The C layer now provides only the essential primitives needed for memory management, object creation/access, and I/O, while all type checking and higher-level operations are implemented in Lisp.

This represents a significant architectural improvement that makes Habu more maintainable, more self-hosting, and more aligned with Lisp best practices.
