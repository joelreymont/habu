# Habu Minimal Runtime - Clean Architecture

## Overview

Habu now has a minimal C runtime with NO REPL-specific code. All REPL functionality is implemented in Habu Lisp itself, demonstrating the language's capability for self-hosting.

## Architecture

```
┌─────────────────────────────────────────┐
│          Habu Lisp Code                 │
│  (REPL written in pure Lisp)            │
└─────────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────────┐
│       C Backend Compiler                │
│  (Translates Lisp → C code)             │
└─────────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────────┐
│       Generated C Code                  │
└─────────────────────────────────────────┘
                  ↓
┌─────────────────────────────────────────┐
│    Minimal C Runtime (54KB)             │
│  - GC, cons, strings, vectors           │
│  - Type predicates, string ops          │
│  - Reader, evaluator primitives         │
│  - NO REPL-specific code                │
└─────────────────────────────────────────┘
```

## C Runtime Primitives

### Core Operations (runtime/runtime.c)
- `habu_cons`, `habu_car`, `habu_cdr` - List operations
- `habu_make_vector`, `habu_vector_ref`, `habu_vector_set` - Vectors
- `habu_make_closure`, `habu_closure_code`, `habu_closure_env` - Closures

### Type Predicates (NEW)
- `habu_fixnum_p(val)` - Check if fixnum
- `habu_cons_p(val)` - Check if cons cell
- `habu_string_p(val)` - Check if string
- `habu_nil_p(val)` - Check if nil
- `habu_symbol_p(val)` - Check if symbol
- `habu_vector_p(val)` - Check if vector

### String Operations (NEW)
- `habu_string_ref(str, index)` - Get character at index
- `habu_string_length_raw(str)` - Get string length
- `habu_string_to_cstr(str)` - Convert to C string
- `habu_string_eq(s1, s2)` - Compare strings

### Symbol Operations (NEW)
- `habu_make_symbol_from_string(str)` - Create symbol
- `habu_symbol_name(sym)` - Get symbol name
- `habu_symbol_eq(s1, s2)` - Compare symbols

### I/O Primitives
- `habu_fgets_line()` - Read line from stdin (minimal, no fancy editing)
- `habu_print_value(val)` - Print any value
- `habu_println_value(val)` - Print with newline
- `habu_read_file(path)` - Read entire file
- `habu_write_file(path, content)` - Write file

### Language Primitives (General Purpose)
- `habu_read_from_string(str)` - Parse S-expression (runtime/reader.c)
- `habu_eval(expr)` - Evaluate expression (runtime/eval.c)

## What Was Removed

### ❌ REPL-Specific Code (DELETED)
- **runtime/lineedit.c** - Fancy line editing with arrow keys, history
  - This was REPL-specific UI code
  - Replaced with simple `habu_fgets_line()`
  - Line editing can be added in Lisp if desired

### ✅ General-Purpose Code (KEPT)
- **runtime/reader.c** - S-expression parser
  - NOT REPL-specific
  - General language feature for parsing
  - Used by any Lisp program that needs to read code

- **runtime/eval.c** - Expression evaluator
  - NOT REPL-specific
  - General language feature for dynamic evaluation
  - Used by any Lisp program that needs `eval`

- **runtime/gc.c** - Garbage collector
  - Core memory management
  - Required by all Habu programs

- **runtime/io.c** - I/O operations
  - File operations, printing
  - General purpose

- **runtime/runtime.c** - Core operations
  - Cons cells, vectors, closures
  - Fundamental data structures

## REPL Implementation

The REPL is now written in pure Habu Lisp:

**File: repl.lisp** (34 lines)
```lisp
(defun repl-loop ()
  (progn
    (print (quote "Habu REPL - Written in Lisp!"))
    (println)
    (print (quote "Press Ctrl-D to exit"))
    (println)
    (println)
    (repl-loop-body)))

(defun repl-loop-body ()
  (progn
    (print (quote "habu> "))
    (let ((line (fgets-line)))
      (if line
          (progn
            (if (> (string-length line) (quote 0))
                (let ((input-str (make-string-from-cstr line)))
                  (let ((expr (read-from-string input-str)))
                    (let ((result (eval expr)))
                      (progn
                        (print-value result)
                        (println)))))
                (quote nil))
            (repl-loop-body))
          (progn
            (println)
            (print (quote "Bye!"))
            (println))))))

(repl-loop)
```

## Compilation

```bash
# Compile Lisp REPL to C
sbcl --load /tmp/compile-repl.lisp

# Compile C to binary
clang -O2 -I runtime habu-repl-minimal.c \
  runtime/gc.o runtime/runtime.o runtime/io.o \
  runtime/reader.o runtime/eval.o \
  -o habu-repl-minimal
```

Result: **54KB standalone executable**

## Usage

```bash
$ ./habu-repl-minimal
"Habu REPL - Written in Lisp!"
"Press Ctrl-D to exit"

"habu> "(+ 1 2 3)
6
"habu> "(list 1 2 3)
(1 2 3)
"habu> "(car (list 10 20))
10
"habu> "^D

"Bye!"
```

## Benefits

1. **Minimal C Runtime**
   - Only general-purpose primitives
   - No application-specific code
   - Clean separation of concerns

2. **Self-Hosting**
   - REPL written in Habu Lisp
   - Demonstrates language capabilities
   - Easy to extend in Lisp

3. **Maintainability**
   - REPL logic in high-level language
   - Easy to modify and enhance
   - C runtime is stable, rarely changes

4. **Educational**
   - Shows how to build REPL in Lisp
   - Clear example of language features
   - Understanding through implementation

## Future Enhancements

All can be done in Lisp without touching C:

- **Line editing** - Implement in Lisp using escape sequences
- **History** - Maintain list of previous commands
- **Tab completion** - Symbol lookup in environment
- **Syntax highlighting** - Color output
- **Multi-line input** - Handle incomplete expressions
- **Help system** - Documentation lookup
- **Debugging** - Breakpoints, stepping

## Summary

Habu now has a clean, minimal architecture:
- ✅ C runtime: General-purpose primitives only
- ✅ REPL: Written entirely in Habu Lisp
- ✅ No REPL-specific C code
- ✅ 54KB standalone executable
- ✅ Self-hosting demonstration

The line between "runtime" and "application" is now clear and well-defined.
