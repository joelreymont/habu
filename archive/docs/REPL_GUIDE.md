# Habu REPL Guide

## Overview

Habu Lisp now has **two REPL implementations**:

1. **habu-repl** - Written in C (55KB) - Reference implementation
2. **habu-repl-lisp** - **Written in Habu Lisp itself** (54KB) - **Self-hosting!**

Both REPLs are functionally identical and provide the same features.

## Features

Both REPLs are **~54KB standalone executables** with full line editing support:

✨ **Line Editing:**
- **Arrow keys**: Navigate left/right through the line
- **Ctrl-A**: Jump to beginning of line
- **Ctrl-E**: Jump to end of line
- **Backspace/Delete**: Edit text
- **Ctrl-C / Ctrl-D**: Exit

📚 **History:**
- **Up/Down arrows**: Browse command history
- Automatically saves non-empty commands
- Avoids duplicate consecutive entries

## Running the REPL

**C version:**
```bash
./habu-repl
```

**Lisp version (self-hosting):**
```bash
./habu-repl-lisp
```

Both provide identical functionality and user experience.

## Supported Features

### Arithmetic
```lisp
=> (+ 1 2 3)
6

=> (* 5 6)
30

=> (/ 100 5)
20

=> (- 42 17)
25
```

### Nested Expressions
```lisp
=> (+ (* 3 4) (* 5 6))
42
```

### Comparisons
```lisp
=> (< 5 10)
1        ; true

=> (> 5 10)
nil      ; false

=> (= 42 42)
1        ; true
```

### Lists
```lisp
=> (cons 1 2)
(1 . 2)  ; dotted pair

=> (list 1 2 3)
(1 2 3)

=> (car (list 10 20 30))
10

=> (cdr (list 10 20 30))
(20 30)
```

### Conditionals
```lisp
=> (if (< 5 10) 100 200)
100

=> (if (> 5 10) 100 200)
200
```

### Quote
```lisp
=> (quote (hello world))
("hello" "world")

=> '(1 2 3)
(1 2 3)
```

## Examples

### Factorial Calculation
```lisp
=> (* 5 (* 4 (* 3 (* 2 1))))
120
```

### Conditional Selection
```lisp
=> (if (= (+ 1 1) 2) (quote correct) (quote wrong))
"correct"
```

### List Processing
```lisp
=> (car (cdr (list 1 2 3 4)))
2
```

## Implementation Details

**C Version (habu-repl):**
- **Size**: 55 KB standalone executable
- **Source**: 44 lines of C code
- **Memory**: 4 MB heap with generational GC
- **Runtime**: Full garbage collection, no memory leaks
- **Dependencies**: None (fully self-contained)

**Lisp Version (habu-repl-lisp) - Self-hosting:**
- **Size**: 54 KB standalone executable
- **Source**: 31 lines of Habu Lisp (in `repl.lisp`)
- **Compilation**: Lisp → C (60 lines) → Binary
- **Memory**: 4 MB heap with generational GC
- **Runtime**: Full garbage collection, no memory leaks
- **Dependencies**: None (fully self-contained)

The Lisp version demonstrates true self-hosting - the REPL logic is written in Habu Lisp itself and compiled to C using the C backend!

## What's NOT Supported (Yet)

- Variables/binding (let, defun)
- Lambdas
- Looping constructs
- File I/O from REPL
- String operations beyond basics

These features work in compiled programs but need environment support for the REPL.

## Building From Source

**C Version:**
```bash
clang -O2 -I runtime habu-repl.c runtime/*.o -o habu-repl
```

**Lisp Version (self-hosting):**
```bash
# Step 1: Compile Lisp source to C
sbcl --noinform --no-userinit --no-sysinit --non-interactive \
  --load bootstrap/compiler.lisp \
  --load bootstrap/reader.lisp \
  --load bootstrap/c-backend.lisp \
  --eval '(in-package :habu-compiler)' \
  --eval "(generate-c-standalone (cons 'progn (read-habu-file \"repl.lisp\")) :output-file \"habu-repl-lisp.c\")"

# Step 2: Compile C to binary
clang -O2 -I runtime habu-repl-lisp.c runtime/*.o -o habu-repl-lisp
```

The Lisp version demonstrates the full compilation pipeline: Habu Lisp → C code → Executable!

---

**Habu Lisp** - A minimal, self-hosting Lisp implementation
