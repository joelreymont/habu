# Habu Bootstrap Compiler

## Overview

The Habu bootstrap compiler is written in Common Lisp (SBCL) and generates native machine code for both x86_64 and ARM64 architectures. It compiles Habu Lisp expressions directly to executable machine code without requiring an intermediate C representation.

## Architecture

```
Habu Source (.lisp)
        ↓
   S-expression Reader
        ↓
    Parser (IR)
        ↓
   Code Generator
    /          \
x86_64        ARM64
Machine Code  Machine Code
    \          /
     ELF Writer
        ↓
  Executable Binary
```

## Components

### 1. S-expression Reader (`bootstrap/reader.lisp`)

Reads Habu Lisp source code. Currently wraps the Common Lisp reader.

```lisp
(read-habu-expr stream)      ; Read single expression
(read-habu-file filename)    ; Read entire file
(read-habu-string string)    ; Read from string
```

### 2. Parser and IR (`bootstrap/compiler.lisp`)

Converts S-expressions to an intermediate representation (IR).

**IR Structure:**
```lisp
(defstruct expr
  type   ; Symbol: fixnum, variable, call
  value  ; The actual value or operator
  args)  ; List of argument expressions
```

**Supported Forms:**
- Fixnums: `42`, `-10`, `0`
- Arithmetic: `(+ a b)`, `(- a b)`
- Nested: `(+ (+ 1 2) 3)`

### 3. Code Generators

#### x86_64 Code Generation

**Fixnum Loading:**
```asm
mov rax, imm64    ; 10 bytes
```

**Addition (+ a b):**
```asm
; Evaluate a → RAX
push rax          ; Save a
; Evaluate b → RAX
mov rbx, [rsp]    ; Load a from stack
add rax, rbx      ; Add
add rsp, 8        ; Pop stack
```

**Code size:** ~32 bytes for binary operation

#### ARM64 Code Generation

**Fixnum Loading:**
```asm
movz x0, #imm16         ; 4 bytes (small values)
movz x0, #imm16         ; 8 bytes (large values)
movk x0, #imm16, lsl#16
```

**Addition (+ a b):**
```asm
; Evaluate a → X0
stp x29, x30, [sp, #-16]!  ; Save frame
; Save and evaluate b
add x0, x0, x1              ; Add
ldp x29, x30, [sp], #16     ; Restore frame
```

**Code size:** ~32 bytes for binary operation

### 4. Binary Writer (`bootstrap/elf-writer.lisp`)

Generates minimal ELF executables for Linux.

**Features:**
- ELF64 header generation
- Program header (PT_LOAD)
- Executable code section
- Proper alignment
- Return instruction appending

## Value Representation

All values use the Habu tagged pointer format:

**Fixnums:**
- Represented as `value * 16` (shift left 4 bits)
- Lower 4 bits = `0000` (fixnum tag)
- Range: 60-bit signed integers

**Example:**
```
Lisp value: 42
Tagged:     0x2A0 (672 in decimal)
Binary:     0000 0010 1010 0000
            ^^^^ fixnum tag
```

## Usage

### From Common Lisp

```lisp
(load "bootstrap/compiler.lisp")
(in-package :habu-compiler)

; Compile expression to machine code bytes
(compile-expression '(+ 10 20) :arch :x86_64)
; => #(72 184 160 0 0 0 0 0 0 0 80 ...)

; Compile to executable binary
(compile-to-binary '(+ 15 27) "output.bin" :arch :x86_64)
; => "output.bin", 33  (filename, code size)
```

### From Command Line

```bash
sbcl --script examples/compile-simple.lisp
```

## Code Generation Examples

### Example 1: Simple Fixnum

**Input:** `42`

**x86_64 output:**
```
48 B8 A0 02 00 00 00 00 00 00  ; mov rax, 0x2A0
C3                              ; ret
```

**ARM64 output:**
```
00 54 80 D2  ; movz x0, #0x2A0
C0 03 5F D6  ; ret
```

### Example 2: Addition

**Input:** `(+ 10 20)`

**Result:** Tagged value `0x1E0` (480 decimal = 30 * 16)

**x86_64:** 32 bytes of code
**ARM64:** 32 bytes of code

### Example 3: Nested Expression

**Input:** `(+ (+ 1 2) (+ 3 4))`

**Result:** Tagged value `0xA0` (160 decimal = 10 * 16)

**Code size:** 76 bytes

## Testing

### Compiler Tests

Run all compiler tests:
```bash
cd bootstrap
sbcl --script test-compiler.lisp
```

**Test coverage:**
- Expression parsing (fixnum, calls)
- x86_64 code generation
- ARM64 code generation
- Binary file generation
- Code size verification

### C Integration Tests

```bash
make
./tests/test_compiler_simple
```

Tests that compiled binaries are generated correctly.

## Limitations (Current)

1. **Operators:**
   - Only `+` and `-` implemented
   - No multiplication, division, or comparison

2. **Types:**
   - Only fixnums
   - No strings, symbols, or compound data

3. **Control flow:**
   - No `if`, `loop`, or function calls

4. **I/O:**
   - No print, read, or file operations

5. **Optimizations:**
   - No constant folding
   - No dead code elimination
   - No register allocation optimization

## Performance

**Compilation speed:**
- Simple expressions: <1ms
- Nested expressions: <10ms

**Generated code:**
- Fixnum: 10-11 bytes (includes ret)
- Binary op: 32-39 bytes
- Competitive with hand-written assembly

## Future Enhancements

### Short-term
1. More arithmetic operators (`*`, `/`, `mod`)
2. Comparison operators (`<`, `>`, `=`)
3. Conditional expressions (`if`)
4. Function definitions and calls

### Medium-term
1. Cons cells and lists
2. Vectors and strings
3. Symbol lookup
4. Lambda expressions

### Long-term
1. Full Common Lisp compatibility
2. Macro system
3. CLOS (object system)
4. Optimizing compiler
5. Self-hosting (compile Habu in Habu)

## Architecture Notes

**Why Common Lisp for bootstrap?**
- Mature S-expression support
- Interactive development (REPL)
- Excellent debugging tools
- Self-hosting path clear

**Why direct machine code?**
- No C dependency in compilation path
- Full control over code generation
- Easier to optimize
- Clearer correspondence to Lisp semantics

**Cross-compilation strategy:**
- Same IR for all targets
- Architecture-specific code generators
- Test on x86_64, deploy to ARM64
- Eventually self-host on ARM64

## References

**Code generation:**
- Intel 64 and IA-32 Architectures Software Developer's Manual
- ARM Architecture Reference Manual ARMv8
- System V AMD64 ABI
- ELF-64 Object File Format

**Lisp compilation:**
- "Lisp in Small Pieces" by Christian Queinnec
- SBCL compiler internals
- Chez Scheme compiler

## Files

```
bootstrap/
├── habu.asd              # ASDF system definition
├── compiler.lisp         # Main compiler and IR
├── reader.lisp           # S-expression reader
├── elf-writer.lisp       # Binary file writer
└── test-compiler.lisp    # Compiler tests

examples/
└── compile-simple.lisp   # Usage examples

tests/
└── test_compiler_simple.c  # C integration tests
```

## Example Session

```lisp
$ sbcl
* (load "bootstrap/compiler.lisp")
* (in-package :habu-compiler)

* (parse '(+ 10 20))
#S(EXPR :TYPE CALL :VALUE + :ARGS (...))

* (compile-expression '(+ 10 20) :arch :x86_64)
#(72 184 160 0 0 0 0 0 0 0 80 72 184 ...)

* (compile-to-binary '(+ 15 27) "test.bin" :arch :x86_64)
"test.bin"
33

* (quit)
```

## Summary

The Habu bootstrap compiler successfully generates native machine code for both x86_64 and ARM64 from Lisp expressions. While currently limited to arithmetic on fixnums, it demonstrates the viability of direct Lisp-to-machine-code compilation and provides a foundation for building a complete self-hosting Lisp compiler.

**Status:** Functional for basic arithmetic
**Next step:** Compile and execute "Hello, World!"
