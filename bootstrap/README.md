# Habu Bootstrap Compiler

A minimal, hand-written bootstrap compiler for the Habu Lisp language, targeting ARM64 (Apple Silicon).

## Overview

This bootstrap compiler implements the complete compilation pipeline from Habu Lisp expressions to executable ARM64 machine code. It demonstrates a working path to self-hosting by hand-writing critical compiler components in C and ARM64 assembly.

## Architecture

### Tier 1: Primitives (`primitives.c`)
Hand-written ARM64 machine code for fundamental operations:
- **List operations:** car, cdr, cons (runtime wrappers)
- **Arithmetic:** +, -, *, / (tagged fixnum operations)
- **Comparisons:** =, <, > (return untagged 0/1)
- **Predicates:** nil?, cons? (type checking)

**Tests:** 18/18 passing

### Tier 2: Encoders (`encoders.c`)
Parametric instruction generators for ARM64:
- **Data movement:** movz, add, sub, mul, lsr, lsl, mov
- **Memory:** ldr, str, stp, ldp
- **Control flow:** b, b.cond, bl, ret, cmp, cset
- **Utilities:** and (immediate), cmp (immediate)

**Tests:** 21/21 passing

### Tier 3: IR Generation (`ir-generation.c`)
Compile Habu Lisp to intermediate representation:
- **IR nodes:** lit, var, binop, if, let, call
- **Environment management:** variable lookup and binding
- **Expression compilation:** recursive descent through Lisp forms

### Tier 4: Code Generation (`code-generation.c`)
Generate ARM64 machine code from IR:
- **Register allocation:** x0 (results), x1 (second operand), x2 (temps)
- **Code buffer management:** emit instructions, track offsets
- **Branch patching:** emit placeholders, calculate offsets, patch
- **Supported constructs:** literals, arithmetic, comparisons, conditionals

### Tier 5: Integration
Full compilation pipeline working end-to-end with comprehensive testing.

## What Works

The bootstrap compiler can compile and execute:

```lisp
; Literals
42                              → 42
100                             → 100

; Arithmetic
(+ 5 7)                         → 12
(* 6 7)                         → 42
(- 20 8)                        → 12

; Nested expressions
(* (+ 3 4) 5)                   → 35

; Comparisons
(= 5 5)                         → 1 (true)
(< 3 10)                        → 1 (true)
(> 10 5)                        → 1 (true)

; Conditionals
(if (= 10 10) 42 99)            → 42
(if (> 5 10) 100 200)           → 200

; Complex nested expressions
(if (< 5 10) (* 10 10) (+ 1 1)) → 100
```

## Building

```bash
make          # Build the bootstrap compiler
make run      # Run with example programs
make test     # Run all integration tests (19/19 passing)
make clean    # Clean build artifacts
```

## Usage

### As a Library

```c
#include "habu-minimal.h"

// Compile and execute an expression
habu_value_t expr = HABU_TAG_FIXNUM(42);
habu_value_t ir = bootstrap_compile(expr);
size_t code_size;
uint8_t *code = bootstrap_codegen(ir, &code_size);
int64_t result = execute_code(code, code_size);
```

### Example Program

See `habu-bootstrap.c` for complete examples of:
- Building expressions manually
- Compiling and executing
- Nested and complex expressions

## Testing

### Unit Tests (38/38 passing)
- `tests/test-inline.c` - Arithmetic primitives (4/4)
- `tests/test-comparisons.c` - Comparison primitives (6/6)
- `tests/test-predicates.c` - Type predicates (8/8)
- `tests/test-encoders.c` - Instruction encoders (20/20)

### Integration Tests (19/19 passing)
- `tests/test-integration-simple.c` - Literal compilation (1/1)
- `tests/test-integration-arithmetic.c` - Arithmetic operations (6/6)
- `tests/test-integration-comparisons.c` - Comparison operations (9/9)
- `tests/test-integration-if.c` - Conditional expressions (3/3)

**Total: 57/57 tests passing** ✓

## Technical Details

### Tagged Values
- **Fixnums:** `value << 4` (tag = 0 in low 4 bits)
- **Cons cells:** `pointer | 1` (tag = 1)
- **Symbols:** `pointer | 2` (tag = 2)
- **Nil:** `0`

### Calling Convention
- Arguments in x0, x1, x2 (ARM64 ABI)
- Return value in x0
- Callee-saved: x29 (FP), x30 (LR)
- Stack frame: stp x29, x30, [sp, #-16]!

### Code Generation Strategy
1. **Binary operations:**
   - Evaluate first operand → x0
   - Save in x2
   - Evaluate second operand → x0
   - Move to x1
   - Move first operand x2 → x0
   - Execute operation

2. **Conditionals:**
   - Evaluate test → x0
   - Compare with 0
   - Branch if equal to else clause
   - Execute then clause
   - Branch to end
   - Execute else clause

3. **Branch patching:**
   - Emit placeholder branch (offset = 0)
   - Generate code
   - Calculate offset (target - current)
   - Patch instruction with memcpy

## Files

```
bootstrap/
├── README.md                   # This file
├── Makefile                    # Build system
├── habu-bootstrap.c            # Main driver program
├── habu-minimal.h              # Minimal runtime interface
├── runtime-minimal.c           # Cons/car/cdr/intern implementation
├── primitives.c                # Tier 1: Hand-written ARM64 primitives
├── encoders.c                  # Tier 2: ARM64 instruction encoders
├── ir-generation.c             # Tier 3: Lisp → IR compiler
├── code-generation.c           # Tier 4: IR → ARM64 codegen
└── tests/
    ├── Makefile                # Test build system
    ├── test-*.c                # Unit and integration tests
    └── test-*                  # Test binaries
```

## Implementation Notes

### What's Implemented
- ✅ Literal compilation
- ✅ Arithmetic operations (+, -, *)
- ✅ Comparison operations (=, <, >)
- ✅ Conditional expressions (if)
- ✅ Nested expressions
- ✅ Full compilation pipeline
- ✅ JIT execution

### TODOs for Full Compiler
- ⏸️ Division operation (udiv encoder exists, needs codegen)
- ⏸️ Let bindings (IR exists, needs stack management)
- ⏸️ Function calls (framework exists, needs implementation)
- ⏸️ Lambda expressions
- ⏸️ Recursion support
- ⏸️ Proper stack discipline
- ⏸️ Garbage collection

## Performance

Code generation is fast and produces compact machine code:
- Literal: 16 bytes (4 instructions)
- Addition: 36 bytes (9 instructions)
- Multiplication: 48 bytes (12 instructions - includes untag/tag)
- Comparison: 40 bytes (10 instructions)
- Conditional: 60 bytes (15 instructions)

## Next Steps

1. **Parser Integration:** Connect to Lisp reader for text input
2. **Let Bindings:** Implement proper stack-based variable binding
3. **Functions:** Add lambda and function call support
4. **Self-Hosting:** Compile simple compiler functions with bootstrap compiler
5. **Fixed Point:** Compile full compiler with itself

## Limitations

- **ARM64 only:** Targets Apple Silicon (M1/M2/M3)
- **Manual AST construction:** No parser yet (expressions built in C)
- **Limited features:** Core constructs only
- **No GC:** Memory is allocated but never freed
- **Simple register allocation:** Uses fixed registers, no spilling

## Credits

Part of the Habu self-hosting compiler project. This bootstrap compiler demonstrates the feasibility of hand-writing a minimal compiler that can eventually compile itself.

## License

See main project LICENSE file.
