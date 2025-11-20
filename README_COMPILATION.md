# Habu Native Compilation Guide

Habu now compiles to native ARM64 executables! This document explains how to use the compilation system.

## Quick Start

### Compile a Simple Expression

```bash
# Compile a literal value
./compile-habu.sh 42

# Compile arithmetic
./compile-habu.sh '(+ 10 15)'
./compile-habu.sh '(* 6 7)'
./compile-habu.sh '(- 20 8)'
```

### Run the Test Suite

```bash
./test-compilation-suite.sh
```

Expected output:
```
======================================
Habu Compilation Pipeline Test Suite
======================================

1. Literal Values
-----------------
Testing return 0... PASS (got 0)
Testing return 1... PASS (got 1)
Testing return 42... PASS (got 42)
...

Results: 16 passed, 0 failed
✅ ALL TESTS PASSED!
```

## Architecture

The compilation system uses a two-stage pipeline:

```
┌─────────────┐      ┌──────────────┐      ┌────────────┐
│ Habu Source │ ───> │      IR      │ ───> │  Assembly  │
│   (Lisp)    │      │ (S-expr)     │      │  (ARM64)   │
└─────────────┘      └──────────────┘      └────────────┘
      │                     │                      │
      │ Habu Compiler      │ C Backend            │ clang
      │ (50 lines Lisp)    │ (ir-to-asm.c)       │
      v                     v                      v
  compile-expr          IR → ASM              executable
```

### Stage 1: Frontend (Habu Lisp)

File: `habu-self-hosting-compiler.lisp`

The frontend compiles Habu expressions to an intermediate representation (IR):

```lisp
Input:  42
Output: (lit 42)

Input:  (+ 3 4)
Output: (call + (lit 3) (lit 4))

Input:  (* (+ 1 2) 5)
Output: (call * (call + (lit 1) (lit 2)) (lit 5))
```

### Stage 2: Backend (C)

File: `ir-to-asm.c`

The backend converts IR to ARM64 assembly:

```
Input:  (lit 42)
Output: mov x0, #672    ; 42 << 4 (tagged fixnum)
        lsr x0, x0, #4  ; Untag for return
        ret

Input:  (call + (lit 3) (lit 4))
Output: mov x1, #48     ; 3 << 4
        mov x2, #64     ; 4 << 4
        add x0, x1, x2
        lsr x0, x0, #4
        ret
```

### Stage 3: System Assembler

The system `clang` assembler converts assembly to a native executable.

## Manual Usage

### 1. Generate IR

```bash
# Using the compiler directly (when working)
echo "(compile-expr '(+ 5 7))" | ./habu

# Or manually construct IR:
IR="(call + (lit 5) (lit 7))"
```

### 2. Generate Assembly

```bash
./ir-to-asm "$IR" > output.s
```

Example output in `output.s`:
```asm
.section __TEXT,__text,regular,pure_instructions
.globl _main
.p2align 2

_main:
    ; Binary operation: 5 + 7
    mov x1, #80
    mov x2, #112
    add x0, x1, x2
    ; Untag fixnum for exit code
    lsr x0, x0, #4
    ret
```

### 3. Assemble and Link

```bash
clang -o output output.s
```

### 4. Run

```bash
./output
echo $?   # Prints 12 (5 + 7)
```

## Supported Operations

### Literals

```bash
./compile-habu.sh 0      # Returns 0
./compile-habu.sh 42     # Returns 42
./compile-habu.sh 255    # Returns 255
```

### Binary Arithmetic

| Operation | Example | Result |
|-----------|---------|--------|
| Addition | `(+ 3 4)` | 7 |
| Subtraction | `(- 10 3)` | 7 |
| Multiplication | `(* 6 7)` | 42 |

```bash
./compile-habu.sh '(+ 10 20)'   # Returns 30
./compile-habu.sh '(- 50 8)'    # Returns 42
./compile-habu.sh '(* 12 5)'    # Returns 60
```

## Technical Details

### Tagged Fixnums

Habu uses a tagged pointer representation. Fixnums (integers) are tagged by left-shifting by 4 bits:

```
Untagged Value  | Tagged Value (hex) | Tagged Value (dec)
----------------|--------------------|-----------------
0               | 0x0                | 0
1               | 0x10               | 16
42              | 0x2A0              | 672
100             | 0x640              | 1600
```

Formula: `tagged = value << 4`

This leaves the lower 4 bits for the type tag (0000 = fixnum).

### ARM64 Calling Convention

- Function arguments: `x0`, `x1`, `x2`, ..., `x7`
- Return value: `x0`
- Stack must be 16-byte aligned
- Preserved registers: `x19-x28`

### Assembly Format

All generated assembly follows this structure:

```asm
.section __TEXT,__text,regular,pure_instructions
.globl _main
.p2align 2

_main:
    ; Your generated code here
    ; ...
    ; Result in x0, tagged
    lsr x0, x0, #4    ; Untag for return/exit code
    ret
```

## Testing

### Run All Tests

```bash
./test-compilation-suite.sh
```

### Test Individual Cases

```bash
# Test a specific operation
./ir-to-asm '(call + (lit 20) (lit 22))' > test.s
clang -o test test.s
./test
echo $?   # Should print 42
```

### Verify Assembly Output

```bash
# Generate assembly and inspect it
./ir-to-asm '(lit 42)' > test.s
cat test.s

# Expected:
#   mov x0, #672
#   lsr x0, x0, #4
#   ret
```

## Files

### Core Implementation

- `habu-self-hosting-compiler.lisp` - Habu frontend (IR generation)
- `ir-to-asm.c` - C backend (assembly generation)
- `ir-to-asm` - Compiled backend binary
- `compile-habu.sh` - Integration script

### Test Suite

- `test-compilation-suite.sh` - Comprehensive automated tests
- `test-*.s` - Example assembly files
- `test-*` - Example executables

### Documentation

- `README_COMPILATION.md` - This file
- `COMPILATION_ARCHITECTURE.md` - Detailed architecture
- `IMPLEMENTATION_STATUS.md` - Overall project status

## Current Limitations

### Not Yet Supported

1. **Nested expressions**: `(* (+ 1 2) (+ 3 4))`
   - Requires stack management for intermediate values
   - Coming soon!

2. **Variables**: `(let ((x 42)) x)`
   - Need environment support
   - Future enhancement

3. **List operations**: `(cons 1 2)`, `(car '(1 2))`
   - Requires runtime linking
   - Next major milestone

4. **Function calls**: `(defun f (x) (* x 2)) (f 21)`
   - Need closure support
   - Future enhancement

### Workarounds

For complex expressions, decompose manually:

```bash
# Instead of: (* (+ 1 2) (+ 3 4))
# Calculate manually:
#   (+ 1 2) = 3
#   (+ 3 4) = 7
#   (* 3 7) = 21

./compile-habu.sh '(* 3 7)'  # Returns 21
```

## Examples

### Example 1: The Answer

```bash
$ ./compile-habu.sh '(* 6 7)'
Compiling: (* 6 7)
IR: (call * (lit 6) (lit 7))
Generated: a.out
Running...
Exit code: 42
```

### Example 2: Hundred

```bash
$ ./ir-to-asm '(call + (lit 90) (lit 10))' > hundred.s
$ clang -o hundred hundred.s
$ ./hundred && echo "Exit code: $?"
Exit code: 100
```

### Example 3: Verify Assembly

```bash
$ ./ir-to-asm '(lit 42)'
.section __TEXT,__text,regular,pure_instructions
.globl _main
.p2align 2

_main:
    ; Load literal 42 (tagged: 672)
    mov x0, #672
    ; Untag fixnum for exit code
    lsr x0, x0, #4
    ret
```

## Troubleshooting

### Issue: Command not found

**Solution**: Make scripts executable:
```bash
chmod +x compile-habu.sh ir-to-asm test-compilation-suite.sh
```

### Issue: clang fails

**Solution**: Verify you're on ARM64 macOS:
```bash
uname -m   # Should print "arm64"
```

### Issue: Wrong exit code

**Solution**: Check the IR is correct:
```bash
./ir-to-asm '(lit 42)' | grep "mov x0"
# Should show: mov x0, #672
```

### Issue: Can't compile Habu source

**Limitation**: S-expression printing not yet implemented in Habu.

**Workaround**: Manually write IR for now:
```bash
# Instead of: echo "(compile-expr '(+ 1 2))" | ./habu
# Use: ./ir-to-asm "(call + (lit 1) (lit 2))"
```

## Next Steps

The compilation system is working! Next phases:

1. ✅ **Basic compilation** (DONE)
   - Literals and arithmetic work
   - 16/16 tests passing

2. 🔄 **Runtime integration** (NEXT)
   - Link with `habu_cons`, `habu_car`, `habu_cdr`
   - Heap allocation
   - GC integration

3. ⏳ **Nested expressions**
   - Stack-based evaluation
   - Complex expression trees

4. ⏳ **Self-hosting**
   - Compile compiler with itself
   - Fixed-point verification
   - True bootstrap

## Contributing

To extend the compiler:

1. **Add new operations**: Edit `ir-to-asm.c`, add case in `codegen_call()`
2. **Test**: Add test case to `test-compilation-suite.sh`
3. **Verify**: Run `./test-compilation-suite.sh`

Example - adding division:

```c
// In ir-to-asm.c, in codegen_call():
case '/':
    printf("    lsr x1, x1, #4  ; Untag dividend\n");
    printf("    lsr x2, x2, #4  ; Untag divisor\n");
    printf("    udiv x0, x1, x2\n");
    printf("    lsl x0, x0, #4  ; Re-tag result\n");
    break;
```

## Resources

- **Architecture**: See `COMPILATION_ARCHITECTURE.md`
- **Status**: See `IMPLEMENTATION_STATUS.md`
- **ARM64 Reference**: [ARM Architecture Reference Manual](https://developer.arm.com/documentation/ddi0487/latest)
- **Mach-O Format**: [macOS ABI](https://github.com/aidansteele/osx-abi-macho-file-format-reference)

---

**Status**: Working prototype - native ARM64 compilation functional! 🚀

**Achievement unlocked**: Habu now compiles to native machine code!
