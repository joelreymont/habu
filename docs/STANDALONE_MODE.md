# Standalone Mode - Habu Lisp without SBCL

## Overview

Habu now supports **standalone mode** - compiling Lisp programs to native executables without requiring SBCL at runtime. This is achieved through a C backend that generates C code, which is then compiled and linked with the Habu runtime library.

## Architecture

### Components

1. **C Backend** (`bootstrap/c-backend.lisp`)
   - Translates Habu Lisp expressions to C code
   - Generates standalone C programs with proper initialization

2. **Runtime Library** (`runtime/*.c`)
   - `gc.c` - Generational garbage collector
   - `runtime.c` - Core runtime functions (cons, car, cdr, etc.)
   - `region.c` - Region-based allocation
   - `io.c` - File I/O and printing

3. **Object Representation** (`runtime/object.h`)
   - 64-bit tagged pointers
   - Inline type checking
   - 16-byte aligned heap objects

## Compilation Pipeline

```
Habu Lisp Source
       ↓
C Backend (compiler → C code)
       ↓
C Compiler (clang/gcc)
       ↓
Linker (links with runtime.o)
       ↓
Standalone Executable
```

## Supported Features

### Data Types
- [x] Fixnums (60-bit signed integers)
- [x] Cons cells (pairs)
- [x] Lists
- [x] Strings
- [ ] Vectors (runtime support exists, C backend TODO)
- [ ] Symbols (runtime support exists, C backend TODO)
- [ ] Closures (runtime support exists, C backend TODO)

### Operations

**Arithmetic**
- `+`, `-`, `*`, `/`

**Comparison**
- `=`, `<`, `>`, `<=`, `>=`

**List Operations**
- `cons`, `car`, `cdr`, `list`

**Conditionals**
- `if` - ternary conditional
- [ ] `cond`, `case` - TODO

**Control Flow**
- `progn`/`begin` - sequential evaluation
- [ ] `while`, `dolist`, `dotimes` - TODO

**Variable Binding**
- `let` - lexical bindings
- [ ] `let*` - sequential bindings (TODO)
- [ ] `defun` - top-level functions (TODO)

**I/O Operations**
- `print` - Print value to stdout
- `read-file` - Read entire file as string
- `write-file` - Write string to file
- [ ] `open-file`, `close-file`, `read-line`, `write-string` - Available in runtime, C backend TODO

## Example Programs

### Hello World
```lisp
(print "Hello, World!")
```

Compiles to:
```c
#include "habu.h"
#include "object.h"
#include <stdio.h>

int main(void) {
    habu_init(4 * 1024 * 1024);
    habu_value_t result = habu_println_value(habu_make_string("Hello, World!", 13));
    habu_shutdown();
    return 0;
}
```

### Fibonacci (using let and recursion)
```lisp
(let ((a 0) (b 1))
  (let ((next (+ a b)))
    (print next)))
```

### File Operations
```lisp
(progn
  (write-file "/tmp/test.txt" "Hello from Habu!")
  (print (read-file "/tmp/test.txt")))
```

### List Processing
```lisp
(let ((numbers (list 1 2 3 4 5)))
  (car (cdr numbers)))  ; Returns 2
```

## Usage

### From SBCL REPL

```lisp
(load "bootstrap/compiler.lisp")
(load "bootstrap/c-backend.lisp")
(in-package :habu-compiler)

;; Generate and compile a program
(generate-c-standalone '(+ (quote 40) (quote 2))
                      :output-file "/tmp/answer.c")
(compile-and-run-c "/tmp/answer.c")
```

### Running Tests

```bash
sbcl --load bootstrap/test-c-backend.lisp     # Basic operations
sbcl --load bootstrap/test-c-extended.lisp    # Control flow
sbcl --load bootstrap/test-io.lisp            # I/O operations
sbcl --load bootstrap/test-let.lisp           # Let bindings
```

## Implementation Details

### Memory Management

The standalone runtime uses a generational garbage collector:

- **Young Generation**: 512 KB copying collector (Cheney's algorithm)
- **Old Generation**: 4 MB mark-sweep collector
- **Promotion**: Objects surviving 5+ collections move to old gen
- **Write Barriers**: Track old→young pointers

### Object Layout

All heap objects have a 16-byte header:
```c
typedef struct {
    uint64_t type : 8;      // Object type
    uint64_t size : 40;     // Size in bytes
    uint64_t gc_color : 2;  // GC marking color
    uint64_t gen_age : 6;   // Generation age
    uint64_t reserved : 8;  // Reserved
    uint64_t padding;       // Alignment padding
} habu_header_t;
```

Tagged pointer scheme (lower 4 bits):
- `0x0` - Fixnum (immediate integer)
- `0x1` - Cons cell
- `0x2` - Symbol
- `0x3` - Vector
- `0x4` - String
- `0x5` - Closure

### C Backend Code Generation

The C backend uses GCC/Clang statement expressions for complex forms:

**Let bindings:**
```c
({
    habu_value_t x = fixnum_to_value(10);
    habu_value_t y = fixnum_to_value(20);
    fixnum_to_value(value_to_fixnum(x) + value_to_fixnum(y));
})
```

**If expressions:**
```c
(is_nil(condition) ? else_expr : then_expr)
```

## Performance

The C backend generates reasonably efficient code:

- Direct function calls to runtime (no FFI overhead)
- Inline type checking
- Compiler optimizations (with `-O2`)

However, it's not as optimized as hand-written C or native machine code generation would be.

## Limitations

### Current Limitations

1. **No Lambda/Closures**: C backend doesn't support lambdas yet (runtime does)
2. **No Macros**: Macro expansion happens in SBCL, not standalone
3. **Limited Standard Library**: Only basic operations implemented
4. **GCC-specific**: Uses statement expressions (non-standard C)
5. **No REPL**: Standalone binaries are batch programs

### Architectural Limitations

- **Compilation Speed**: Slower than interpreters due to C compilation step
- **Binary Size**: Each binary includes full runtime (~50KB overhead)
- **Debugging**: C-level debugging, not Lisp-level

## Future Work

### Short Term
- [ ] Add `defun` support for top-level functions
- [ ] Implement `lambda` and closures in C backend
- [ ] Add more list operations (map, filter, reduce)
- [ ] Implement proper error handling

### Medium Term
- [ ] Direct machine code generation (bypass C backend)
- [ ] Proper ELF/Mach-O linker
- [ ] Inline bump-pointer allocation (avoid C function calls)
- [ ] REPL support in standalone mode

### Long Term
- [ ] Port compiler to Habu (self-hosting)
- [ ] Optimize GC for standalone workloads
- [ ] Add threading support
- [ ] Build standard library

## Comparison with Bootstrap Mode

| Feature | Bootstrap (SBCL) | Standalone (C) |
|---------|------------------|----------------|
| Runtime Dependency | SBCL | None |
| Compilation Speed | Fast | Slow |
| Execution Speed | Fast | Medium |
| Binary Size | N/A | ~50KB + code |
| Debugging | Excellent | Limited |
| Distribution | Complex | Simple (single binary) |
| Development | Interactive | Batch |

## Success Metrics

✅ **Standalone Operation**: Binaries run without SBCL
✅ **GC Integration**: Full generational GC works
✅ **I/O System**: Read/write files, print values
✅ **Control Flow**: Let, if, progn work correctly
✅ **Memory Safety**: No leaks, proper cleanup

## Conclusion

The C backend provides a practical path to standalone Habu programs. While not as sophisticated as direct machine code generation, it leverages existing C compilers and provides a solid foundation for bootstrapping a self-hosting Lisp compiler.

The next major milestone is **self-hosting**: using the standalone Habu compiler to compile itself, eliminating the SBCL dependency entirely.
