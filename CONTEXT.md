# Habu Self-Hosting Lisp Compiler - Context

**Last Updated**: December 1, 2025
**Milestone**: Stage 1 compiles and runs; working on Stage 1 → Stage 2

## Current Status

The Habu compiler compiles programs with:
- Expressions: literals, arithmetic, comparisons
- Control flow: if, cond, when, unless, progn, and, or, not, setq
- Bindings: let, let*
- Functions: defun, labels, lambda, funcall, (function name)
- Closures: captured variables, higher-order functions
- Data: cons, car, cdr, list, quote, setcar, setcdr
- Predicates: null, consp, numberp, symbolp, stringp, vectorp
- Strings: string-length, string-ref, string-concat
- Symbols: symbol-name, make-symbol-from-string
- Vectors: make-vector, vector-ref, vector-set, vector-length
- File I/O: sys-open, sys-read, sys-write, sys-close, native-read-file
- System: sys-exit, get-intern-table, set-intern-table

**Test Results**: 76/76 pass (comprehensive, self-hosting, edge cases)

## Current Session (December 1, 2025)

### Stage 1 Status

Full Stage 1 compiler (reader + compiler + codegen + macho-utils) works:
- Source size: ~170KB
- Native binary: 8.7MB
- Compiles and runs: PASS (exit 42)
- read-all, compile-forms, compile-program: all work natively

### In Progress: Stage 1 Generate Stage 2

Testing if Stage 1 can generate a binary (call `deliver` natively).

**Blockers found:**
1. `native-write-file` is NOT a built-in IR - must be compiled from source
2. The `#-sbcl` guarded version in macho-utils.lisp uses `#x1ED` (755 octal)
3. When Stage 1 reads macho-utils.lisp, feature `#-sbcl` correctly includes native version
4. But the native-write-file call wasn't producing output - investigating

### Key Bug Fixes This Session

1. **Symbol registration for native code**
   - Refactored `ensure-symbols-registered` into 18 small helpers (was deeply nested)
   - Added comma/unquote handling in reader
   - Added empty symbol protection in `read-sym`
   - Added `#-sbcl` guards for native `string=`

2. **STP offset encoding overflow** (from Nov 30)
   - STP 7-bit signed offset can't encode 0x3F0
   - Changed to STR/LDR which use 12-bit unsigned offset

3. **Heap alignment in make-vector** (from Nov 30)
   - Wrong AND mask `0xE3` → correct `0xF0`
   - Fixed encoding: `(and-imm 1 1 1 59 60)`

## File Structure

```
bootstrap/
  compiler-sbcl.lisp  - SBCL bootstrap compiler (5400+ lines)
  compiler.lisp       - Habu compiler (no SBCL dependencies)
  codegen.lisp        - ARM64 code generator
  macho.lisp          - Mach-O linker (#+sbcl versions)
  macho-utils.lisp    - Mach-O utilities (#-sbcl native versions)
  reader.lisp         - Habu reader

arm64/
  asm.lisp            - ARM64 instruction encoders
```

## Key Functions

```lisp
;; Compile and deliver program
(habu:deliver-v3 "(defun f (x) (* x 2)) (sys-exit (f 21))" "/tmp/out")

;; Individual steps
(read-all source-string)           ; Parse to S-expressions
(compile-program forms)            ; Compile to IR + defuns
(codegen ir rtaddrs fnoffs td)     ; Generate ARM64 code
(build-macho bytes imports)        ; Create Mach-O executable
```

## Tagged Value Representation

- Fixnum: `value << 4`, tag 0
- Cons: `pointer | 1`
- Symbol: `pointer | 2`
- Vector: `pointer | 3`
- String: `pointer | 4`
- Closure: `pointer | 5`

## ARM64 Register Usage

- x0-x7: Arguments and return value
- x20: Environment frame base
- x24: Closure environment pointer
- x26: Code base register
- x28: Heap bump pointer

## Stack Frame Layout

```
sp+0x10:  x19, x20 (callee-saved)
sp+0x20:  x21, x22
sp+0x30:  x23, x24
sp+0x40:  temp slots (td*8)
sp+0x180: environment base
sp+0x240: spill area (td*64)
sp+0x3F0: x29 (fp), x30 (lr)
```

## Self-Hosting Status

**Achieved:**
- deliver-v3 compiles programs to native ARM64
- Closures work (4+ levels nested, 6+ captures)
- All 76 tests pass
- Stage 1 compiles and runs correctly

**Next Steps:**
1. Get Stage 1 to output Stage 2 binary
2. Verify fixed-point (Stage N == Stage N+1)

## Common Operations

```bash
# Compile and run a program
sbcl --load bootstrap/compiler-sbcl.lisp --load bootstrap/macho.lisp \
     --load bootstrap/reader.lisp --load bootstrap/compiler.lisp \
     --load bootstrap/codegen.lisp --load bootstrap/macho-utils.lisp \
     --eval '(habu:deliver-v3 "(sys-exit 42)" "/tmp/test")' --quit
/tmp/test && echo $?  # Should output 42
```

## Known Limitations

1. Max 8 arguments per function
2. 64KB file limit for native-read-file
3. No macros (uses reader macros)

## Debugging

- Exit 139 = SIGSEGV (check stack/spill)
- Exit 137 = SIGKILL (codesign issue on macOS)
- Use `--dynamic-space-size 4096` for large compilations
