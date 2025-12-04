# Habu Self-Hosting Lisp Compiler - Context

**Last Updated**: December 4, 2025
**Test Results**: 44 passed, 0 failed, 4 skipped (with regalloc enabled)
**Stage 1**: Compiles and runs (1,101,728 bytes with &key support)

## Current Work

All core tests passing. MCP server fixed and stable.

**Recent Changes**:
- Fixed MCP server warnings causing disconnections (warning suppression, format bug)
- Cleaned up native-jit.lisp to avoid sys-write-char (crashes in compiled binaries)
- Added string-based output functions for native REPL

**Next Steps**:
1. Build echo REPL binary (defvar handling needed for globals)
2. Build Stage 1 and verify it works
3. Fix sys-write-char codegen bug (deferred)

## Work Items

Work items are tracked in beads. Run `bd ready` to see available work.

## Architecture

### File Structure
```
bootstrap/
  compiler-sbcl.lisp  - SBCL bootstrap compiler
  compiler.lisp       - Habu compiler (no SBCL dependencies)
  optimize.lisp       - Optimization passes (TCO)
  codegen.lisp        - ARM64 code generator
  gc.lisp             - Garbage collector (Cheney's copying GC)
  gen-gc.lisp         - Generational GC runtime
  macho.lisp          - Mach-O linker (#+sbcl versions)
  macho-utils.lisp    - Mach-O utilities (#-sbcl native versions)
  reader.lisp         - Habu reader
  reg-alloc.lisp      - Register allocator (TAC pipeline)
arm64/
  asm.lisp            - ARM64 instruction encoders (canonical API)
```

### Tagged Value Representation
- Fixnum: `value << 4`, tag 0
- Cons: `pointer | 1`
- Symbol: `pointer | 2`
- Vector: `pointer | 3`
- String: `pointer | 4`
- Closure: `pointer | 5`
- Nil: `0x06` (tag 6)

### ARM64 Register Usage
- x0-x7: Arguments and return value
- x20: Environment frame base
- x24: Closure environment pointer
- x26: Code base register
- x27: GC globals base (memory layout below)
- x28: Heap bump pointer

### Memory Layout at x27
Simple GC mode:
- `[x27+0]`: intern_table (tagged pointer)
- `[x27+8]`: lambda_counter (untagged integer)
- `[x27+16]`: from_end (GC trigger address)
- `[x27+24]`: half_heap_size (constant)
- `[x27+32]`: space_flag (0 or half_heap_size)
- `[x27+40]`: gc_state (0=idle)
- `[x27+48]`: symbol_counter
- `[x27+56]`: symbol_table
- `[x27+64]`: argc (command-line argument count)
- `[x27+72]`: argv (command-line argument vector)
- `[x27+80]`: packages (package list for native reader)
- `[x27+88]`: current-package (current package name)
- `[x27+96]`: stack_base (initial SP for stack scanning)
- `[x27+104]`: reserved (for 16-byte alignment)
- `[x27+112]`: heap data starts (MUST be 16-byte aligned for tag masking)

Generational GC mode (extends above):
- `[x27+128]`: nursery-start
- `[x27+136]`: nursery-end (also old-space-start)
- `[x27+144]`: card-table-start
- `[x27+152]`: old-space-half-size
- `[x27+160]`: old-space-flag
- `[x27+168]`: old-space-alloc
- `[x27+176]`: heap data starts

### Key Conventions

**ARM64 Instructions**: Always use `arm64:` intrinsics directly with keyword arguments.
```lisp
(arm64:add rd rn imm :imm t)      ; ADD immediate
(arm64:ldr rt rn :offset off)     ; LDR with offset
(arm64:b.eq 5)                    ; Branch (instruction count, not bytes)
```
DO NOT create wrapper functions. See `arm64/asm.lisp` for full API.

**Branch Offsets**: All branch instructions take instruction counts, not bytes.
When computing from `code-size` (bytes): `(ash byte-offset -2)`

**GC Triggers**: Toggle via `*use-generational-gc*` in codegen.lisp:406.
Write barriers in: setcar-ir (:1412), setcdr-ir (:1434), vector-set-ir (:1266).

## Known Limitations

1. Max 8 arguments per function
2. 64KB file limit for native-read-file
3. No reader conditionals in native mode
4. Inlining disabled (variable capture bug)
5. Stack frame: 2KB per call (codegen.lisp:2337) - limits recursion depth

## Debugging Reference

- Exit 132 = SIGILL (check code alignment, branch targets)
- Exit 137 = SIGKILL (codesign issue on macOS)
- Exit 138 = SIGBUS (often stack slot collision - check spill-end in codegen.lisp:240)
- Exit 139 = SIGSEGV (stack overflow or bad pointer)

Use `lldb` with function symbols (LC_SYMTAB embedded in binaries).
Use `slot-debug.lisp` for stack slot collision diagnosis.
