# Habu Self-Hosting Lisp Compiler - Context

**Last Updated**: December 3, 2025
**Test Results**: 44 passed, 0 failed, 4 skipped (with regalloc enabled)
**Stage 1**: Compiles and runs (1,101,728 bytes with &key support)

## Current Work

All core tests passing. MCP server integrated and working.

**Next Steps**:
1. Close stale beads related to register allocator (tests now pass)
2. Work on native reader features (4 skipped tests need it)
3. Continue toward full self-hosting

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
- `[x27+0]`: from_start
- `[x27+8]`: to_start
- `[x27+16]`: from_end
- `[x27+24]`: half_heap_size
- `[x27+48]`: symbol_counter
- `[x27+56]`: symbol_table
- `[x27+64]`: heap data starts

Generational GC mode (extends above):
- `[x27+80]`: nursery-start
- `[x27+88]`: nursery-end (also old-space-start)
- `[x27+96]`: card-table-start
- `[x27+104]`: old-space-half-size
- `[x27+112]`: old-space-flag
- `[x27+120]`: old-space-alloc
- `[x27+128]`: heap data starts

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
