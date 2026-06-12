# habu native code generation

habu compiles checked Forth to **standalone ARM64 macOS executables** — entirely
in Forth, no C and no LLVM. The compiler is hosted on gforth; its *output* is a
self-contained, dyld-loaded, ad-hoc-signed Mach-O that runs with no gforth.

## Quick start

```forth
gforth habu-cg.fs            \ checker + colon override + codegen
CODEGEN-ON? on              \ record checked definitions for codegen
: SQUARE ( i64 -- i64 ) DUP * ;
s" /tmp/sq" HABU-EXE SQUARE  \ emit a standalone CLI executable
bye
```
```sh
/tmp/sq 12        # -> 144     (reads argv[1], computes, prints)
```

`RUN-NATIVE` compiles + runs immediately, returning the exit code:
`7 RUN-NATIVE SQUARE` → 49.

## Pipeline (`bootstrap/cg/`)

| file | role |
| ---- | ---- |
| `icode.fs` | ICode IR — assembler mnemonics append abstract instructions (op + register/immediate/label *fields*) |
| `opt.fs`   | peephole optimizer over IR records (self-mov, arith-0, dead-LIT, branch-to-next) |
| `asm.fs`   | ARM64 encoders IR→machine code; 2-pass label binding; **range-checked** branches/immediates (throw, never wrap) |
| `templ.fs` | per-primitive and control-structure ICode generators over the data stack |
| `walk.fs`  | tokenize a word body, drive the generators (number→literal, prim, or call) |
| `rt.fs`    | native runtime routines: `.` (itoa + write), `atoi` (parse argv) |
| `link.fs`  | subroutine ABI, transitive dependency collection, multi-word layout + `MAIN`/CLI entry |
| `macho.fs` | compose a minimal dynamic Mach-O in memory |
| `exec.fs`  | write + `codesign -f -s -` + run (gforth built-ins; no FFI) |
| `install.fs` | `CODEGEN-HOOK` wiring + `CODEGEN-ON?` |

Architecture: **ICode** (SwiftForth-style "assembly in Forth") — mnemonics build
an IR, the optimizer rewrites it, encoders emit bytes. Registers/immediates/labels
are IR fields, so the optimizer and register allocation work on structured
instructions.

## ABI

- `x19` (Xds) = data-stack pointer, threaded through calls (push/pop mutate it,
  never restored).  `x20` = DATA base.  (Historical note: this doc describes the
  early gforth-hosted codegen. The **live engine's** register map is wider — the
  JIT register pool is 13 (`x9`–`x15`, `x29`, `x25`, `x23`, `x24`, `x21`, `x22`)
  defined by `VRPACK`/`VRPACK2` in `regalloc.f`, plus a `d8`–`d15` float pool;
  tokenizer state (INP/INE/TKA/TKL/PEND) and `?DO` loop frames live in DATA
  header cells, not pinned registers. See `src/habu/regalloc.f` and `jit.f`.)
- A word is a native subroutine: args/results on the Xds stack; non-leaf words
  save/restore `x30`; calls are `BL`, `RECURSE` calls self.
- CLI entry: `x0`=argc, `x1`=argv; `argv[1]` parsed by `atoi`, result printed
  by `.`, `exit(0)`.
- OS via emitted `svc #0x80` (`write`=4, `exit`=1); no C, no libSystem calls.

## Supported subset

`DUP DROP SWAP OVER NIP`, `+ - * / MOD 1+ 1- NEGATE`, `AND OR XOR`,
`< > = <= >= <> 0= 0< 0>`, `IF/ELSE/THEN`,
`BEGIN/UNTIL/AGAIN/WHILE/REPEAT`, `?DO/DO/LOOP/I`, `EXIT`, `RECURSE`,
word→word calls, `.`. Numbers are i64 literals.

Words whose body uses anything outside this subset are skipped by the hook
(the threaded definition is unaffected). `?DO/LOOP` is single-level (one loop
register pair).

## Tests

`test/t-cg-asm.fs`, `t-cg-opt.fs` (encoder/optimizer, in `test/all.fs`).
End-to-end (slow, exec per case, run explicitly): `t-cg-exe.fs` (raw ICode),
`t-cg-word.fs`/`t-cg-ctrl.fs` (body strings), `t-cg-hook.fs` (checked words +
calls + recursion), `t-cg-io.fs` (`.` output), `t-cg-cli.fs` (standalone CLI).

## Not yet

The compiler still runs on gforth. A **fully self-hosting** habu (the compiler
itself compiled to native, gforth dropped) needs a native Forth runtime —
interpreter, dictionary, `evaluate` — which is the remaining long pole
(`CODEGEN-PLAN.md` Part F).
