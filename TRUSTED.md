# Trusted base — the `TRUST` manifest

`TRUST` is the checker's escape hatch. `s" name" s" effect" TRUST` declares a
word's stack effect *without* checking its body, so callers are verified against
the declared signature but the body is taken on faith. Every trusted word is part
of the **trusted base**: a soundness cliff. If a declared effect is wrong, the
checker will happily certify programs built on a lie, and the error surfaces far
from its cause. Trust only what *cannot* be inferred — host primitives, raw code
emitters, `CREATE`/`DOES>`, and recursion — and keep the set as small as possible.

Why these words defeat inference: they are the **engine emitters**. Their bodies
are sequences of raw ARM64 encodings (`LBL`, `BL,`, `ADR,`, `STR,`, …) and
`xt execute` calls through token-passed handlers. The checker reasons about Forth
stack effects, not about machine-code labels or indirectly-executed handler xts,
so it cannot derive an effect for them. Their correctness is pinned instead by:

- **Native self-rebuild tests** — the standalone emitter rebuilds the engine from
  current source and compares the next stage against the previous native stage.
  Any drift fails.
- **Behavioral tests** — the emitted engine is run on real programs and its stdout
  is compared against the expected result.

`Last audited` is the date a human last confirmed the declared effect matches the
body. Re-audit when a row's body or effect string changes.

`tools/trust-lint.f` enforces this manifest: every `TRUST` site in `src/` and
`lib/` must have a row here, and every row must cite a test.

| Word | Effect | Reason | Tests | Site | Last audited |
|------|--------|--------|-------|------|--------------|
| RPD@ | `-- ptr u8` | Reads the primitive-name pool cursor stored in a raw variable; audited accessor preserves byte-pointer type across native `@`. | `test/run.sh` | src/habu/habu1.f:88 | 2026-06-16 |
| fprim | `ptr u8 n n --` | Raw-asm prim emitter: lays a REG-PRIM frame + `xt execute`s a code-emitting handler; no Forth effect to infer. | `test/run.sh` | src/habu/habu1.f:111 | 2026-06-16 |
| fprim-l | `ptr u8 n n --` | Leaf variant of FPRIM (no x30 frame); same `xt execute` of a code emitter. | `test/run.sh` | src/habu/habu1.f:118 | 2026-06-16 |
| spawn-dup2-action | `n n --` | Build-side helper that emits one raw XNU `PSFA_DUP2` file-action record append; label/register code is not inferable as a Forth data transform. | `test/proc-pty.f`, `test/engine-suite.f` | src/habu/habu1.f:234 | 2026-06-15 |
| emit-prims | `--` | Emits the engine's whole primitive table as raw ARM64. | `test/run.sh` | src/habu/habu1.f:395 | 2026-06-13 |
| emit-fp-prims | `--` | Emits the floating-point prim table as raw asm via FPRIM-L. | `test/run.sh` | src/habu/habu1.f:469 | 2026-06-13 |
| cf-entry | `n ptr a n n --` | Control-flow keyword case: spills the VS then `hxt execute`s a code emitter; keyword label cell is a pointer. | `test/run.sh` | src/habu/habu2.f:737 | 2026-06-16 |
| cfn-entry | `n ptr a n n --` | Like CF-ENTRY, no spill (loop words manage the VS); keyword label cell is a pointer. | `test/run.sh` | src/habu/habu2.f:747 | 2026-06-16 |
| cfb-entry | `n ptr a n n n --` | Branch-keyword case (if/until/while) with a reg-aware condition path; asm + two `hxt execute` handlers. | `test/run.sh` | src/habu/habu2.f:775 | 2026-06-16 |
| cfbn-entry | `n ptr a n n n --` | Like CFB-ENTRY, no-spill register path; raw asm + indirect xts. | `test/run.sh` | src/habu/habu2.f:796 | 2026-06-16 |
| em-interpret | `--` | Emits the interpreter-mode main-loop dispatch as raw ARM64. | `test/run.sh` | src/habu/habu2.f:914 | 2026-06-13 |
| em-compile | `--` | Emits the compile-mode main-loop dispatch as raw ARM64. | `test/run.sh` | src/habu/habu2.f:1066 | 2026-06-13 |
| emit-main | `--` | Allocates main-loop labels and chains EM-STARTUP/COMMENT/INTERPRET/COMPILE. | `test/run.sh` | src/habu/habu2.f:1071 | 2026-06-13 |
| SRCA@ | `-- ptr u8` | Reads EMIT-FORTH's saved source pointer from a raw variable for the final `BYTES,` copy. | `test/run.sh` | src/habu/habu2.f:1210 | 2026-06-16 |
| emit-forth | `ptr u8 n --` | Top-level engine builder: consumes source bytes, allocates every forward-ref label, and emits the complete image. | `test/run.sh` | src/habu/habu2.f:1259 | 2026-06-16 |
| fold-entry | `n ptr a n n --` | JIT constant-fold case: emits the keyword guard then `fxt execute`s a fold handler + raw branches. | `test/run.sh` | src/habu/jit.f:104 | 2026-06-16 |
| vop-entry | `n ptr a n n n --` | JIT binop case: fold-vs-emit split with two indirect xts and raw asm. | `test/run.sh` | src/habu/jit.f:325 | 2026-06-16 |
| vopi-entry | `n ptr a n n n n --` | JIT binop-immediate case: fold/register/immediate split with three indirect xts and raw asm. | `test/run.sh` | src/habu/jit.f:324 | 2026-06-16 |
| vshuf-entry | `n ptr a n n n --` | JIT reg-aware stack-shuffle case (dup/over/swap/drop/nip as register moves); `sxt execute` + raw asm. | `test/run.sh` | src/habu/jit.f:685 | 2026-06-16 |
| vun-entry | `n ptr a n n n --` | JIT unary-op case: con-fold vs in-place reg op via `foldxt`/`emitxt` and raw asm. | `test/run.sh` | src/habu/jit.f:718 | 2026-06-16 |
| emit-prof-prims | `--` | Emits the sampling-profiler prims as raw asm via FPRIM-L. | `test/run.sh` | src/habu/prof.f:77 | 2026-06-13 |
| DIP | `R a [ R -- S ] -- S a` | Body checks, but TRUST pins the public higher-order scheme in the baked image instead of relying on build-time inference. | `test/engine-suite.f`, `test/run.sh` | src/core/combinators.f:5 | 2026-06-16 |
| KEEP | `R a [ R a -- S ] -- S a` | Body checks, but TRUST pins the public higher-order scheme in the baked image instead of relying on build-time inference. | `test/engine-suite.f`, `test/run.sh` | src/core/combinators.f:9 | 2026-06-16 |
| BI | `R a [ R a -- R b ] [ R b a -- R b c ] -- R b c` | Preserves one quotation while executing another; expressing that directly would require recursive quotation types. | `test/engine-suite.f`, `test/run.sh` | src/core/combinators.f:18 | 2026-06-16 |
| TRI | `R a [ R a -- R b ] [ R b a -- R b c ] [ R b c a -- R b c d ] -- R b c d` | Preserves later quotations while executing earlier ones; expressing that directly would require recursive quotation types. | `test/engine-suite.f`, `test/run.sh` | src/core/combinators.f:22 | 2026-06-16 |
| TIMES | `R i64 [ R -- R ] -- R` | Counted loop keeps the quotation available across repeated `execute`; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.sh` | src/core/combinators.f:26 | 2026-06-16 |
| EACH | `R ptr a i64 [ R a -- R ] -- R` | Array iterator keeps the quotation across element calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.sh` | src/core/combinators.f:30 | 2026-06-16 |
| MAP | `R ptr a i64 [ R a -- R a ] -- R` | Array map keeps the quotation across element calls and mutates cells in place; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.sh` | src/core/combinators.f:34 | 2026-06-16 |
| FOLD | `R ptr a i64 b [ R b a -- R b ] -- R b` | Array fold keeps the quotation across accumulator calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.sh` | src/core/combinators.f:38 | 2026-06-16 |
| TTHROWS-RAW | `a n --` | Test assertion boundary around `catch`; the checker has no model for applying an arbitrary execution token and observing its throw code. | `lib/test-test.sh`, `test/run.sh` | lib/test.f:61 | 2026-06-18 |
| BUILD-CHECK-RAW | `ptr u8 n -- n` | Build helper boundary around `CHECK!`; the checker cannot certify a source definition by evaluating its own checker recursively. | `lib/build-test.sh`, `test/run.sh` | lib/build.f:47 | 2026-06-18 |
| EP@ | `-- ptr u8` | Reads the current byte-emission cursor stored in a raw variable; preserves pointer type for byte stores. | `test/run.sh` | src/arch/arm64/icode.f:18 | 2026-06-16 |
| BYP@ | `-- ptr u8` | Reads the byte-copy cursor stored in a raw variable during `BYTES,`. | `test/run.sh` | src/arch/arm64/icode.f:92 | 2026-06-16 |
| DRP@ | `-- ptr u8` | Reads the disassembler row-table cursor stored in a raw variable; preserves byte-pointer type for table byte/word loads. | `tools/jitdump.f`, `test/run.sh` | src/arch/arm64/disasm.f:38 | 2026-06-20 |
| ENV-DATA | `-- ptr n` | Returns the fixed engine data-region header pointer used for argc/argv/envp cells. | `test/run.sh`, `tools/argv-test.f` | src/os/macos/env.f:9 | 2026-06-16 |
| ARGV-BASE | `-- ptr n` | Reads the raw argv vector pointer from the engine startup cell. | `test/run.sh`, `tools/argv-test.f` | src/os/macos/env.f:14 | 2026-06-16 |
| ARGV | `n -- ptr u8` | Reads a NUL-terminated argv entry from the raw argv vector. | `test/run.sh`, `tools/argv-test.f` | src/os/macos/env.f:17 | 2026-06-16 |
| ENVP-BASE | `-- ptr n` | Reads the raw envp vector pointer from the engine startup cell. | `test/run.sh`, `test/engine-suite.f` | src/os/macos/env.f:20 | 2026-06-16 |
| ENVP | `n -- ptr u8` | Reads a NUL-terminated envp entry from the raw envp vector. | `test/run.sh`, `test/engine-suite.f` | src/os/macos/env.f:23 | 2026-06-16 |
| NULL$ | `-- ptr u8 n` | Returns a typed empty string pair used for absent environment values. | `test/run.sh`, `test/engine-suite.f` | src/os/macos/env.f:42 | 2026-06-16 |
| TPP@ | `-- ptr u8` | Reads the temporary-path scratch cursor from a raw variable. | `test/run.sh` | src/os/macos/env.f:57 | 2026-06-16 |
| SHK-A@ | `-- ptr u8` | Reads the treeshaker source-buffer pointer stored in a raw variable. | `test/run.sh` | src/habu/treeshake.f:11 | 2026-06-16 |
| TA@ | `-- ptr u8` | Reads the current treeshaker token pointer stored in a raw variable. | `test/run.sh` | src/habu/treeshake.f:46 | 2026-06-16 |
| MP@ | `-- ptr u8` | Reads the Mach-O output cursor stored in a raw variable. | `test/run.sh` | src/os/macos/macho.f:11 | 2026-06-16 |
| PHP@ | `-- ptr u8` | Reads the Mach-O header patch cursor stored in a raw variable. | `test/run.sh` | src/os/macos/macho.f:91 | 2026-06-16 |
| SIGA@ | `-- ptr u8` | Reads the code-signing identifier pointer stored in a raw variable. | `test/run.sh` | src/os/macos/sign2.f:8 | 2026-06-16 |
| HLP@ | `-- ptr u8` | Reads the code-signing header patch cursor stored in a raw variable. | `test/run.sh` | src/os/macos/sign2.f:31 | 2026-06-16 |
| HB@ | `-- ptr u8` | Reads the stdin-engine baked-source buffer pointer stored in a raw variable. | `test/run.sh` | src/habu/stdin.f:17 | 2026-06-16 |
| BLD-PB@ | `-- ptr u8` | Reads the standalone-build source buffer pointer stored in a raw variable. | `test/run.sh`, `tools/hb-build.sh` | src/habu/build.f:18 | 2026-06-16 |
| AOT-PB@ | `-- ptr u8` | Reads the AOT build source buffer pointer stored in a raw variable. | `test/run.sh`, `tools/hb-build.sh` | src/habu/aot.f:14 | 2026-06-16 |
| AOT-DBASE@ | `-- ptr a` | Reads the runtime dictionary base pointer for AOT dictionary-record scans; record fields are mixed, so callers specialize the pointee type at each access. | `test/run.sh`, `tools/hb-build.sh` | src/habu/aot.f:16 | 2026-06-16 |
| AOT-PTR@ | `ptr a -- ptr a` | Reads a dictionary long-name pointer field whose pointee is another address; the checker cannot express this pointer-to-pointer load yet. | `test/run.sh`, `tools/hb-build.sh` | src/habu/aot.f:17 | 2026-06-16 |
| STB@ | `-- ptr u8` | Reads the snapshot source text base pointer stored in a raw variable. | `test/run.sh`, `tools/snap-hb.sh` | src/habu/snap.f:21 | 2026-06-16 |
| SDB@ | `-- ptr u8` | Reads the snapshot dictionary/data-region pointer stored in a raw variable. | `test/run.sh`, `tools/snap-hb.sh` | src/habu/snap.f:23 | 2026-06-16 |
| SBUF@ | `-- ptr u8` | Reads the stage2 source buffer pointer stored in a raw variable. | `test/run.sh` | src/habu/stage2.f:12 | 2026-06-16 |
