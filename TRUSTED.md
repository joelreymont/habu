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

- **Byte-for-byte differential tests** — the standalone emitter output is compared
  against the gforth-hosted bootstrap emitter for the same source. Any drift fails.
- **Behavioral tests** — the emitted engine is run on real programs and its stdout
  is compared against the expected result.

`Last audited` is the date a human last confirmed the declared effect matches the
body. Re-audit when a row's body or effect string changes.

`tools/trust-lint.py` enforces this manifest: every `TRUST` site in `src/` must
have a row here, and every row must cite a test.

| Word | Effect | Reason | Tests | Site | Last audited |
|------|--------|--------|-------|------|--------------|
| fprim | `n n n --` | Raw-asm prim emitter: lays a REG-PRIM frame + `xt execute`s a code-emitting handler; no Forth effect to infer. | `test/t-sh-habu1.fs`, `test/t-sh-prims.fs` | src/habu/habu1.f:93 | 2026-06-13 |
| fprim-l | `n n n --` | Leaf variant of FPRIM (no x30 frame); same `xt execute` of a code emitter. | `test/t-sh-habu1.fs`, `test/t-sh-prims.fs` | src/habu/habu1.f:100 | 2026-06-13 |
| emit-prims | `--` | Emits the engine's whole primitive table as raw ARM64. | `test/t-sh-habu1.fs`, `test/t-sh-flow.fs` | src/habu/habu1.f:395 | 2026-06-13 |
| emit-fp-prims | `--` | Emits the floating-point prim table as raw asm via FPRIM-L. | `test/t-sh-fp.fs` | src/habu/habu1.f:469 | 2026-06-13 |
| cf-entry | `n n n n --` | Control-flow keyword case: spills the VS then `hxt execute`s a code emitter; raw labels + indirect xt defeat inference. | `test/t-sh-habu2.fs`, `test/t-sh-flow.fs` | src/habu/habu2.f:633 | 2026-06-13 |
| cfn-entry | `n n n n --` | Like CF-ENTRY, no spill (loop words manage the VS); `hxt execute` of a raw emitter. | `test/t-sh-habu2.fs`, `test/t-sh-flow.fs` | src/habu/habu2.f:643 | 2026-06-13 |
| cfb-entry | `n n n n n --` | Branch-keyword case (if/until/while) with a reg-aware condition path; asm + two `hxt execute` handlers. | `test/t-sh-habu2.fs`, `test/t-sh-jit.fs` | src/habu/habu2.f:670 | 2026-06-13 |
| cfbn-entry | `n n n n n --` | Like CFB-ENTRY, no-spill register path; raw asm + indirect xts. | `test/t-sh-habu2.fs`, `test/t-sh-jit.fs` | src/habu/habu2.f:691 | 2026-06-13 |
| em-interpret | `--` | Emits the interpreter-mode main-loop dispatch as raw ARM64. | `test/t-sh-habu2.fs`, `test/t-sh-repl.fs` | src/habu/habu2.f:914 | 2026-06-13 |
| em-compile | `--` | Emits the compile-mode main-loop dispatch as raw ARM64. | `test/t-sh-habu2.fs`, `test/t-sh-repl.fs` | src/habu/habu2.f:1066 | 2026-06-13 |
| emit-main | `--` | Allocates main-loop labels and chains EM-STARTUP/COMMENT/INTERPRET/COMPILE. | `test/t-sh-habu2.fs`, `test/t-sh-flow.fs` | src/habu/habu2.f:1071 | 2026-06-13 |
| emit-forth | `n n --` | Top-level engine builder: allocates every forward-ref label and emits the complete image. | `test/t-sh-habu2.fs`, `test/t-sh-stage2.fs` | src/habu/habu2.f:1119 | 2026-06-13 |
| fold-entry | `n n n n --` | JIT constant-fold case: emits the keyword guard then `fxt execute`s a fold handler + raw branches. | `test/t-sh-fold.fs`, `test/t-sh-jit.fs` | src/habu/jit.f:104 | 2026-06-13 |
| vop-entry | `n n n n n --` | JIT binop case: fold-vs-emit split with two indirect xts and raw asm. | `test/t-sh-fold.fs`, `test/t-sh-jit.fs` | src/habu/jit.f:259 | 2026-06-13 |
| vopi-entry | `n n n n n n --` | JIT binop-immediate case: fold/register/immediate split with three indirect xts and raw asm. | `test/t-sh-jit.fs`, `bench/typed-codegen/run.sh` | src/habu/jit.f:324 | 2026-06-14 |
| vshuf-entry | `n n n n n --` | JIT reg-aware stack-shuffle case (dup/over/swap/drop/nip as register moves); `sxt execute` + raw asm. | `test/t-sh-jit.fs`, `test/t-sh-fold.fs` | src/habu/jit.f:613 | 2026-06-13 |
| vun-entry | `n n n n n --` | JIT unary-op case: con-fold vs in-place reg op via `foldxt`/`emitxt` and raw asm. | `test/t-sh-jit.fs`, `test/t-sh-fold.fs` | src/habu/jit.f:646 | 2026-06-13 |
| emit-prof-prims | `--` | Emits the sampling-profiler prims as raw asm via FPRIM-L. | `test/t-sh-prof.fs` | src/habu/prof.f:77 | 2026-06-13 |
