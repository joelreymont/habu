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

`tools/trust-lint.f` repository mode enforces this manifest for every `TRUST`
site in the default scanned roots, `src/` and `lib/`, and every row must cite a
test. `tools/check.f --source-list` also invokes `trust-lint source-only` on each
named input path, so source-list certified tool, test, and benchmark sources use
the same manifest rows below. Rows outside `src/` and `lib/` are validated when
that source is explicitly certified; they are not stale-checked by the default
`src/`/`lib/` repository scan.

| Word | Effect | Reason | Tests | Site | Last audited |
|------|--------|--------|-------|------|--------------|
| RPD@ | `-- ptr u8` | Reads the primitive-name pool cursor stored in a raw variable; audited accessor preserves byte-pointer type across native `@`. | `test/run.f` | src/habu/habu1.f:104 | 2026-06-16 |
| fprim | `ptr u8 n n --` | Raw-asm prim emitter: lays a REG-PRIM frame + `xt execute`s a code-emitting handler; no Forth effect to infer. | `test/run.f` | src/habu/habu1.f:126 | 2026-06-16 |
| fprim-l | `ptr u8 n n --` | Leaf variant of FPRIM (no x30 frame); same `xt execute` of a code emitter. | `test/run.f` | src/habu/habu1.f:133 | 2026-06-16 |
| spawn-dup2-action | `n n --` | Build-side helper that emits one raw XNU `PSFA_DUP2` file-action record append; label/register code is not inferable as a Forth data transform. | `test/proc-pty.f`, `test/engine-suite.f` | src/habu/habu1.f:300 | 2026-06-15 |
| spawn-chdir-action | `n --` | Build-side helper that emits one raw XNU `PSFA_CHDIR` file-action record append and NUL-path copy; label/register code is not inferable as a Forth data transform. | `lib/process-cwd-test.f`, `test/run.f` | src/habu/habu1.f:317 | 2026-06-21 |
| emit-prims | `--` | Emits the engine's whole primitive table as raw ARM64. | `test/run.f` | src/habu/habu1.f:874 | 2026-06-13 |
| emit-fp-prims | `--` | Emits the floating-point prim table as raw asm via FPRIM-L. | `test/run.f` | src/habu/habu1.f:948 | 2026-06-13 |
| cf-entry | `n ptr a n n --` | Control-flow keyword case: spills the VS then `hxt execute`s a code emitter; keyword label cell is a pointer. | `test/run.f` | src/habu/habu2.f:1007 | 2026-06-16 |
| cfn-entry | `n ptr a n n --` | Like CF-ENTRY, no spill (loop words manage the VS); keyword label cell is a pointer. | `test/run.f` | src/habu/habu2.f:1017 | 2026-06-16 |
| cfb-entry | `n ptr a n n n --` | Branch-keyword case (if/until/while) with a reg-aware condition path; asm + two `hxt execute` handlers. | `test/run.f` | src/habu/habu2.f:1045 | 2026-06-16 |
| cfbn-entry | `n ptr a n n n --` | Like CFB-ENTRY, no-spill register path; raw asm + indirect xts. | `test/run.f` | src/habu/habu2.f:1066 | 2026-06-16 |
| em-interpret | `--` | Emits the interpreter-mode main-loop dispatch as raw ARM64. | `test/run.f` | src/habu/habu2.f:1305 | 2026-06-13 |
| em-compile | `--` | Emits the compile-mode main-loop dispatch as raw ARM64. | `test/run.f` | src/habu/habu2.f:1514 | 2026-06-13 |
| emit-main | `--` | Allocates main-loop labels and chains EM-STARTUP/COMMENT/INTERPRET/COMPILE. | `test/run.f` | src/habu/habu2.f:1519 | 2026-06-13 |
| SRCA@ | `-- ptr u8` | Reads EMIT-FORTH's saved source pointer from a raw variable for the final `BYTES,` copy. | `test/run.f` | src/habu/habu2.f:1522 | 2026-06-16 |
| emit-forth | `ptr u8 n --` | Top-level engine builder: consumes source bytes, allocates every forward-ref label, and emits the complete image. | `test/run.f` | src/habu/habu2.f:1572 | 2026-06-16 |
| fold-entry | `n ptr a n n --` | JIT constant-fold case: emits the keyword guard then `fxt execute`s a fold handler + raw branches. | `test/run.f` | src/habu/jit.f:103 | 2026-06-16 |
| vop-entry | `n ptr a n n n --` | JIT binop case: fold-vs-emit split with two indirect xts and raw asm. | `test/run.f` | src/habu/jit.f:324 | 2026-06-16 |
| vopi-entry | `n ptr a n n n n --` | JIT binop-immediate case: fold/register/immediate split with three indirect xts and raw asm. | `test/run.f` | src/habu/jit.f:323 | 2026-06-16 |
| vshuf-entry | `n ptr a n n n --` | JIT reg-aware stack-shuffle case (dup/over/swap/drop/nip as register moves); `sxt execute` + raw asm. | `test/run.f` | src/habu/jit.f:684 | 2026-06-16 |
| vun-entry | `n ptr a n n n --` | JIT unary-op case: con-fold vs in-place reg op via `foldxt`/`emitxt` and raw asm. | `test/run.f` | src/habu/jit.f:717 | 2026-06-16 |
| emit-prof-prims | `--` | Emits the sampling-profiler prims as raw asm via FPRIM-L. | `test/run.f` | src/habu/prof.f:79 | 2026-06-13 |
| DIP | `R a [ R -- S ] -- S a` | Body checks, but TRUST pins the public higher-order scheme in the baked image instead of relying on build-time inference. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:5 | 2026-06-16 |
| KEEP | `R a [ R a -- S ] -- S a` | Body checks, but TRUST pins the public higher-order scheme in the baked image instead of relying on build-time inference. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:9 | 2026-06-16 |
| BI | `R a [ R a -- R b ] [ R b a -- R b c ] -- R b c` | Preserves one quotation while executing another; expressing that directly would require recursive quotation types. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:18 | 2026-06-16 |
| TRI | `R a [ R a -- R b ] [ R b a -- R b c ] [ R b c a -- R b c d ] -- R b c d` | Preserves later quotations while executing earlier ones; expressing that directly would require recursive quotation types. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:22 | 2026-06-16 |
| TIMES | `R i64 [ R -- R ] -- R` | Counted loop keeps the quotation available across repeated `execute`; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:26 | 2026-06-16 |
| EACH | `R ptr a i64 [ R a -- R ] -- R` | Array iterator keeps the quotation across element calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:30 | 2026-06-16 |
| MAP | `R ptr a i64 [ R a -- R a ] -- R` | Array map keeps the quotation across element calls and mutates cells in place; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:34 | 2026-06-16 |
| FOLD | `R ptr a i64 b [ R b a -- R b ] -- R b` | Array fold keeps the quotation across accumulator calls; direct checked code would require a recursive quotation type. | `test/engine-suite.f`, `test/run.f` | src/core/combinators.f:38 | 2026-06-16 |
| >IDX | `n -- idx` | Runtime identity cast from a generic cell to the nominal index role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:10 | 2026-06-22 |
| IDX>N | `idx -- n` | Runtime identity cast from the nominal index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:12 | 2026-06-22 |
| >LEN | `n -- len` | Runtime identity cast from a generic cell to the nominal length role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:15 | 2026-06-22 |
| LEN>N | `len -- n` | Runtime identity cast from the nominal length role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:17 | 2026-06-22 |
| >COUNT | `n -- count` | Runtime identity cast from a generic cell to the nominal count role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:20 | 2026-06-22 |
| COUNT>N | `count -- n` | Runtime identity cast from the nominal count role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:22 | 2026-06-22 |
| >OFF | `n -- off` | Runtime identity cast from a generic cell to the nominal offset role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:25 | 2026-06-22 |
| OFF>N | `off -- n` | Runtime identity cast from the nominal offset role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:27 | 2026-06-22 |
| >FD | `n -- fd` | Runtime identity cast from a generic cell to the nominal file-descriptor role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:30 | 2026-06-22 |
| FD>N | `fd -- n` | Runtime identity cast from the nominal file-descriptor role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:32 | 2026-06-22 |
| >RC | `n -- rc` | Runtime identity cast from a generic cell to the nominal return-code role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:35 | 2026-06-22 |
| RC>N | `rc -- n` | Runtime identity cast from the nominal return-code role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:37 | 2026-06-22 |
| >PID | `n -- pid` | Runtime identity cast from a generic cell to the nominal process-id role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:40 | 2026-06-22 |
| PID>N | `pid -- n` | Runtime identity cast from the nominal process-id role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:42 | 2026-06-22 |
| >MS | `n -- ms` | Runtime identity cast from a generic cell to the nominal millisecond role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:45 | 2026-06-22 |
| MS>N | `ms -- n` | Runtime identity cast from the nominal millisecond role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:47 | 2026-06-22 |
| >NS | `n -- ns` | Runtime identity cast from a generic cell to the nominal nanosecond role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:50 | 2026-06-22 |
| NS>N | `ns -- n` | Runtime identity cast from the nominal nanosecond role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:52 | 2026-06-22 |
| >TOK | `n -- tok` | Runtime identity cast from a generic cell to the nominal token-index role; the checker cannot infer nominal role refinement from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:55 | 2026-06-22 |
| TOK>N | `tok -- n` | Runtime identity cast from the nominal token-index role back to a generic cell; the checker cannot infer nominal role erasure from an empty body. | `test/gate-engine.f`, `test/run.f` | src/core/roles.f:57 | 2026-06-22 |
| TTHROWS-RAW | `a n --` | Test assertion boundary around `catch`; the checker has no model for applying an arbitrary execution token and observing its throw code. | `lib/test-test.f`, `test/run.f` | lib/test.f:61 | 2026-06-18 |
| BUILD-CHECK-RAW | `ptr u8 n -- n` | Build helper boundary around `CHECK!`; the checker cannot certify a source definition by evaluating its own checker recursively. | `lib/build-test.f`, `test/run.f` | lib/build.f:140 | 2026-06-18 |
| MEM-ALLOC-PTR | `n -- ptr u8` | Refines a raw anonymous `mmap` result into a typed byte pointer after size validation and `-1` failure checking; the checker cannot express this syscall-result refinement yet. | `lib/memory-test.f`, `test/run.f` | lib/memory.f:39 | 2026-06-21 |
| EP@ | `-- ptr u8` | Reads the current byte-emission cursor stored in a raw variable; preserves pointer type for byte stores. | `test/run.f` | src/arch/arm64/icode.f:18 | 2026-06-16 |
| BYP@ | `-- ptr u8` | Reads the byte-copy cursor stored in a raw variable during `BYTES,`. | `test/run.f` | src/arch/arm64/icode.f:92 | 2026-06-16 |
| DRP@ | `-- ptr u8` | Reads the disassembler row-table cursor stored in a raw variable; preserves byte-pointer type for table byte/word loads. | `tools/jitdump.f`, `test/run.f` | src/arch/arm64/disasm.f:38 | 2026-06-20 |
| ENV-DATA | `-- ptr n` | Returns the fixed engine data-region header pointer used for argc/argv/envp cells. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:10 | 2026-06-16 |
| ARGV-BASE | `-- ptr n` | Reads the raw argv vector pointer from the engine startup cell. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:15 | 2026-06-16 |
| ARGV | `n -- ptr u8` | Reads a NUL-terminated argv entry from the raw argv vector. | `test/run.f`, `tools/argv-test.f` | src/os/macos/env.f:18 | 2026-06-16 |
| ENVP-BASE | `-- ptr n` | Reads the raw envp vector pointer from the engine startup cell. | `test/run.f`, `test/engine-suite.f` | src/os/macos/env.f:21 | 2026-06-16 |
| ENVP | `n -- ptr u8` | Reads a NUL-terminated envp entry from the raw envp vector. | `test/run.f`, `test/engine-suite.f` | src/os/macos/env.f:24 | 2026-06-16 |
| NULL$ | `-- ptr u8 n` | Returns a typed empty string pair used for absent environment values. | `test/run.f`, `test/engine-suite.f` | src/os/macos/env.f:72 | 2026-06-16 |
| TPP@ | `-- ptr u8` | Reads the temporary-path scratch cursor from a raw variable. | `test/run.f` | src/os/macos/env.f:87 | 2026-06-16 |
| SHK-A@ | `-- ptr u8` | Reads the treeshaker source-buffer pointer stored in a raw variable. | `test/run.f` | src/habu/treeshake.f:10 | 2026-06-16 |
| TA@ | `-- ptr u8` | Reads the current treeshaker token pointer stored in a raw variable. | `test/run.f` | src/habu/treeshake.f:45 | 2026-06-16 |
| MP@ | `-- ptr u8` | Reads the Mach-O output cursor stored in a raw variable. | `test/run.f` | src/os/macos/macho.f:11 | 2026-06-16 |
| PHP@ | `-- ptr u8` | Reads the Mach-O header patch cursor stored in a raw variable. | `test/run.f` | src/os/macos/macho.f:91 | 2026-06-16 |
| SIGA@ | `-- ptr u8` | Reads the code-signing identifier pointer stored in a raw variable. | `test/run.f` | src/os/macos/sign2.f:8 | 2026-06-16 |
| HLP@ | `-- ptr u8` | Reads the code-signing header patch cursor stored in a raw variable. | `test/run.f` | src/os/macos/sign2.f:31 | 2026-06-16 |
| HB@ | `-- ptr u8` | Reads the stdin-engine baked-source buffer pointer stored in a raw variable. | `test/run.f` | src/habu/stdin.f:19 | 2026-06-16 |
| BLD-PB@ | `-- ptr u8` | Reads the standalone-build source buffer pointer stored in a raw variable. | `test/run.f`, `tools/hb-build.f` | src/habu/build.f:18 | 2026-06-16 |
| V-TRUST-SIG | `ptr u8 n ptr u8 n --` | hb-build pre-verifier records source-order defining-word signatures for parsed names; the checker cannot infer a dynamic mutation of its signature table from scanner state. | `tools/hb-build-test.f`, `test/run.f` | src/habu/build.f:122 | 2026-06-21 |
| AOT-PB@ | `-- ptr u8` | Reads the AOT build source buffer pointer stored in a raw variable. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f:15 | 2026-06-16 |
| AOT-DBASE@ | `-- ptr a` | Reads the runtime dictionary base pointer for AOT dictionary-record scans; record fields are mixed, so callers specialize the pointee type at each access. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f:17 | 2026-06-16 |
| AOT-PTR@ | `ptr a -- ptr a` | Reads a dictionary long-name pointer field whose pointee is another address; the checker cannot express this pointer-to-pointer load yet. | `test/run.f`, `tools/hb-build.f` | src/habu/aot.f:20 | 2026-06-16 |
| STB@ | `-- ptr u8` | Reads the snapshot source text base pointer stored in a raw variable. | `test/run.f`, `tools/build-fixpoint.f snap` | src/habu/snap.f:21 | 2026-06-16 |
| SDB@ | `-- ptr u8` | Reads the snapshot dictionary/data-region pointer stored in a raw variable. | `test/run.f`, `tools/build-fixpoint.f snap` | src/habu/snap.f:23 | 2026-06-16 |
| SBUF@ | `-- ptr u8` | Reads the stage2 source buffer pointer stored in a raw variable. | `test/run.f` | src/habu/stage2.f:12 | 2026-06-16 |
| JSON-A@ | `-- ptr u8` | Reads the JSON parser source pointer stored in a raw variable; callers need a typed byte pointer after parser setup. | `tools/json-only-test.f`, `test/run.f` | tools/json.f:101 | 2026-06-21 |
| JSON-GKA@ | `-- ptr u8` | Reads the JSON object-key scratch pointer stored in a raw variable; preserves byte-pointer type for key comparisons. | `tools/json-only-test.f`, `test/run.f` | tools/json.f:103 | 2026-06-21 |
| JSONL-A@ | `-- ptr u8` | Reads the JSONL input pointer stored in a raw variable; the streaming cursor keeps pointer state outside the checked stack. | `tools/json-only-test.f`, `test/run.f` | tools/json.f:105 | 2026-06-21 |
| JSONL-LA@ | `-- ptr u8` | Reads the current JSONL line pointer stored in a raw variable; preserves byte-pointer type for per-line parsing. | `tools/json-only-test.f`, `test/run.f` | tools/json.f:107 | 2026-06-21 |
| JSONL-CATCH-LINE | `-- i64` | Recovery boundary around one JSONL parse line; the checker cannot model `catch` over dynamic parser state yet. | `tools/json-only-test.f`, `bench/llm/report-test.f`, `test/run.f` | tools/json.f:744 | 2026-06-21 |
| LKIND | `-- ptr n` | Reads the source-lexer kind table pointer stored in a raw variable after OS-backed allocation. | `tools/lint/text-foundation-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | tools/lint/source-lex.f:18 | 2026-06-21 |
| LADDR | `-- ptr n` | Reads the source-lexer token-address table pointer stored in a raw variable after OS-backed allocation. | `tools/lint/text-foundation-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | tools/lint/source-lex.f:19 | 2026-06-21 |
| LLEN | `-- ptr n` | Reads the source-lexer token-length table pointer stored in a raw variable after OS-backed allocation. | `tools/lint/text-foundation-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | tools/lint/source-lex.f:20 | 2026-06-21 |
| LBYTE | `-- ptr n` | Reads the source-lexer byte-offset table pointer stored in a raw variable after OS-backed allocation. | `tools/lint/text-foundation-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | tools/lint/source-lex.f:21 | 2026-06-21 |
| LLINE | `-- ptr n` | Reads the source-lexer line table pointer stored in a raw variable after OS-backed allocation. | `tools/lint/text-foundation-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | tools/lint/source-lex.f:22 | 2026-06-21 |
| LCOL | `-- ptr n` | Reads the source-lexer column table pointer stored in a raw variable after OS-backed allocation. | `tools/lint/text-foundation-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | tools/lint/source-lex.f:23 | 2026-06-21 |
| LCADDR | `-- ptr n` | Reads the source-lexer comment-content address table pointer stored in a raw variable after OS-backed allocation. | `tools/lint/text-foundation-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | tools/lint/source-lex.f:24 | 2026-06-21 |
| LCLEN | `-- ptr n` | Reads the source-lexer comment-content length table pointer stored in a raw variable after OS-backed allocation. | `tools/lint/text-foundation-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | tools/lint/source-lex.f:25 | 2026-06-21 |
| LEX-ALLOC-CELLS | `n -- ptr n` | Refines an OS-backed byte allocation sized from cells into a typed cell pointer for lexer tables. | `tools/lint/text-foundation-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | tools/lint/source-lex.f:27 | 2026-06-21 |
| LEX-COPY-CELLS | `ptr n ptr n n --` | Performs a raw byte move for typed cell-table growth; the checker cannot express cell-count-to-byte-count refinement here. | `tools/lint/text-foundation-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | tools/lint/source-lex.f:30 | 2026-06-21 |
| BF-TMP-OVERRIDE$ | `-- ptr u8 n` | Reads the build-fixpoint temp override pointer and length stored in raw variables. | `tools/build-fixpoint-test.f`, `tools/hb-build-test.f`, `test/run.f` | tools/build-fixpoint.f:42 | 2026-06-21 |
| BF-TMP! | `ptr u8 n --` | Stores the build-fixpoint temp override pointer and length in raw variables for child build paths. | `tools/build-fixpoint-test.f`, `tools/hb-build-test.f`, `test/run.f` | tools/build-fixpoint.f:45 | 2026-06-21 |
| MR-LINE! | `ptr u8 n --` | Stores the current model-registry line pointer and length in raw variables across row-field parsing. | `bench/llm/model-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/model.f:38 | 2026-06-21 |
| MR-LINE$ | `-- ptr u8 n` | Reads the current model-registry line pointer and length from raw variables. | `bench/llm/model-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/model.f:42 | 2026-06-21 |
| MRUN-OUT-BUF | `-- ptr u8` | Reads the OS-backed model stdout buffer pointer stored in a raw variable after capacity allocation. | `bench/llm/model-run-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/model-run.f:30 | 2026-06-21 |
| MRUN-ERR-BUF | `-- ptr u8` | Reads the OS-backed model stderr buffer pointer stored in a raw variable after capacity allocation. | `bench/llm/model-run-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/model-run.f:33 | 2026-06-21 |
| MRUN-TEXT-BUF | `-- ptr u8` | Reads the OS-backed parsed model-text buffer pointer stored in a raw variable after capacity allocation. | `bench/llm/model-run-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/model-run.f:36 | 2026-06-21 |
| PR-PARSER! | `ptr u8 n --` | Stores response-parser configuration pointer and length in raw variables shared by parser dispatch. | `bench/llm/drive-array-habu-repair-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/parse-resp-lib.f:49 | 2026-06-21 |
| PR-TOKEN-FIELDS! | `ptr u8 n --` | Stores response-parser token-field configuration pointer and length in raw variables shared by parser dispatch. | `bench/llm/drive-array-habu-repair-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/parse-resp-lib.f:53 | 2026-06-21 |
| PR-PARSER-CONFIG$ | `-- ptr u8 n` | Reads response-parser configuration pointer and length from raw variables. | `bench/llm/drive-array-habu-repair-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/parse-resp-lib.f:57 | 2026-06-21 |
| PR-TF-CONFIG$ | `-- ptr u8 n` | Reads response-parser token-field configuration pointer and length from raw variables. | `bench/llm/drive-array-habu-repair-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/parse-resp-lib.f:60 | 2026-06-21 |
| PR-PARSE-RESP | `--` | Recovery boundary around model response parsing; JSON syntax/type errors fall back to raw text, while capacity/internal parser failures rethrow. | `bench/llm/drive-array-habu-repair-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/parse-resp-lib.f:209 | 2026-06-21 |
| LR-SET$ | `ptr u8 n ptr n ptr n --` | Stores live-row string pointer and length cells through raw address parameters; checker lacks typed field references. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:87 | 2026-06-21 |
| LR-RUN-ID$ | `-- ptr u8 n` | Reads the live-row run-id pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:133 | 2026-06-21 |
| LR-NAME$ | `-- ptr u8 n` | Reads the live-row task-name pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:136 | 2026-06-21 |
| LR-MODEL-ID$ | `-- ptr u8 n` | Reads the live-row model-id pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:139 | 2026-06-21 |
| LR-MODEL$ | `-- ptr u8 n` | Reads the live-row model-label pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:142 | 2026-06-21 |
| LR-ARM$ | `-- ptr u8 n` | Reads the live-row arm pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:145 | 2026-06-21 |
| LR-SEED$ | `-- ptr u8 n` | Reads the live-row seed pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:148 | 2026-06-21 |
| LR-OUTCOME$ | `-- ptr u8 n` | Reads the live-row outcome pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:151 | 2026-06-21 |
| LR-FAMILY$ | `-- ptr u8 n` | Reads the live-row family pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:154 | 2026-06-21 |
| LR-MODEL-VERSION$ | `-- ptr u8 n` | Reads the live-row model-version pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:157 | 2026-06-21 |
| LR-MODEL-DATE$ | `-- ptr u8 n` | Reads the live-row model-date pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:160 | 2026-06-21 |
| LR-FIRST-CHECKER$ | `-- ptr u8 n` | Reads the live-row first-checker-status pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:163 | 2026-06-21 |
| LR-RUNTIME-STATUS$ | `-- ptr u8 n` | Reads the live-row runtime-status pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:166 | 2026-06-21 |
| LR-REPAIR-CLASS$ | `-- ptr u8 n` | Reads the live-row repair-class pointer and length stored in raw variables. | `bench/llm/live-row-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/live-row.f:169 | 2026-06-21 |
| DS-SET$ | `ptr u8 n ptr n ptr n --` | Stores driver string pointer and length cells through raw address parameters; checker lacks typed field references. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:122 | 2026-06-21 |
| DS-NAME$ | `-- ptr u8 n` | Reads the driver task-name pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:144 | 2026-06-21 |
| DS-SIG$ | `-- ptr u8 n` | Reads the driver signature pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:147 | 2026-06-21 |
| DS-CATEGORY$ | `-- ptr u8 n` | Reads the driver category pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:150 | 2026-06-21 |
| DS-TESTS$ | `-- ptr u8 n` | Reads the driver tests-description pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:153 | 2026-06-21 |
| DS-SPEC$ | `-- ptr u8 n` | Reads the driver task-spec pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:156 | 2026-06-21 |
| DS-SEED$ | `-- ptr u8 n` | Reads the driver seed pointer and length stored in raw variables. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:159 | 2026-06-21 |
| DS-LINE! | `ptr u8 n --` | Stores the current manifest row pointer and length in raw variables during driver row scanning. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:307 | 2026-06-21 |
| DS-LINE$ | `-- ptr u8 n` | Reads the current manifest row pointer and length from raw variables during driver row scanning. | `bench/llm/drive-stdlib-test.f`, `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-stdlib-lib.f:311 | 2026-06-21 |
| FTL-TASK-BUF | `-- ptr u8` | Reads the OS-backed task manifest buffer pointer stored in a raw variable. | `bench/llm/forth-task-lines-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/forth-task-lines-lib.f:16 | 2026-06-21 |
| FTL-OUT-BUF | `-- ptr u8` | Reads the OS-backed emitted task-row buffer pointer stored in a raw variable. | `bench/llm/forth-task-lines-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/forth-task-lines-lib.f:19 | 2026-06-21 |
| AS-LINE! | `ptr u8 n --` | Stores the current attempt-solution line pointer and length in raw variables during TSV scanning. | `bench/llm/attempt-solutions-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/attempt-solutions-lib.f:47 | 2026-06-21 |
| AS-LINE$ | `-- ptr u8 n` | Reads the current attempt-solution line pointer and length from raw variables during TSV scanning. | `bench/llm/attempt-solutions-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/attempt-solutions-lib.f:50 | 2026-06-21 |
| AS-TASK-BUF | `-- ptr u8` | Reads the OS-backed task TSV buffer pointer stored in a raw variable. | `bench/llm/attempt-solutions-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/attempt-solutions-lib.f:53 | 2026-06-21 |
| AS-SOL-BUF | `-- ptr u8` | Reads the OS-backed solution source buffer pointer stored in a raw variable. | `bench/llm/attempt-solutions-test.f`, `bench/llm/run-attempts-cli-test.f`, `test/run.f` | bench/llm/attempt-solutions-lib.f:56 | 2026-06-21 |
| DFH-FEEDBACK$ | `-- ptr u8 n` | Reads the Forth driver feedback-mode pointer and length stored in raw variables. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:43 | 2026-06-21 |
| DFH-ARM$ | `-- ptr u8 n` | Reads the Forth driver arm pointer and length stored in raw variables. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:46 | 2026-06-21 |
| DFH-TASK-BUF | `-- ptr u8` | Reads the OS-backed Forth task buffer pointer stored in a raw variable. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:52 | 2026-06-21 |
| DFH-BUNDLE-BUF$ | `-- ptr u8 n` | Reads the OS-backed Forth bundle buffer pointer and capacity from raw variables. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:61 | 2026-06-21 |
| DFH-SCRATCH-BUF$ | `-- ptr u8 n` | Reads the OS-backed Forth scratch buffer pointer and capacity from raw variables. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:64 | 2026-06-21 |
| DFH-BUNDLE$ | `-- ptr u8 n` | Reads the emitted Forth bundle pointer and current length from raw variables. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-lib.f:67 | 2026-06-21 |
| DFHT-SRC-BUF | `-- ptr u8` | Reads the OS-backed large source fixture buffer pointer stored in a raw variable. | `bench/llm/drive-forth-test.f`, `test/run.f` | bench/llm/drive-forth-test.f:10 | 2026-06-21 |
| BMA-LINE! | `ptr u8 n --` | Stores the current manifest-audit line pointer and length in raw variables during TSV scanning. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:31 | 2026-06-21 |
| BMA-LINE$ | `-- ptr u8 n` | Reads the current manifest-audit line pointer and length from raw variables during TSV scanning. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:34 | 2026-06-21 |
| BMA-TAG! | `ptr u8 n --` | Stores the current manifest-audit tag pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:37 | 2026-06-21 |
| BMA-TAG$ | `-- ptr u8 n` | Reads the current manifest-audit tag pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:40 | 2026-06-21 |
| BMA-ID! | `ptr u8 n --` | Stores the current manifest-audit task-id pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:43 | 2026-06-21 |
| BMA-ID$ | `-- ptr u8 n` | Reads the current manifest-audit task-id pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:46 | 2026-06-21 |
| BMA-NAME! | `ptr u8 n --` | Stores the current manifest-audit task-name pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:49 | 2026-06-21 |
| BMA-NAME$ | `-- ptr u8 n` | Reads the current manifest-audit task-name pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:52 | 2026-06-21 |
| BMA-CAT! | `ptr u8 n --` | Stores the current manifest-audit category pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:55 | 2026-06-21 |
| BMA-CAT$ | `-- ptr u8 n` | Reads the current manifest-audit category pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:58 | 2026-06-21 |
| BMA-HARNESS! | `ptr u8 n --` | Stores the current manifest-audit harness pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:61 | 2026-06-21 |
| BMA-HARNESS$ | `-- ptr u8 n` | Reads the current manifest-audit harness pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:64 | 2026-06-21 |
| BMA-CONV! | `ptr u8 n --` | Stores the current manifest-audit convention pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:67 | 2026-06-21 |
| BMA-CONV$ | `-- ptr u8 n` | Reads the current manifest-audit convention pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:70 | 2026-06-21 |
| BMA-TAGS! | `ptr u8 n --` | Stores the current manifest-audit tag-list pointer and length in raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:73 | 2026-06-21 |
| BMA-TAGS$ | `-- ptr u8 n` | Reads the current manifest-audit tag-list pointer and length from raw variables. | `bench/llm/manifest-audit-test.f`, `test/run.f` | bench/llm/manifest-audit.f:76 | 2026-06-21 |
| LBB-SRC-BUF | `-- ptr u8` | Reads the OS-backed large-buffer fixture source pointer stored in a raw variable. | `bench/llm/large-buffer-bundle-test.f`, `test/run.f` | bench/llm/large-buffer-bundle-test.f:19 | 2026-06-21 |
| DAH-CONV$ | `-- ptr u8 n` | Reads the Habu array driver convention pointer and length stored in raw variables. | `bench/llm/drive-array-habu-test.f`, `bench/llm/drive-array-habu-repair-test.f`, `test/run.f` | bench/llm/drive-array-habu-lib.f:45 | 2026-06-21 |
| DAH-VECTORS$ | `-- ptr u8 n` | Reads the Habu array driver vector-spec pointer and length stored in raw variables. | `bench/llm/drive-array-habu-test.f`, `bench/llm/drive-array-habu-repair-test.f`, `test/run.f` | bench/llm/drive-array-habu-lib.f:48 | 2026-06-21 |
| DAH-ARM$ | `-- ptr u8 n` | Reads the Habu array driver arm pointer and length stored in raw variables. | `bench/llm/drive-array-habu-test.f`, `bench/llm/drive-array-habu-repair-test.f`, `test/run.f` | bench/llm/drive-array-habu-lib.f:51 | 2026-06-21 |
