# habu — Native Backend: Self-Hosting ICode Codegen Plan

A typed native ARM64 code generator for habu, building toward a **standalone,
self-hosting native compiler**. habu's checked, row-polymorphic stack effects are
type information the codegen exploits: they let the selector pick *untagged,
width-correct, unboxed* code that an untyped Forth JIT cannot emit soundly.

**End-state (committed):** a standalone macOS ARM64 habu executable that compiles
its own checker + codegen sources to native and reaches a **self-compile
fixpoint** (stage1 emits stage2, and stage2 ≡ stage3), with **gforth dropped**.
gforth is the **bootstrap host and the differential oracle only**. Self-hosting
requires building a small Forth runtime (Part F) — accepted as the long pole.

> **STATUS (2026-06-12): the end-state is REACHED — Parts A–F DONE, fixpoint
> holds.** `bin/hb` is a standalone macOS ARM64 Forth engine that compiles the
> whole toolchain source (`src/`), type-checks it with its built-in checker
> (self-checks clean — see `STATUS.md` for the count), and **rebuilds itself
> byte-for-byte** (stage2 ≡ stage3; `tools/build.sh` is the no-gforth daily
> loop, `tools/bootstrap.sh` regenerates from nothing). gforth is bootstrap +
> differential oracle only. The runtime `:` compiler JIT-allocates registers
> (jit: constant folding, register ops, fused branches, loop-resident
> registers across BEGIN loops — 30M-iteration counter loop in 0.016 s).
> In-binary disassembler, sampling profiler, and crash handler ship inside the
> engine; `tools/{probe,imgdump,jitdump,clobber-lint,shadow-lint}`
> are the dev loop. The 2026-06-12 wave closed the remaining gaps: full
> control flow (`exit recurse ?do +loop j leave unloop >r r> r@`),
> user-extensible compile words (`immediate postpone compile, create does>`),
> quotations (`[: ;]` engine + checker types), trust declarations, reject
> diagnostics, typed locals, encodeBitMasks, the engine-run behavior gate
> (test/hb-suite.f via the `run-rc` spawn prim), and TRUE AOT: tools/snap-hb.sh
> emits a dictionary-snapshot binary that boots the whole toolchain WARM in
> ~3 ms (fixed-VA regions + startup relocation of engine-text references).
> The toolchain self-checks clean (see `STATUS.md`). Sections below are the historical design
> record; their "remaining/not started" markers predate this status.
>
> **PATHS:** the design body cites the gforth-hosted checker as bare
> `src/<file>.fs` (e.g. `src/colon.fs`, `src/checker.fs`, `src/db.fs`) — those
> files now live under **`bootstrap/src/`**. The NATIVE self-hosted toolchain
> (what `bin/hb` compiles and re-checks) is `src/{core,arch/arm64,habu,os/macos}`.
> So read every bare `src/<name>.fs` below as `bootstrap/src/<name>.fs`.

## Foundational principle: everything in Forth, no C

**No authored C. No compiled C shim. No `libcc`-generated wrapper.** Proven on
this host (live spikes, 2026-06-09 — see *Proven facts* below), the C-free design
is:

- **Execution vehicle = compose the Mach-O image in memory → write to disk →
  invoke it** (a proven self-host loop). gforth cannot execute our raw bytes
  in-process (its `code` words are engine-internal threaded dispatch; hand-built
  bodies fault — proven), and we author no C call-thunk. So the Forth-hosted
  compiler **assembles the whole Mach-O in a memory buffer, writes it** with
  gforth's *built-in* file I/O (`create-file`/`write-file`), ad-hoc signs it, and
  **invokes it** via gforth's *built-in* `system` — no FFI, no `libcc`, no
  authored C. The payload grows across the plan: early it is a **minimal Mach-O
  test stub** (fixed load commands + one patchable `__TEXT` slot, Phase 0.1); at
  self-host it is **the new Forth itself** — a complete invokable habu (Part F).
  Differential testing follows the same loop: early, per-word stubs (Part B);
  once the new Forth exists, **invoke it on habu's own `T{ … -> … }T` corpus** and
  diff transcripts against gforth (stack comparison stays in-process inside each
  Forth, as the tester already does) — no per-word spawns, no bespoke serializer.
- **Mach-O is dynamic, never static.** Fully-static dyld-less binaries are
  SIGKILLed by AMFI even ad-hoc signed (proven). The artifact carries
  `LC_LOAD_DYLINKER` + `LC_MAIN` + `LC_LOAD_DYLIB libSystem` + a `__LINKEDIT`
  ad-hoc `LC_CODE_SIGNATURE`. **This needs no C:** dyld is the loader; linking
  libSystem satisfies AMFI; we never author or compile C.
- **The OS is reached two C-free ways, both from emitted machine code:** (1)
  direct `svc #0x80` syscalls (number in `x16`, args `x0–x5`, carry = error —
  proven for `write`/`exit`); (2) **emitted calls to libSystem symbols bound by
  dyld** — emit the Mach-O bind/chained-fixup opcodes so dyld fills a `__DATA`
  pointer, then `BLR` through it. Calling libSystem requires no C.
- **W^X / icache, all emitted:** `mmap` RW → write → `mprotect` RX → execute
  (proven; no `MAP_JIT`, no `pthread_jit_write_protect_np` — the call that hung
  through libcc is never used); i-cache flushed by an emitted `IC IVAU / DSB ISH /
  ISB` stub (proven to run from EL0).

The one unavoidable *tooling* touch is `codesign -s -` (ad-hoc) as a build step,
invoked via `system` — a signing tool, not C, comparable to needing an assembler.

## Proven facts (live spikes on this host, 2026-06-09)

These settled the substrate's biggest unknowns; the plan is built on them, not on
optimism. gforth 0.7.9_20260513 arm64, macOS 26.5.

| Claim | Result |
| ----- | ------ |
| gforth `code … end-code` hosting raw emitted bytes | **DEAD** — `,` is 8-byte (need `l,`); even byte-correct `l,`-built code words fault ("Invalid memory address"); gforth is primitive-centric dynamic-native threaded; no usable host assembler. → no in-process exec under gforth. |
| `svc #0x80`, BSD number in `x16`, no `0x2000000` mask | **CORRECT** — emitted `write(1,…)` printed to stdout; `exit` worked. Pin: `write=4 exit=1 mmap=197 mprotect=74`. |
| `mmap` RW → `mprotect` RX (no `MAP_JIT`) | **CORRECT** — executed; writing the RX page faults, so W^X is genuinely enforced (the 0.1 probe won't pass vacuously). |
| emitted `IC IVAU/DSB ISH/ISB` from EL0 | **CORRECT** — no trap. |
| self-emitted **static** Mach-O, ad-hoc signed | **SIGKILL (rc 137)** even signed. A **dynamic** Mach-O (`LC_LOAD_DYLINKER`+`LC_MAIN`+libSystem) with svc-only code **runs, returns 42**. |
| habu's own sources are stencil-emittable | **NO** — they are ~entirely metaprogramming (`evaluate`, `parse`, `wordlist`, `find-name`, `defer`/`is`, `catch`/`throw`, `{: :}` locals). Self-host needs a Forth runtime (Part F). |

## Why stencils, and what ports from habu

habu (`~/Work/habu`) is a live, working ARM64 codegen; we port its *recipes and
engine*, not its host coupling. (habu paths are `src/jit/…` and `src/ir/…`;
unqualified `jit.zig` would wrongly resolve to a 191-line `bench/jit.zig` decoy.)

- **Inline-stencil composition, not tail-call copy-and-patch.** habu emits
  stencils contiguously into one linear function with intra-function branch
  fixups (`src/jit/jit.zig:104-127,483-538,1000-1010`) — no `MUSTTAIL`, no runtime
  C compiler. This is what makes it Forth-portable.
- **A stencil is data:** `{bytes, holes:[{offset,type,name}]}`
  (`src/jit/stencils.zig:33-50`). We **assemble stencils at load with our own
  Forth encoders** (Phase 1) — golden-tested, ctx offsets as named Forth
  constants (a load-time assertion replaces habu's lost `@offsetOf` guard).
  Under ICode, registers are IR operand fields — parameterization habu's frozen
  bytes cannot do.
- **Five hole types, pure bit-math** (`src/jit/stencils.zig:19-30`, patchers
  `src/jit/patch.zig:226-288`): `imm64`, `imm32`, `rel26`, `rel19`, `rel14`. A
  **`reg` hole is not among them — moot under ICode: registers are IR fields.** **Position-independence
  note:** prefer `rel*`/`adrp+add` (PC-relative) over `imm64` absolute holes in
  `__TEXT`, so the fixpoint (Phase 7) is reachable (§Goal).
- **habu's private VM-stack ABI ports cleanly** (`src/jit/stencils.zig:305-330,
  586-650`): habu owns its stacks (`Xds`/`Xrs`) over its own arenas.

Not taken: habu's bytecode dispatcher (`src/interp/vm.zig:597-652`), its fixnum
tagging / overflow slow-paths (`src/jit/jit.zig:908-937` — habu's types delete the
tag), Zig comptime authoring, the unused SSA scaffolding
(`src/jit/ir.zig`,`verify.zig`). Golden vectors are thin (5,
`src/ir/arm64.zig:607-624`); several encoders live in `stencils.zig` or only as
bare hex — we regenerate a full set. No general logical-immediate encoder exists —
implement `decodeBitMasks` if needed.

## Goal & success condition

**Goal:** habu-checked words run as native ARM64, **faster than gforth-fast** on a
real inner loop, **bit-identical** to the gforth-threaded reference for every
checked word and example, culminating in a **standalone self-compiling habu**.

**Bit-identical, two tiers.** Comparison is always over *what is observed across a
process boundary*, never a raw in-memory diff of the subprocess.
- **Mature (once the new Forth exists, Part F):** run habu's own `T{ … -> … }T`
  corpus under both gforth-threaded and the new habu Forth; the tester compares
  stack results *in-process inside each Forth*, so the cross-process diff is just
  the test transcript + stdout. This is the primary, trusted path.
- **Early (Parts B–E, before the new Forth):** the per-word test stub's epilogue
  serializes, via the Part-A runtime, the full data-stack span (depth + every cell
  as raw 64-bit hex), the full return-stack span, every written scratch-arena
  region, and stdout; the gforth oracle serializes identically. The serializer is
  then in the trusted base — a `TRUSTED:`, golden-tested routine printing
  *depth-derived full spans* (never a fixed arity), or a miscompiled cell it omits
  is invisible (see Risks).

**Fixpoint (compiler-level):** reproducibility needs more than PC-relative code —
it needs a full **determinism checklist** the emitter satisfies: (a) emit
**position-independent `__TEXT`** (PC-relative `adrp+add`/`rel*`; no `imm64`
absolute addresses in code; `__DATA` rebased by dyld); (b) **omit `LC_UUID`** (or
zero it); (c) zero all Mach-O / `__LINKEDIT` timestamps; (d) emit symbols/strings
in a **stable sorted order**; (e) traverse `CODE-TABLE` for emission in a
**deterministic order** (by name or definition index — never wordlist hash
order); (f) no arena base address in `__DATA` *initial* contents (only
dyld-rebased fixup offsets). Success = **stage2 ≡ stage3 byte-identical** on the
*normalized file image* (excluding `LC_CODE_SIGNATURE` **and** `LC_UUID`), where
stage1 = gforth-hosted habu emits the native habu; stage2 = native habu emits itself;
stage3 = stage2 emits itself. stage1 ≠ stage2 is expected (different host); only
stage2 ≡ stage3 must hold.

**Gates (any failure → write `LESSONS.md` and stop):** (1) Phase-0 passes
(Mach-O test stub runs as a subprocess C-free; emitted syscalls work; ≥2× speed
gap); (2) the differential suite (native ≡ threaded over `test/`+`examples.fs`,
on the adversarial corpus) is green; (3) the fixpoint holds; (4) the bench meets
target over gforth-fast.

## Reality check (grounded in code, 2026-06-09)

Paths repo-relative; sources under `src/`.

1. **Every checked word is gforth-threaded today.** `src/colon.fs:41–44,79–84`
   re-emits `: NAME body ;` via the saved native colon (`NCOLON`,
   `src/colon.fs:6–7`) through `EVALUATE`. **This threaded build is the
   differential oracle.**
2. **No typed IR survives checking.** `CHECK-DEF` (`src/checker.fs:72–88`) walks,
   unifies, `CHART`s only the canonical signature string (`src/db.fs:13–19` →
   `RENDER-EFFECT` `src/render.fs:126`); the typed trace and bindings are
   discarded (`src/checker.fs:76`). Body text lives in `CAP$` (persistent buffer,
   `src/capture.fs:5,8`; filled at `src/colon.fs:74`). → **Codegen re-derives the
   op list (arity from `EFFECT-OF`, `src/db.fs:21`, via `PARSE-SIG`→`STACK-ARITY`
   `src/rows.fs:61`) and the types (`RESOLVE-TYPE` `src/types.fs:43`, `RESOLVE-ROW`
   `src/rows.fs:42`) by re-walking the body.** (`src/sig.fs` is a dead orphan —
   never `require`d, its `RESOLVE-TYPE` at `:195` is a dead dup of `types.fs:43`;
   do not cite it.)
3. **Type tags** (`src/config.fs:24–34`, `src/types.fs`): `(payload<<3)|tag`;
   `TC-I64=1 U8=2 U32=3 CELL=4 BOOL=5 CHAR=6 STR=7 ADDR=8`.
4. **Primitive DB** (`src/prims.fs`, `src/db.fs`): every prim, `TRUSTED:` word,
   constant, variable, and ordinary checked word is `CHART`ed into **one
   undifferentiated `EFFECTS` wordlist** (`src/db.fs:5`); `EFFECT-OF` can't tell
   them apart, and control words aren't charted (`src/control.fs:82`,
   `src/forward.fs:11`). Tiering eligibility is an **explicit native allow-list**
   (Phase 3), not an `EFFECT-OF` heuristic; `TRUSTED:` words need a positive
   marker (Phase 3).
5. **Combinators run via `execute`** (`src/runtime.fs:1–14`). A quotation literal
   `[: … ;]` is checked by `DO-QUOT-LIT` (`src/quots.fs:14–22`, terminator scan in
   `CHECK-QSEG` `src/quots.fs:6–11`) into a `quot<effect>` term — **body ops
   discarded, only the effect survives**; native inlining re-locates the span in
   `CAP$` and re-walks it.
6. **habu is the asset; dixie/hoist are not** (dixie = decoder; hoist = regalloc
   unimplemented, emission stubbed).
7. **Seam — one site.** Edit `src/colon.fs:80`
   (`code 0= if RE-EVAL-SAFE exit then`) to insert `CODEGEN-HOOK` before `exit`;
   declare `defer CODEGEN-HOOK ( -- )` in `src/forward.fs` (default no-op). The
   hook takes **no stack args**; reads `NM@`/`CAP$` (live at line 80). The
   `checker.fs:88` site is wrong (stale colon-owned `NM@/EF@`). Gated by
   `CODEGEN-ON?` defaulting **off**.

## ABI (habu owns it — no host coupling)

`Xds` = data-stack ptr, `Xrs` = return-stack ptr, `Xtos` = TOS accumulator,
scratch `X9–X13`, `X16` = syscall number. Stacks/arenas are habu-owned (Phase 2).
A compiled word is **position-independent**: it materializes its own arena bases
from a rebased `__DATA` slot (dyld-bound), not from baked absolute addresses.
Syscalls: `x16`, `svc #0x80`, carry = error. Native words call native words
directly. The Mach-O test stub wraps a word with a print-observables epilogue.

## Architecture

```
checked def ──CHECK-DEF──▶ charted scheme (unchanged) ─── gforth-threaded word = ORACLE
                  │  (src/colon.fs:80, after RE-EVAL-SAFE)
                  └─CODEGEN-HOOK──▶ WALK-OPS (ops + OP-ARITY) → ANNOTATE-TYPES (RESOLVE-TYPE)
                                          │
                                     stencil selection (prim × resolved-shape, sparse)
                                          │  transactional: snapshot pos; on throw restore + skip
                                     inline-compose stencils + patch holes (range-checked, PC-rel)
                                          │
                                     record NAME→{bytes,holes,entry} in the habu CODE-TABLE
                                          │
              bootstrap differential: emit NAME's Mach-O test stub → run via `system`
                                       → compare 4 observables vs threaded oracle (one process pair)
              Part F/G: link all entries + runtime → dynamic Mach-O → self-compile fixpoint
```

**v1 is all-or-nothing:** a word using an op without a stencil/runtime routine is
not emitted yet. For the **standalone build every word must emit**, or the build
fails loudly. Codegen never overwrites the threaded oracle.

Files (`bootstrap/cg/`, `require`d at end of `habu.fs` after `src/colon.fs`;
`defer CODEGEN-HOOK` in `src/forward.fs`; `cg/install.fs` runs
`' DO-CODEGEN is CODEGEN-HOOK` **last**, after the bank is assembled+asserted):

**Code generation is ICode (SwiftForth-style "assembly in Forth"), DONE for the
core op set:** mnemonics append abstract instructions (5-cell records — op +
register/immediate/label *fields*) to an IR buffer; a peephole optimizer rewrites
the IR; encoders then emit machine code. This subsumes the original
byte-stencils-with-holes design: "holes" are just IR operand fields, "stencils"
become per-prim ICode generator words, and the Phase-5 "register hole" problem
disappears (registers were never frozen into bytes).

- `cg/icode.fs` — **DONE.** ICode IR: records, labels, ~45 mnemonics incl.
  `svc`/cache-flush/`adr`. **TRUSTED:**.
- `cg/opt.fs` — **DONE (seed rules).** Peephole over IR records (self-mov,
  arith-0, dead-LIT, branch-to-next); killed records never break labels.
- `cg/asm.fs` — **DONE.** ARM64 encoders IR→u32, table-dispatched; labels bound
  in PASS1; branches/immediates **range-checked, throw, never wrap**; `LIT64,`
  synthesizes minimal MOVZ/MOVN+MOVK chains. Golden-tested
  (`test/t-cg-asm.fs`, `t-cg-opt.fs`; in `test/all.fs`). **TRUSTED:**.
- `cg/macho.fs` — Mach-O emit: the minimal **test stub template** (Phase 0.1) and
  the full multi-word linker (Part G). **TRUSTED:**.
- `cg/exec.fs` — write + `codesign` + `system`-run a Mach-O, capture output.
- `cg/abi.fs` — habu register/stack ABI; frame/offset named constants (asserted).
- `cg/rt.fs` — minimal native runtime: stack arenas, bump memory arena, syscall
  I/O routines, entry/exit, runtime code-allocator. **TRUSTED:**.
- `cg/walk.fs` — `WALK-OPS`+`OP-ARITY`, `ANNOTATE-TYPES`, `[: … ;]` spans.
- `cg/templ.fs` — per-prim **ICode generator words** (append IR), not byte blobs.
- `cg/sel.fs` — selection: typed op → ICode generator variant (generic / not-yet).
- `cg/install.fs` — the `CODE-TABLE` (a dedicated wordlist mirroring `EFFECTS`,
  entry body = native entry + length; plus an insertion-ordered array for
  deterministic Part-G emission), allow-list, hook wiring.

## Build order & dependency spine (critical path)

```
A substrate ─▶ B codegen ─▶ C typed ─▶ D regalloc ─▶ E completeness ─┐
(macho-stub,asm,rt)  (engine,MVP,diff)  (unbox)     (TOS,LSRA)  (control,combs,locals)
                                                                    ▼
                                                F runtime substrate ─▶ G self-host
                                                (interp,dict,evaluate)   (full linker,fixpoint)
```

Hard path is **A → B → … → E → F → G** (F/G need everything emittable). C/D/E
improve B's output and may be reordered among themselves; all precede F. No phase
forward-depends on a later one: I/O prims are hand-emitted runtime routines in A
(assembler only); arithmetic/memory prims are inline stencils in B/C; the Forth
*system* (interpreter/dictionary/`evaluate`) is Part F, needed only by self-host.

---

## Part A — No-C substrate (foundations)

### Phase 0 — Feasibility gates (three kill-switches; do not skip)

0.1 **C-free execution vehicle.** Build the minimal **dynamic Mach-O test
    template** in `cg/macho.fs`: fixed `__PAGEZERO`/`__TEXT`/`__LINKEDIT`,
    `LC_LOAD_DYLINKER`+`LC_MAIN`+`LC_LOAD_DYLIB libSystem`, one patchable `__TEXT`
    code slot, ad-hoc `LC_CODE_SIGNATURE` (emit, or apply via `system" codesign
    -s -"`). `cg/exec.fs` writes it (`create-file`/`write-file`) and runs it
    (`system`), capturing stdout/exit. Splice `MOVZ X0,#42; svc exit` into the
    slot. *Accept:* the subprocess exits 42 — proving emit→sign→run with **no C,
    no FFI, no libcc**. (mprotect/svc/icache are already proven; this proves the
    *vehicle*.)
0.2 **Emit working syscalls** from the stub: `write(1,…)` then `exit`. Pin macOS
    ARM64 numbers (`write=4 exit=1 mmap=197 mprotect=74`, `svc #0x80`, x16, no
    `0x2000000` mask) → `LESSONS.md` + named constants. *Accept:* the stub prints
    a known string and exits; bytes verified.
0.3 **The speed gap is real.** Inner loop (arith + `@`/`c@` + a branch) with a
    hand-written native baseline in `bench/inner-loop.s`; historical gforth
    microbench wrappers were removed once gforth became bootstrap-only. *Accept:*
    native timings justify backend work; else write the finding and **stop**.

### Phase 1 — ARM64 assembler (Forth, golden-tested)

1.1 `cg/asm.fs`: port register/immediate recipes from `src/ir/arm64.zig:24-214`
    (aliases `mov=orr Xd,XZR,Xn`, `cmp=subs XZR`, `cset=csinc` inverted, `:132-135`;
    `hw=shift/16` MOVZ/MOVK; LDR/STR pre-scaled imm12) **and**
    `src/jit/stencils.zig:56-203` (movz/movk, UBFM/SBFM shifts `:104-122`,
    stp_pre/ldp_post). Fresh encoders for bare-hex CSET/CMP/TST
    (`src/jit/stencils.zig:489-582`), for `svc`/`IC IVAU`/`DSB`/`ISB`, and for
    **`adrp`+`add` / `adr`** (PC-relative, for position-independence). Add
    `decodeBitMasks` only if `AND/ORR/EOR #imm` is needed. *Accept:* `test/t-asm.fs`
    — each encoder reproduces its static vector (below) **or** its execution probe.
1.2 **Validate encoders — no external toolchain (no LLVM/clang).** Two layers,
    both in-house: (a) a small **static vector set** hand-derived from the ARM ARM
    + habu's 5 (`src/ir/arm64.zig:607-624`), committed to `test/asm-vectors.fs`,
    for fast bit-level regression; (b) **execution probes — the CPU is the
    authoritative oracle**: for each encoder, emit a tiny program that exercises
    the instruction with known operands and assert the runtime result via the
    compose→write→invoke loop (e.g. `ADD`/`SUB` on known values, `LDR/STR`
    round-trip, a taken/not-taken branch landing). This validates the bits *and*
    their CPU semantics at once, and most encoders are additionally exercised
    end-to-end by the Phase-3 differential corpus. **Encoders execution cannot
    isolate (a wrong bit-field that still "works") REQUIRE a static vector:** all
    branch holes (`rel26`/`rel19`/`rel14`), `adrp`+`add`, `ldp_post`/`stp_pre`
    writeback, and `decodeBitMasks` if added. *Accept:* every encoder passes its
    execution probe and/or static vector (the named set: static vector required).
1.3 Little-endian emit + label/fixup table; **every branch patch range-checked**
    (rel26 ±128MB, rel19 ±1MB), typed error on overflow — not
    `src/ir/arm64.zig:305-320`'s unchecked mask. *Accept:* assembles a back-edge +
    forward branch; out-of-range → `E-BRANCH-RANGE`, never a wrap.

### Phase 2 — Minimal native runtime (habu-owned, no C)

2.1 `cg/rt.fs` + `cg/abi.fs`: stack arenas + a bump **memory arena** via emitted
    `mmap`; `Xds/Xrs` materialized from rebased `__DATA` (PC-relative, not baked).
    Frame/offset named constants shared with stencils + a **load-time assertion**
    they match. The **runtime code-allocator** (mmap RX, bump-emit, icache flush)
    for the standalone's later self-recompilation. *Accept:* a stub pushes/pops
    across the arena boundary; assertion passes.
2.2 **I/O as native runtime routines** (hand-emitted via `cg/asm.fs`, called by
    `BL` — not stencils, no engine dep): `EMIT/TYPE/./CR` via `write`; `exit`.
    *Accept:* an emitted program prints via the habu runtime (not gforth) and
    exits; stdout matches. (Memory-access prims `@ ! c@ c!` are Part-B inline
    stencils by design — the Part-A runtime owns stacks+arena+I/O, not arena
    *access*.)

---

## Part B — Correct native codegen

### Phase 3 — ICode pipeline + untyped MVP end-to-end

3.1 **ICode engine — DONE** (`cg/icode.fs`, `cg/opt.fs`, `cg/asm.fs`): IR records
    with labels, peephole optimizer, table-dispatched encoders, range-checked
    branches/immediates, `LIT64,` minimal constant synthesis. Golden-tested
    (`test/t-cg-asm.fs`, `test/t-cg-opt.fs`; wired into `test/all.fs`).
    Remaining 3.1 work: **transactional compile** at the word level (on `throw`
    mid-generation: `ICODE-RESET`, skip the word, continue; model the cleanup on
    `RE-EVAL-SAFE`'s `catch`, `src/colon.fs:47-51`). *Accept:* a mid-generation
    `throw` leaves the next word compiling correctly.
3.2 `cg/walk.fs` `WALK-OPS` + `OP-ARITY` = `EFFECT-OF` (`src/db.fs:21`, yields a
    scheme *string*, not a number) → `PARSE-SIG` (`src/sigparse.fs:203`) →
    `EFF>DIN`/`EFF>DOUT` (`src/effects-repr.fs:12-13`) → `STACK-ARITY`
    (`src/rows.fs:61`). `cg/templ.fs`: generic width-cell **ICode generators** for
    `DUP/DROP/SWAP/+/-/*/1+/@/!/c@/c!` + literal push (`LIT64,`).
    `cg/install.fs`: record `NAME→{entry,len}` in the `CODE-TABLE`
    (dedicated wordlist; `nextname create` the entry) **and** append it to an
    **insertion-ordered array** (definition index) — Part-G emission traverses
    that array, never wordlist hash order, so the fixpoint stays deterministic
    (§Goal e); the wordlist is for lookup only; the **native allow-list**
    (prims-with-generators + control words + Phase-6.2 combinators) gates
    eligibility; `TRUSTED:` words carry a positive marker (tag
    `src/defining.fs:38`) and are never compiled. *Accept:*
    `: SQUARE ( i64 -- i64 ) DUP * ;` records a native entry; its Mach-O test stub
    prints `49` for input 7 ≡ threaded.
3.3 **Differential harness — early tier** (per-word stub vs in-gforth threaded
    oracle). To bound subprocess cost, emit **one stub per word, not per input**:
    the stub embeds an input-vector table and a driver loop, runs the word per
    input, and prints delimited observable blocks; sign+invoke once per word
    (spawn count ≈ #words, not #words×#inputs). The oracle serializes identically
    in gforth; compare; assert codegen fired. **Adversarial corpus:** boundary
    ints (INT_MAX/MIN, 0, −1, ÷0), width-overflow u8/u32,
    empty/juggle/recursive/large bodies, fault-injection. **Every later
    "differential ≡" runs this corpus** (via the mature `T{ }T`-through-the-new-
    Forth path once Part F lands). *Accept:* MVP op set + `SQUARE` pass.

---

## Part C — The typed payoff

### Phase 4 — Typed stencil specialization

4.1 `cg/walk.fs` `ANNOTATE-TYPES`: annotate each op's slots with resolved concrete
    types (`RESOLVE-TYPE` `src/types.fs:43`). *Accept:* op list of `DUP *` shows
    i64 slots (ohsnap snapshot).
4.2 **Typed variants** (sparse bank): `i64` add uses an untagged 64-bit stencil
    with **no fixnum tag/untag and no fixnum-range slow-path**
    (`src/jit/jit.zig:908-937`). **First pin gforth per-op semantics** in
    `LESSONS.md`: wrap (`+ - * AND OR XOR LSHIFT RSHIFT`) vs **trap** (`/ MOD` ÷0 /
    `INT_MIN/-1` — native `SDIV ÷0 → 0` but gforth throws). Trapping ops **must
    reproduce the trap**; delete guards only for proven-wrapping ops. `u8/u32` use
    `W` regs + `UXTB/UXTW` with a **canonical truncation point** matching gforth
    (truncate only at store). `ptr<τ>` = raw address, no mask. Fall back to generic
    when types aren't concrete. *Accept:* u8 accumulator overflowing 8 bits then
    `c!`/`c@` ≡ threaded; `5 0 /` and `INT_MAX 1 +` ≡ threaded; Phase-0.3 loop
    shows the gap.
4.3 Peephole: fuse push/pop of the same slot, fold `LIT n +`, drop redundant
    `MOV`s, fuse `LDR/STR`→`LDP/STP`. *Accept:* `test/t-opt.fs` — **each named
    fusion fires** on a canned word that triggers it (assert the specific
    instruction-count delta per fusion, not a vague "count drops"); corpus
    differential ≡.

---

## Part D — Performance

### Phase 5 — Register caching & allocation (from scratch; habu has neither)

> 5.1/5.2 of the old stencil design ("reg hole type", register-parameterized
> stencils) are **obsolete — ICode already carries registers as IR fields**;
> generators take register arguments for free. Phase 5 is now purely the
> allocator.

5.1 **TOS-in-register**, then keep the top N data-stack cells in registers across a
    basic block; spill only at block boundaries/calls (effect arity bounds N).
    *Accept:* `SQUARE` emits **zero memory traffic** for its body (assert non-zero
    buffer for empty/pure-juggle bodies); differential ≡.
5.2 Liveness → linear-scan allocation; degrade to memory under pressure (never
    miscompile). *Accept:* a 6-slot loop body compiles without spills;
    differential ≡.

---

## Part E — Completeness (all of checkable habu)

### Phase 6 — Control flow, combinators, locals

6.1 Control flow via range-checked branch holes: `IF/ELSE/THEN`,
    `BEGIN…UNTIL/AGAIN/WHILE…REPEAT`, `?DO/DO…LOOP/+LOOP`, `EXIT`, `RECURSE`
    (self-call = a fixup hole patched after layout; entry published atomically).
    Join points merge regalloc state (checker proves equal arm effects). On a
    branch exceeding range, emit a veneer or **reject the word (whole-word, stay
    threaded — not a runtime deopt)**; never wrap. *Accept:*
    `: FAC ( i64 -- i64 ) DUP 2 < IF DROP 1 ELSE DUP 1- RECURSE * THEN ;` native;
    corpus differential ≡.
6.2 **Combinator inlining:** on `[:`, capture the quotation body span from `CAP$`
    (mirror `CHECK-QSEG`, `src/quots.fs:6-11`) into a nested op list; inline it
    (no `execute`), threading register lifetimes; emit each combinator's loop
    scaffold (`?do/loop`, `cells +`, `src/runtime.fs:8-13`). Non-literal `xt`
    keeps a native indirect call. *Accept:* `[: 1 + ;] DIP` inlines; differential
    ≡; non-literal xt correct.
6.3 **Locals lowering** (`{: … :}`, 81 uses across `src/`): lower named locals to
    register/frame slots over the regalloc model. *Accept:* a word using `{: a b :}`
    compiles native; differential ≡.

---

## Part F — Build the new Forth (runtime substrate; the long pole)

The standalone artifact *is a Forth* — a complete, invokable habu — since habu's own
sources are metaprogramming (Proven facts). This Part builds that new Forth; the
self-host loop is then **compose its Mach-O in memory → write → invoke it**. Each
phase spikes a piece before integrating.

7.1 **Outer interpreter + input source:** tokenizer over an mmap'd source buffer,
    `refill`/`source`/`>in`/`parse`/`parse-name`. *Accept:* the standalone reads a
    `.fs` line and echoes parsed tokens.
7.2 **Dictionary + headers:** `wordlist`/`get-current`/`set-current`/
    `search-wordlist`/`nextname`/`create`/`>body`/`find-name` over habu-owned
    memory. *Accept:* define and find a word at runtime in the standalone.
7.3 **Native `evaluate` + the `:` seam:** a native colon that captures a body,
    invokes `CODEGEN-HOOK`, and installs via the **runtime code-allocator** (2.1)
    — the standalone replacement for gforth `code` words. Plus `defer`/`is`,
    `:noname`/`execute`, `catch`/`throw` unwinding. *Accept:* the standalone
    `evaluate`s `: SQ DUP * ; 7 SQ .` and prints 49.
7.4 **Re-host the checker:** ensure the checker + codegen sources, with
    `TRUSTED:` annotations on the substrate words (7.1–7.3 routines), pass the
    checker and compile. *Accept:* the gforth-hosted habu composes-in-memory →
    writes → the new Forth binary (= stage1) with no un-emittable op remaining;
    invoking stage1 on habu's `T{ }T` corpus matches gforth (the mature
    differential path replaces per-word stubs from here on).

---

## Part G — Self-host & fixpoint (gforth dropped)

8.1 **Full Mach-O linker** (`cg/macho.fs` beyond the stub): lay all native words +
    runtime + dyld load commands + ad-hoc signature into a multi-word dynamic
    executable; **position-independent** `__TEXT`. *Accept:* a multi-word habu
    program builds and runs standalone.
8.2 **Stage 2 — habu compiles habu.** Run stage1 (native habu) on habu's own sources →
    stage2. *Accept:* stage2 compiles the corpus with output ≡ stage1's.
8.3 **Fixpoint — stage 3.** stage2 emits stage3; assert **stage2 ≡ stage3 on the
    normalized image** (exclude `LC_CODE_SIGNATURE` and `LC_UUID`; addresses
    PC-relative so no ASLR variance; per the §Goal determinism checklist).
    gforth now unnecessary. *Accept:* normalized `diff` empty;
    full suite passes built by the native compiler.
8.4 Bench + profile: re-run Phase-0.3 on the standalone; one profile-guided pass.
    *Accept:* meets the Phase-0.3 target; numbers in `LESSONS.md`.

---

## Goal → part/phase coverage

| Goal / constraint                                   | Part · Phase    |
| --------------------------------------------------- | --------------- |
| C-free execution vehicle (dynamic Mach-O subprocess)| A · 0.1 (gate)  |
| OS via emitted syscalls / dyld-bound calls (no C)   | A · 0.2, 2; G · 8.1 |
| Native is worth it (≥2×)                            | A · 0.3 (gate)  |
| Emit correct ARM64 (golden, range-checked, PC-rel)  | A · 1           |
| habu-owned minimal runtime (stacks, arena, I/O)      | A · 2           |
| Working native end-to-end (untyped)                 | B · 3           |
| Bit-identical to threaded (4 observables, corpus)   | B · 3.3 + every "differential ≡" |
| Use effect *types* for unboxing/width/untagging     | C · 4.2 (payoff)|
| Don't break trapping-op semantics                   | C · 4.2         |
| Eliminate data-stack traffic                        | D · 5.1, 5.2    |
| Control flow / combinators / locals native          | E · 6           |
| Forth runtime substrate for the standalone          | F · 7           |
| Standalone executable, gforth dropped               | G · 8.1         |
| Self-compile fixpoint (stage2 ≡ stage3)             | G · 8.2, 8.3    |
| Faster than gforth-fast on a real loop              | A · 0.3; G · 8.4|
| Backend-agnostic front end (checker untouched)      | seam = CODEGEN-HOOK at src/colon.fs:80 |

## Risks & open questions

- **No in-process exec under gforth (proven).** Bootstrap execution is
  compose→write→invoke. Early tier spawns one stub *per word* (inputs batched into
  the stub's driver loop, §3.3), ≈#words spawns; the mature tier invokes the
  single new-Forth binary on the `T{ }T` corpus (§F), so cost stops scaling with
  the corpus.
- **Early-tier serializer is in the trusted base** — observable fidelity is
  bounded by it until the mature `T{ }T`-through-the-new-Forth path takes over
  (§Goal). Golden-test it.
- **Mach-O emission (0.1, 8.1)** is detailed/unforgiving (load commands, page
  alignment, `__LINKEDIT` ad-hoc CodeDirectory). The 0.1 stub de-risks it early;
  the full linker is real work. **Must be dynamic** (dyld+libSystem) — static is
  SIGKILLed (proven).
- **Part F is the long pole** — a Forth system (interpreter, dictionary,
  `evaluate`, `catch`/`throw`, the runtime code-allocator) ≈ the size of Parts
  A–E. The committed self-host goal accepts this.
- **Fixpoint position-independence** — `__TEXT` must avoid `imm64` absolute
  addresses (use `adrp+add`/`rel*`; arena bases from rebased `__DATA`), else
  stage2 ≠ stage3. Normalized comparison excludes the signature blob.
- **Trap vs wrap / width masking (4.2)** — delete a fixnum slow-path only for
  proven-wrapping ops; `/ MOD ÷0` keep the trap; mask `u8/u32` only at gforth's
  truncation points.
- **W^X enforcement** is real on this host (proven) — the standalone controls its
  own signing; the runtime code-allocator uses `mprotect` RW↔RX.
- **Phase 5 is a real allocator**, not polish.
- **No logical-immediate encoder in habu** — implement `decodeBitMasks` (1.1).

## Out of scope (now)

x86-64 backend; cross-compilation; a real GC (bump arena only — no free in v1);
**fully-static binaries** (impossible on this macOS — the artifact is a dynamic,
dyld-loaded, ad-hoc-signed Mach-O, with zero authored C); the TUI REPL (separate
dot). macOS ARM64 only.
