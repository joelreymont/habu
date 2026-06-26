# Maki + Habu-PTX build plan

**Scope.** Maki is the ML *framework* — tensor/array types, the autograd
*orchestration*, optimizers, ONNX import, the training/eval loop, and the
LLM-target experiment — layered on Habu and its PTX kernel backend. The kernel DSL
("checked Triton"), local type inference, and the reverse-mode autograd
*transform* are Habu language/compiler features; Maki is the framework above them.

This file is the handoff. It has been reviewed (`/review-plan`, 4 adversarial
subagents grounded in code on 2026-06-26 — see *Review log* below); the accepted
findings are folded in here. Next it is split (`/small-dots`) and implemented by
another agent **in this workspace** (`~/Work/habu-maki`, branch `maki`).

## Implementation status (2026-06-27)

Built + tested this session (the older sections below predate it):

- **Habu-PTX checked kernels:** M4 tile vocab (`lib/ptx-tile.f`) + checked SAXPY;
  M6 collectives/rows (`lib/ptx-collective.f`) + checked SOFTMAX-ROWS; both AD
  primitives **BROADCAST** and **BLOCK-MAX-SELECT**; verified-gradient
  SOFTMAX-ROWS-BWD. Gate-wired (`ptx-stdlib`), trust-lint green.
- **Reverse-mode AD transform** (`lib/ptx-ad.f`): VJP table + reverse pass that
  auto-derives backwards for **linear + unary-nonlinear (EXP./BLOCK-MAX) +
  binary-nonlinear (\*./B-)** ops; algebraic-simplify peephole; save-vs-recompute
  (VJP-SAVES + cost decision). (`B/`'s adjoint needs a uniform÷uniform op — dotted.)
- **Found already-built (probed, not assumed):** M2 parametric checker; **local
  type inference** (untyped intermediates infer + thread; misuse rejects).
- **Maki framework** (`maki/`, fenced/one-way/extractable, its own gate): tensor
  shape+dtype, **tensor-scale float arrays + tensor SGD**, autograd VJPs with
  **numeric gradcheck**, optimizers (SGD family), losses (MSE/L1), a training loop
  that **converges at tensor scale** (weight tensor [0,0]→[3,4]), ONNX fail-closed
  op-lowering, and the eval harness **checker-as-judge** + pass@k. 9 components,
  all runnable + tested.

**Deep remainder (each a focused session; honestly unbuilt):**

1. **M5 uniformity** — a uniform/lane-varying control effect in `checker.f`
   (reject collectives under divergent control). Only matters for kernels with
   data-dependent branches; current straight-line kernels are trivially uniform.
2. **Rigid-token soundness** — fresh rigid extent/mask minting at constructors so
   the mask/extent-identity negatives reject. Mechanism now precisely located and
   bounded (see `LESSONS.md`): re-parse-per-call instantiation + unique atom
   naming in `SIG-TYPE`/`MK-ATOM`; needs a `checker.f` change + fixpoint rebuild.
3. **PTX codegen → device — WORKING on-device end-to-end (2026-06-27).** This IS an
   Orin (local GPU, `ptxas` 12.6, Tegra `libcuda`, sm_87). The full chain runs and
   is **verified correct on the GPU**: checked SAXPY → emitted PTX →
   `ptxas -arch=sm_87` → cubin → load → **launch** → CPU-golden compare, all via
   the Habu FFI. `tools/ptx/cuda-launch.f` launches SAXPY (x=2.0, y=0, a=3.0) and
   reads back y = 6.0 (f32 0x40C00000) = PASS. The deprecated ≤8-arg launch API
   (`cuFuncSetBlockShape`/`cuParamSetv`/`cuLaunchGrid`) avoids `cuLaunchKernel`'s
   11 args, and the real driver entry points are the **`_v2`** memory symbols
   (the earlier INVALID_CONTEXT was symbol versioning, not an ABI gap) — **no
   engine change needed**. The eval-matrix data path is now OPEN: this generalizes
   to the M11 kernel set + the maki/eval harness to produce the pass@k/GB-s matrix.

The thesis "better target" claim stays **unmade** until item 3 produces the data.

## Review log (2026-06-26, folded in)

Four parallel reviewers (completeness, specificity/realism, edge-cases, scout)
re-grounded in `docs/`, the dot chain, the checker surface, and the gate lints.
The load-bearing corrections, all verified against code:

- **M2 is already built+landed — corrected by measurement (2026-06-27).** The
  review flagged "M2 has no dot" (ptx-sketch.md §Milestones #2 sizes it "large")
  and inferred it was an unbuilt gate. **That inference was wrong.** The
  parametric-type machinery is implemented in `src/core/checker.f` and works in the
  installed `bin/hb`: `SIG-TYPE`/`MK-PARAM` parse `span<space-global,f32,extent-n>`;
  the unifier does field-by-field param unify; `render.f` round-trips; `KERNEL:`/
  `GRID:`/`WHERE`/`%BLOCK` check (lib/ptx-test.f runs clean). Empirically a matching
  parametric sig certifies (exit 0); `space-global`→`space-shared` and `extent-r`→
  `extent-c` mismatches reject with field-precise diagnostics (exit 70). The "no
  dot" was because M2 was *done*, not unbuilt. **The real checker-track frontier is
  M4 (the tile *operation* vocabulary — MK-SPAN/GRID-CTX/LOAD/STORE/SCALE/
  collectives — none of which exist yet), now UNBLOCKED.** The only owed M2 remnant
  is a committed parametric type-mismatch negative-regression suite (the rejects
  work but aren't pinned in the gate). Lesson recorded in LESSONS.md: ground
  capability claims in the checker source, not the dot tracker + spec.
- **Maki (workstream D) is named, not designed.** No `maki/` dir, no design doc
  for tensors / optimizers / losses / ONNX / training / eval orchestration. Only
  the *Habu* features (A=ptx-sketch.md, B=inference.md, C=autograd.md) are
  specced. → D is gated on design docs (see *Maki design docs owed*).
- **"Verified gradients" is type-verified, not numerically verified.** The checker
  proves address-space/extent/mask/uniformity — *not* derivative correctness. A
  wrong VJP entry or wrong algebraic rewrite type-checks and ships a silently
  wrong gradient. There is no finite-difference gradcheck anywhere. The review
  found a concrete instance: `autograd.md` `OVER` adjoint was wrong (a fan-out
  treated as a permutation). → gradcheck is now a hard gate; the claim is scoped;
  `docs/autograd.md` is patched.
- **The "read multiplicity already tracked" claim is false.** `docs/effects.md`
  has no multiplicity/linearity effect, and grid-global aliasing is structurally
  invisible to a per-thread checker. → scatter-add is the conservative default;
  the substructural effect is a dotted checker capability.
- **The trust fence is real for the fixpoint + trust-lint + gate allow-list, but
  two repo-wide lints reach into `maki/`** (`tools/host-lint.f:132,88` rejects any
  `.py`; `tools/stale-status-lint.f:441,194` allows only the root `STATUS.md`),
  and **"one-way imports, enforced" has no enforcing code** (no dependency lint
  exists). → *Trust fence — mechanics & maintenance* section added.

## Architecture (decided — see this session's discussion + `docs/`)

Three layers, not two:

1. **Habu core** — checked Forth, JIT/AOT, the trust root (`TRUSTED.md`, the
   byte-for-byte fixpoint, the native gate), the arm64 targets. General-purpose.
2. **PTX kernel backend** — `src/arch/ptx/`, `lib/ptx.f`, the `tile<T,B,M>` type
   system, milestones M1–M11, the **M2 parametric-type checker extension**, a
   general PTX **IR + opt layer** (new — see workstream C / ptx.md §3),
   *inference*, the AD *reverse-pass transform*. A **codegen target of the Habu
   compiler** (shares the IR, checker, encoder).
3. **Maki** — the ML framework. A **`maki/` subdirectory**, with a strict one-way
   dependency (**maki → habu**, never the reverse), **fenced out of the trust
   root** (not in `TRUSTED.md`, not in the fixpoint, not a native-gate dependency).
   Maki source is still **checked/typed Habu** run by `bin/hb` (`CHECKED:` + real
   effects); the fence excludes it from the *trust manifest*, not from the checker.

**Guardrails (keep the subdir extractable):**

- One-way imports: maki loads Habu libraries; Habu core/gate must never reference
  `maki/`. **Today this holds only by convention + the gate's explicit file
  allow-lists — there is NO lint that rejects a `maki/` reference from core.** A
  dependency lint is owed (see *Trust fence*); until it lands, "enforced" is
  "enforced by allow-list + review", stated honestly.
- Maki carries its own `maki/STATUS.md` and a `maki-*` dot **naming convention**
  (the `.dots/` prefix is a single global `habu`; there is no per-subtree
  namespace — a `maki-*` id prefix under one epic subdir is the achievable form).
- Treat the habu↔maki seam as an API even in-repo; that discipline is the future
  repo boundary.
- Extract `maki/` to its own repo when the Habu PTX API stabilizes, maki gets
  external users / a divergent release cadence, or maki churn slows the Habu gate.

## Current state (what already exists)

- `docs/ptx.md`, `docs/ptx-sketch.md` — PTX kernel DSL v0 spec + strategy.
- `docs/inference.md` — local type inference design (infer bodies, annotate the edge).
- `docs/autograd.md` — reverse-mode autograd design + the VJP table (patched by
  this review: `OVER`/`DROP`/`DUP` cotangent types, the numeric-vs-type
  disclosure, BLOCK-MAX tie-break, read-multiplicity default, gradient-buffer
  extent disclosure).
- Dots: `habu-ptx-m1` (+ `m1a` done / `m1b` ready / `m1c`,`m1d` blocked), `m3`
  (active), `m4 … m11`, `habu-ptx-local-type`, `habu-ptx-ad-reverse`, plus the
  `/small-dots` additions (PTX-IR-layer, autograd primitives + pass, 6 Maki
  design-doc gates, Maki impl, the 3 trust-fence dots, the parametric
  negative-regression dot). **M2 is built (closed by evidence), so M4 is the ready
  checker-track frontier.**
- `src/arch/ptx/emit.f` — a **hardcoded one-kernel SAXPY PTX string printer**
  (literal `s" …"` per instruction), not a parametric encoder; M3's device-run
  acceptance is still unmet (needs the M1d CUDA Driver harness on `zed`). The
  encoder is **staged**: a minimal encoder (M3-emit) suffices for M4's "saxpy from
  checked source"; the **full parametric encoder** (arbitrary tiles/collectives/
  matrix) is owed for M6.
- `tools/ptx/` — saxpy emit + `ptxas` smoke (Orin-only).
- `dot ready` currently returns exactly **one** item: `habu-m1b`. That is the only
  buildable frontier today.

## Workstreams

- **A. PTX kernel DSL (Habu).** The M1→M11 chain (`ptx-sketch.md`): the "checked
  Triton." Two large prerequisites inside A, on **separate tracks**:
  - *On-device track (only on `zed`/Orin):* M1 = AAPCS64 FFI + CUDA Driver harness
    (M1b ready → M1c → M1d), then M3 device-run, M8 camera, M9 bench, M11 eval.
    Requires CUDA toolkit (`ptxas` at `/usr/local/cuda-12.6/bin`, not on `PATH`).
  - *Checker track (any dev box, no CUDA):* **M2 parametric-type checker
    extension** (new dot), M4 tile DSL, M5 mask/uniformity, M6 collectives/softmax,
    M6 negatives, plus M3-emit (minimal encoder, for M4) and the full parametric
    encoder (for M6) — see *Current state*. M6 is the gate for autograd; its
    `EXP.` tolerance acceptance transitively needs M1d.
- **B. Local type inference (Habu).** `habu-ptx-local-type`, **after M2+M4** (not
  "parallel to A" — it threads M2's space/extent/mask atom tokens and needs
  tile+locals). Improves all kernel authoring; runs on the checker track.
- **C. Reverse-mode autograd transform (Habu).** `habu-ptx-ad-reverse`, after M6.
  This is a **full compile-time AD pass, not "two primitives"**. New work:
  1. **Two new primitives:** `BROADCAST` (name the implicit broadcast in `B-`/`B/`)
     and the `BLOCK-MAX` arg-max **select** (a masked scatter, with a deterministic
     GPU tie-break — lowest lane index — pinned in *both* the forward argmax and
     the backward scatter).
  2. **A general PTX IR + opt layer (new, untracked prerequisite).** The simplify
     step that collapses a derived backward to closed form needs fold/DCE/CSE/
     peephole over a real PTX IR; ptx.md §3 confirms only a gforth-bootstrap
     peephole exists. Dot it as a prerequisite of the simplify step (or scope
     AD-v0 to literal reversal and dot the closed-form simplifier as a follow-on).
  3. **The reverse pass** over the typed IR word list. **v0 is straight-line tile
     pipelines only:** any forward containing `IF`/loop/`RECURSE` must be **rejected
     fail-closed** with a diagnostic (list-reversal does not reverse control flow —
     that is why PyTorch keeps a tape). Control-flow reversal is a separate dotted
     capability.
  4. **The `VJP:` primitive table** for the M6 forwards. Every entry is itself a
     hand-written backward and must carry a gradcheck (see honesty note).
  5. **Save-vs-recompute** with an **explicit documented cost model** + a test that
     save and recompute produce within-tol-identical gradients.
  6. **Scatter-add** (`red.global.add` — *verify availability on sm_87 first*) for
     accumulating adjoints. Scatter-add is the **conservative default** for every
     `LOAD` adjoint; plain-store is an opt-in *proven-read-once* refinement, never
     a silent guess (the effect system does not track grid read multiplicity).
  Unblocks `SOFTMAX-ROWS-BWD`. Transformer-block backward additionally needs M11
  matmul, not just M6.
- **D. Maki framework (new, `maki/`).** Built on A+B+C. **Each component is gated on
  a design doc** (see below) before it is split into dots:
  - tensor/array types above raw tiles (shapes, dtypes, layouts, broadcasting) —
    needs M4 tiles, **not** the AD transform;
  - autograd *orchestration*: the **tensor-op** VJP rule table (distinct from C's
    **primitive** `VJP:` table — maki's lowers *onto* C's), the user-facing "define
    forward → get a checked backward", optimizer integration;
  - optimizers (SGD/Adam), losses;
  - ONNX import → Habu-PTX kernels (inference deploy needs only forward kernels,
    **not** C);
  - training loop + gradient checkpointing;
  - the **LLM-target eval harness**. Two distinct evals, do not conflate:
    (i) the **kernel-authoring matrix** (ptx-sketch.md §LLM experiment: kernels
    {vector-add, row-reduce, argmax, softmax-row}; arms {Habu-PTX, raw Triton};
    metrics pass@k / repair-rounds / tokens-to-green / GB/s) — already specced;
    (ii) a **Maki model train/eval** (does an imported/trained model reach target
    accuracy) — **not yet specced**. The "raw Triton" + LLM-driver arms are Python;
    AGENTS.md §Habu-Only forbids new Python, and `host-lint` rejects `.py`
    repo-wide — so the harness orchestrator is Habu-native, and any external
    runner is a **named, tested host-glue boundary tracked by a retire-it dot**,
    not loose `.py` under `maki/`.

## Maki design docs owed (before splitting D into dots)

D has no design. Write these to the depth of `inference.md`/`autograd.md` first;
each gates its dots:

- `docs/maki/tensors.md` — tensor type over tiles: shape/rank, dtype set, layout,
  broadcasting rules, the trusted constructor boundary, how shapes map to extent
  tokens.
- `docs/maki/autograd.md` — the **tensor-op** VJP table and the C-vs-D seam (which
  table owns what), the user API, optimizer hand-off.
- `docs/maki/optim.md` — SGD/Adam/loss set + the parameter/gradient update contract.
- `docs/maki/onnx.md` — target opset range, the supported-op subset (mapped to
  existing/M6/M11 kernels), a **fail-closed** policy for unsupported ops (reject
  with a named diagnostic, never silently approximate), and a dynamic-shape policy
  (which axes may be symbolic vs rejected). Round-trip + negative tests required.
- `docs/maki/train.md` — training/eval loop, checkpointing/rematerialization.
- `docs/maki/eval.md` — the two evals above, datasets, model, accuracy metric, and
  the no-Python host-glue boundary for any external arm.

## Trust fence — mechanics & maintenance

The fixpoint (`tools/build-fixpoint.f`), `trust-lint` (`tools/trust-lint-core.f`
scans `src`+`lib` only), and the native gate (`test/run.f` enumerates explicit
phase files) **need zero edits** for maki — it is auto-excluded. The fence leaks
in exactly two repo-wide tree-walkers + one missing lint; these are owed work:

- `tools/host-lint.f:132` walks `.` (whole repo); `:88` rejects any `.py`. Fix:
  maki ships **no `.py`** (preferred, matches Habu-Only), OR add a `maki/` skip in
  `HOST-SCAN-FILE`. Note: editing host-lint to special-case maki itself pierces
  the one-way fence — do it as a deliberate, audited exception with a test.
- `tools/stale-status-lint.f:441` walks `SS-ROOT` (default `.`); `:194`
  `SS-ALLOWED?` whitelists only `STATUS.md`/`LESSONS.md`. A `maki/STATUS.md` (full
  of self-check counts) and any count-bearing `maki/*.md` fail the gate. Fix: add
  `maki/` to `SS-SKIP-PATH?` (`:199`), audited, with a test.
- **Dependency lint (does not exist).** Build a checked lint that fails if any
  `src/`/`lib/`/gate file references a `maki/` path, register it in
  `test/gate-stdlib.f` + `FILEMAP.md` (+ `TRUSTED.md` if it needs a trusted
  boundary). This is what makes "one-way imports, enforced" true.
- `filemap-lint` does **not** require new files to be registered (it only flags
  listed-but-missing / required-but-absent) — adding `maki/` does not trip it.
  Over-worry retracted; no action.
- Document the maki `bin/hb --load` prelude (the habu lib order from
  `docs/bootstrap.md` + `lib/ptx.f` + the PTX layer, then maki files) in the maki
  scaffold; it does not exist yet.

## Sequencing (critical path — two tracks)

The headline is a long single funnel through M1→M2→M4; the parallel axis is
**checker-track (dev box, no CUDA)** vs **on-device-track (`zed`/Orin)**, not
"workstream B parallel to A". Build bottom-up:

**M3 has two halves — split them** so the tracks don't entangle: **M3-emit** (the
PTX encoder / "saxpy from checked source", no CUDA) and **M3-device-run** (launch
the cubin vs CPU golden, needs M1d). M4 needs only M3-emit; M3-device-run lives on
the on-device track and gates nothing on the checker track.

1. **On-device track:** M1b (ready) → M1c → M1d (CUDA Driver harness) →
   M3-device-run vs CPU golden. Unblocks every on-device acceptance.
2. **Checker track (in parallel, no CUDA needed):** ~~M2 parametric checker~~
   (**already built+landed in checker.f** — see *Review log*) + **M3-emit** (minimal
   encoder) → **M4 tile DSL + negatives (the ready frontier)** → M5 mask/uniformity →
   M6 collectives/softmax (needs the parametric encoder, see *Current state*). M6
   is the autograd gate.
3. **PTX IR + opt layer** (new — fold/DCE/CSE/peephole): a pre-M6 parallel item on
   the checker track, after M2/M3-emit; autograd's simplifier *consumes* it.
4. **Inference (B):** after M2+M4, on the checker track — improves all kernels.
5. **Autograd (C):** after M6 (and the IR layer, step 3) — `BROADCAST` + `BLOCK-MAX`
   select; the reverse pass (straight-line only, fail-closed on control flow); the
   `VJP:` table with gradchecks; save-vs-recompute; scatter-add (sm_87-verified).
   Unblocks `SOFTMAX-ROWS-BWD`.
6. **Scaffold `maki/`** (D.4): subdir, one-way dep + the fence-maintenance items,
   own `maki/STATUS.md` / `maki-*` dots, README pointing here, the documented load
   prelude. **No blocker** — a directory + README + dep fence has no technical
   dependency on AD and starts immediately.
7. **Maki tensor types** (after M4, gated on `docs/maki/tensors.md`) and **autograd
   orchestration** (after C, gated on `docs/maki/autograd.md`).
8. **ONNX import** (off forward kernels M4–M6, gated on `docs/maki/onnx.md`) +
   **training loop** + checkpointing.
9. **The eval matrix** — kernel-authoring (M11, behind M7–M10 + the M1d on-device
   gate + M9 bench for GB/s) and the Maki model eval — the validation that earns
   the thesis.

The milestone dots **already carry `blocks:`/blocked-by edges** (e.g.
m4←m3←m1c, m5←m4, m6←m5, m7←m6, m11←m10, `local-type`←m4, `ad-reverse`←m6), so
`/small-dots` must **reconcile and augment**, not recreate ordering from scratch
(re-adding existing edges risks duplicates). The genuinely-owed additions:
`M4←M2`, `local-type`←M2, **retarget `M4←M3` to M3-emit** (not M3-device-run, so
the checker track stays CUDA-free), and edges for the new dots — `PTX-IR-layer`←M2
(+ M3-emit), `ad-reverse`←PTX-IR-layer, maki tensor←M4, maki autograd←`ad-reverse`,
maki onnx←M6. (`+` means AND throughout.)

## Handoff steps (for the picking-up agent)

Work in `~/Work/habu-maki` on branch `maki`. Then:

1. `/review-plan` this `PLAN.md` — **done** (2026-06-26, folded in above).
2. `/small-dots`: create the missing dots — **M2**, the **PTX IR/opt layer**, the
   **dependency lint** + the two fence-skip fixes, the C sub-dots (BROADCAST,
   BLOCK-MAX select + tie-break, reverse pass + control-flow reject, VJP table +
   gradcheck harness, save/recompute + cost model, scatter-add + sm_87 verify),
   and the Maki design-doc dots (one per `docs/maki/*.md`) — then the Maki
   implementation dots gated on those docs. Slot them with `blocked-by` edges per
   *Sequencing*.
3. Implement bottom-up. Respect the guardrails (one-way dep, trust fence). The
   **aot-gate WIP lives in the `default` workspace and is out of scope** — do not
   pull it in (this workspace is clean of it; verified via `jj st`).

## Non-goals / honesty

- Not beating cuBLAS GEMM — `ptx.md` concedes the FLOPS axis. The thesis is
  **checked kernels + verified gradients** as a better target for LLM-authored ML.
- **"Verified gradients" means type-verified, not numerically verified.** The
  checker proves address-space / extent / mask / uniformity — the bug *class* an
  LLM most often fumbles — but **not** that a VJP entry or an algebraic rewrite is
  the correct derivative. A wrong rule type-checks. Therefore: every `VJP:` entry
  and every generated backward **must pass a device-run finite-difference
  gradcheck** (central differences vs the analytic VJP, per-element relative tol)
  as a **hard gate**; the algebraic simplifier carries a numeric-equivalence test
  per rewrite rule. Make no "verified gradient" claim for the derivative-rule
  class until gradcheck is the gate.
- No "better LLM target" claim until step 8 produces the data (both the
  kernel-authoring matrix and the Maki model eval).
