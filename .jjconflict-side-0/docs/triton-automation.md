# What Triton automates, where Habu stands, and how we automate the same

Recorded 2026-07-20 from the GB10 campaign review (Joel's question: "what are we
automatic vs triton? can we automate what triton automates? how?"). Companion to
`docs/eval-triton.md` (the measured head-to-head, rounds 1-14) and the settled
design in dot `habu-ptx-opt-layer-325b9507`. Current-behavior claims carry
`file:line`; standings quoted are the round-14 table.

## The four axes (the classic table)

| Optimization | CUDA | Triton | Habu today |
|---|---|---|---|
| Memory coalescing | manual | automatic | manual — hand-derived once per kernel family |
| Shared-memory management | manual | automatic | manual — hand-staged, checked + knob-searched |
| Scheduling within SMs | manual | automatic (given knobs) | manual knobs per family; ptxas floor shared with Triton |
| Scheduling across SMs (grid) | manual | manual | manual — winner rows + raster knob |

Habu sits in the CUDA column on all four rows, with three things CUDA doesn't
give: every hand-shaping is a parameterized knob, every knob is measured, and
selection among configurations is automatic per shape (the shape-keyed
autotuner, `tools/ptx/autotune.f`).

## Where Habu stands per axis, precisely

- **Coalescing — manual, derived per family.** The MMA emitter's access
  patterns are hand-derived: the shared-memory epilogue maps a warp's 32 lanes
  onto 32 contiguous C columns (`docs/eval-triton.md` round 3), the `cp.async`
  staging is hand-laid (`lib/ptx/cg-mma.f`). A new kernel family re-derives its
  coalescing by hand. Nothing infers it.
- **Shared memory — manual, checked, searched.** Staging is hand-shaped per
  family; scratchpad sizes are computed from tile geometry with fail-closed
  legality (`E-MMA-SMEM`), and bank-conflict avoidance is two explicit measured
  knobs — `MMA-PAD=8` and the pad-free `MMA-XSWIZ` XOR swizzle (round 13) —
  the autotuner picks between per shape.
- **Within-SM scheduling — manual knobs over the shared ptxas floor.** Warp
  count (`MMA-WARPS`, round 2) and pipeline depth (`MMA-STAGES`; stages ≥ 3
  measured dead on GB10 under the half-smem residency rule, rounds 2 and 13)
  are explicit parameters. Physical register allocation and low-level
  instruction scheduling are ptxas's for everyone, Triton included; current
  hand-blocking runs zero spills (`docs/codegen-verdict.md`), which is why the
  virtual register-pressure allocator (`habu-ptx-register-pressure-ed521b40`)
  is parked measure-first.
- **Grid — manual in both.** Habu's machinery: per-shape winner rows and the
  grouped-raster CTA ordering knob (round 10, the 4096³ L2-locality lever).

The structural difference is not quality — the round-14 standings table in
`docs/eval-triton.md` puts every shape between 0.76× and 0.95× of Triton — it
is **generality**:
Triton derives the first three rows automatically for any program in its
language; Habu derives them once per kernel family, which cannot scale to
arbitrary `SPEC:` dataflow (`habu-codegen-deficiencies-no-a79f059a`, the
Joel-confirmed "no native optimizer" finding).

## What Triton's automation mechanically is

A tile-level IR plus roughly three passes:

1. **Layout assignment** — each tile value gets a layout (which lane/register
   holds which element); the pass picks layouts where consecutive lanes touch
   consecutive addresses. That is all coalescing is.
2. **Staging materialization** — layout mismatches (global-blocked feeding a
   tensor-core op) materialize as shared-memory staging; a swizzle formula
   handles bank conflicts; a lifetime allocator packs the scratchpad.
3. **Loop pipelining** — given a stage count, rotate the main loop N deep,
   multi-buffer the staged tiles, interleave copy issue with compute.

The grid stays the user's. Layouts as types, plus three rewrites.

## How Habu automates the same (the settled design)

Full trail in dot `habu-ptx-opt-layer-325b9507`; summary:

- **Layer 1 — a typed tile IR**, the missing layer between `SPEC:` equations
  and PTX: load-tile / dot / elementwise / reduce / store-tile /
  convert-layout, with types carrying extents (the landed extent-role
  machinery), dtype, and layout. The checker *types* layouts — "dot requires
  fragment-layout operands" is a fail-closed typing rule, not a compiler
  convention. Legality by construction, house style.
- **Layer 2 — passes extracted from measured hand-emitter tricks**: layout
  assignment generalizes the epilogue's coalescing derivation; staging
  generalizes the pad-8/XSWIZ formulas (both measured per-shape winners);
  pipelining generalizes the two-stage double buffer (and carries the measured
  GB10 negative on deeper stages). Nothing is invented; the lab notebook
  becomes passes.
- **Proof strategy — the hand emitters are the oracle.** Each pass lands only
  when the IR path reproduces the current winners (element-exact via
  `tools/ptx/mma-exact-lib.f` / `mma-gemm-check.f`, byte-identical via
  `mma-emit-diff.f` where possible), and the round-14 standings are the
  non-regression floor. The SASS/roofline verdict tooling compares
  pass-generated against hand kernels.
- **Layer 3 — the existing shape-keyed autotuner searches pass knobs**
  (tile sizes, warps, stages, swizzle) exactly as it searches hand-config knobs
  today (`habu-feed-mma-config-d783e33b` owns the wiring).
- **Acceptance — the first no-hand-emitter consumer**: flash-tiled batched
  attention (`habu-gb10-batched-attention-3055d565`, BTC-6), the fusion no
  per-family emitter can reasonably ship, whose equations already exist in the
  grammar with batch extents.

## The structural advantage to preserve

Habu's source language is declarative equations with **derived adjoints**
(`SPEC:` derivation, gradchecked), so backward kernels lower through the same
passes for free. Triton users hand-write their backward kernels. When the tile
IR lands, that asymmetry is the moat: one authored equation yields forward and
backward device kernels from the same measured pass pipeline.
