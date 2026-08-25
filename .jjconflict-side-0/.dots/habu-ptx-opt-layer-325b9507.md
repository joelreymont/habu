---
title: "PTX opt layer: target-independent and native-first"
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T14:15:04.496599+02:00"
blocks:
  - habu-remove-generated-ptx-f949cf0f
---

Depth review: NO native optimizer exists (zero peephole/fold/CSE/DCE in src/habu; only bootstrap/cg/opt.fs, 240 lines gforth-side). When the PTX typed IR lands (docs/tma-gather.md campaign), build the opt layer target-independent + native-first or we get a third per-dialect optimizer. Also: do not copy the label-VARIABLE global-state emitter pattern (habu1/habu2/jit) into the PTX emitter — the emit.f line-sink shape is the better precedent.

Compiler-IR reconciliation: this dot owns the closed `GPU-KIR`/`GPU-GIR`
schema and stable structured pass framework used by GPU Waves B-D. It does not
own PTXIR2, RIR fusion output, physical register assignment, artifact promotion,
or the Wave E tuner cutover. Its first bounded outcome is the elementwise
load/compute/store schema, flat schedule records, and exact structural
canonicalization needed by the Wave B children. Existing tile/layout/staging
and pipeline ideas remain retained outcomes only when their owning wave reaches
them and their measured baseline supports them.

2026-07-20 SETTLED DESIGN (orchestrator, answering Joel's "can we automate what
triton automates? how?"): Triton's automation = a tile-level IR + three passes;
ours builds the same shape from measured parts.

LAYER 1 - typed tile IR (the missing layer between SPEC: equations and PTX):
ops load-tile / dot / elementwise / reduce / store-tile / convert-layout; types
carry extents (extent-role machinery, landed b192992e/2b6ad8f8), dtype, and
LAYOUT (lane/register distribution, or shared+swizzle). The checker TYPES
layouts: "dot requires fragment-layout operands", "store requires a coalescible
layout" become fail-closed typing rules - legality by construction, house style.

LAYER 2 - passes, each extracted from a measured hand-emitter trick:
(1) layout assignment/propagation = coalescing (the epilogue's 32-lanes-to-32-
    contiguous-columns derivation generalized; cost model: minimize converts,
    maximize vector width);
(2) staging materialization + shared allocation (convert-layout global->fragment
    materializes as smem staging; the pad=8 and MMA-XSWIZ swizzle FORMULAS
    generalized - both measured per-shape winners; lifetime allocator with the
    E-MMA-SMEM budget check generalized);
(3) loop pipelining (MMA-STAGES generalized: rotate K-loop N deep, N-buffer
    staged tiles, hoist cp.async issue - knowing stages>=3 measured dead on GB10
    under half-smem residency, twice).
Grid stays manual/autotuned (winner rows), as in Triton.

PROOF STRATEGY (the house bar): the hand emitters are the ORACLE - each pass
lands only when the IR path reproduces the current winners (element-exact via
MMA-EXACT/mma-gemm-check; byte-identical via mma-emit-diff where possible), and
the GB10 standings 0.76/0.92/0.95/0.91x are the non-regression floor. SASS/
roofline verdict tooling compares pass-generated vs hand kernels.

LAYER 3 - the existing shape-keyed autotuner searches PASS knobs (BM/BN/BK/
warps/stages/swizzle) instead of hand-config knobs (the feed-mma-config dot's
wiring, currently fenced on the Mac's lower-mm claim).

FIRST NOVEL CONSUMER (the generality payoff and acceptance): flash-tiled
batched attention (BTC-6, habu-gb10-batched-attention-3055d565) lowering through
the passes with NO hand emitter - the fusion no per-family emitter can ship.
STRUCTURAL ADVANTAGE to preserve: SPEC: sources are declarative with DERIVED
adjoints, so backward kernels come from the same lowering free - Triton users
hand-write their backwards.

Sequencing unchanged: register-pressure allocator (ed521b40) stays parked
measure-first; this dot is the load-bearing remedy for deficiencies (1)+(3) of
habu-codegen-deficiencies-no-a79f059a.
