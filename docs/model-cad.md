# Habu Model CAD — Implementation Plan

**Status:** reviewed plan (2026-07-04), supersedes the imported HabuCAD draft.
**Design:** `CAD-PLAN.md` (planners, cost model, schedules, gates — the *how*).
**Positioning:** `docs/positioning.md`. **Type system:** `docs/type-families.md`
(TFAM campaign, in flight on `maki-type-families`). **Framework substrate:**
`PLAN.md` (Maki + Habu-PTX), `maki/STATUS.md`.

## Mission

Turn Habu into a live model-CAD environment for GPUs: one REPL where a model
edit updates the fusion plan, memory/coalescing plan, tiling schedule,
validation results, profile report, and selected implementation.

The user benefit, stated as mechanism: more model iterations with less
hand-written kernel work, because fusion/layout/schedule decisions are
generated, gated, and measured instead of hand-maintained.

## Driving workload

The first real model this loop must serve is a fixed-prompt, specialized,
PBD-style detector derived from NVIDIA LocateAnything
(`huggingface.co/nvidia/LocateAnything-3B`), running on a Jetson Orin NX
across multiple camera streams. The application layer around it is a
separate project; habu/maki owns the capabilities the port demands:

- Data-movement ops in the model IR and ONNX lowering (reshape, transpose,
  slice, concat, gather) — the port's kernel order starts there, and today
  they fail closed.
- RMSNorm and RoPE checked kernels (row-reduction and pointwise-pair
  families), then attention/KV-cache regions on the existing fused-attention
  and MMA dot chain.
- `GOLDEN` against external reference artifacts (saved reference tensor
  dumps with recorded tolerances), not only the CPU reference.
- GEMM bring-up policy: a library/FFI call is an acceptable first GEMM for
  workload bring-up; the checked tensor-core path (Phase 8) is the research
  lever, not the bring-up gate.

That workload also validates the loop's design: a real port needs an
operator/tensor ledger — shape, dtype, strides, occurrences, device time,
bytes moved, FLOPs, arithmetic intensity, support status, candidate fusion,
golden artifact, tolerance — which is exactly the Phase 0 report schema plus
Phase 1 IR facts.

The second driving workload is training, not porting: a small temporal
model developed and trained **from scratch** in maki for the estimator side
of the same system — measurement-uncertainty prediction (predicted
variance/covariance trained with negative-log-likelihood losses),
association support, and an optional range prior over detection and IMU
feature streams. For maki this exercises the whole loop as a training tool:
`MODEL:` definition, generated and gradchecked backward, GPU training step,
loss/optimizer vocabulary, a committed convergence gate, and a profiled
training step — not only inference optimization. Dots: `habu-maki-gaussian-nll`
(loss family), `habu-maki-from-scratch` (end-to-end from-scratch training
demo); they ride the existing training chain (`habu-maki-training-loop`,
`habu-epic-maki-autograd`, `habu-autograd-tensor-batched`).

## Non-goals

- A GPU-resident Forth VM.
- A prettier Triton syntax layer.
- Host-language glue around Habu (forbidden by `CLAUDE.md` Habu-only rule).
- A benchmark-only demo with no REPL loop.
- A fusion pass that blindly creates one huge kernel.
- A tensor-core backend without correctness and profile gates.
- A README that leads with checker internals (see `docs/positioning.md`).

## Architecture

Keep the three existing layers and the one-way dependency:

```text
Habu core   self-hosted checked Forth, native engine, JIT/AOT, trust root, gate
Habu-PTX    lib/ptx/: tile/span/matrix vocabulary, KERNEL:, PTX emission,
            CUDA driver launch, reverse-mode AD transform
Maki        maki/: tensors, autograd orchestration, optimizers, losses,
            training loop, ONNX import, eval harness

maki → habu; habu core never depends on maki.
```

The REPL is host-resident. GPU execution happens through generated kernels.
Device phases run on the Orin (sm_87); host phases run anywhere `bin/hb` runs.

## Typed backbone (type families)

Model CAD is the first large consumer of `docs/type-families.md`. Every plan,
verdict, and report artifact is an ADT; `MATCH` is the elimination form for
verdict dispatch and report rendering. Target vocabulary:

| Artifact | Family kind | Sketch |
| --- | --- | --- |
| Op kind | enum | `ENUM op-kind add mul bias gelu matmul ... END-ENUM` |
| Dtype/layout keys | enum + product | `dtype` enum exists as constants today; becomes enum family |
| Gate verdict | sum | `SUMTYPE verdict 1  VARIANT pass END  VARIANT fail a END` |
| Fusion decision | sum | `fused` / `split<reason>` with named split reasons |
| Lookup/IO results | sum | `option<a>`, `result<a,b>` from the stdlib families |
| Report rows | product | shape key, schedule, profile row, memory-plan row |
| Legality facts | evidence | `coalesced<t>`, `uniform<r>` — already the PTX pattern |
| Model IR node | product + indices | op-kind enum + operand indices into node table |

**Recursion rule.** The model IR is a DAG; recursive unboxed ADTs are a
type-families v1 non-goal. IR nodes therefore reference operands by typed
index into a node table (the `lib/ptx/ir.f` pattern), not by nested value.
The by-value recursive form arrives with the boxed layout policy (TFAM 16),
tracked by `habu-epic-adopt-adts` — "typed ptr + arena until then".

**Staging rule.** Phase 0–1 code must not block on TFAM. Reports and IR ship
first on existing checked records behind small constructor/accessor words
whose signatures do not leak representation; internals swap to
sum/enum/product families as TFAM lands (dot `cad-adt-swap`). No
result-specific staging helpers; no new `TRUSTED:`/`set-check` boundaries for
report or IR plumbing.

### TFAM dependency map

| Model CAD need | TFAM dot |
| --- | --- |
| `TYPEFAMILY` registry (replace parser whitelist) | tfam-2a, 4, 6 |
| Generated constructors without trust | tfam-8, 9 |
| Checked `MATCH` + lowering + bad-tag proof | tfam-9, 10 |
| Layout-aware `dup`/`drop`/locals over ADTs | tfam-12 |
| Enum families (op-kind, verdict tags) | tfam-14 |
| Product families (report rows, schedule) | tfam-15 |
| Boxed policy (recursive IR by value) | tfam-16 (prioritized by maki need) |
| Derived eq/order/hash (cache keys) | habu-checker-capability-derive |
| ADT adoption sweep across maki | habu-epic-adopt-adts |

### Type-system review verdict (2026-07-04)

The TFAM campaign as dotted covers Model CAD's stack-level needs. The two
levers that matter most, both already tracked: **boxed policy priority**
inside TFAM 16 (recursive IR, autograd tape, ONNX graph by value) and
**derive eq/order/hash** for shape/dtype/layout/target cache keys, which
Phase 4 needs the moment schedule caching lands — without it every key
comparison is hand-written (accepted until the derive dot lands, since it
depends on TFAM 15/16).

One gap was found and dotted
(`habu-checker-capability-typed-a480c423`): TFAM 16 ships the packed memory
ABI *descriptor* (`docs/type-families.md` §22.2), but no TFAM item
implements the consumer capability — checked buffer store/load for
layout-family values and a typed array-of-ADT container. Report tables,
schedule measurement history, and artifact-cache rows need it; until it
lands they stay parallel-column records per the staging rule.

If implementation uncovers further gaps, the rule from `CLAUDE.md` applies:
dot the missing capability; no local workarounds, no checker edits on this
branch (checker files are owned by the TFAM campaign).

## Product language rules

The README and all user-facing copy follow `docs/positioning.md`: honest and
technical, no aphorisms, claims are mechanisms or measurements. Internals
(stack effects, row polymorphism, mask tokens, PTX, cubins) are explained
only as what makes the automation safe. The Moore lineage is identity, not
evidence.

## North-star workflow

```forth
maki> MODEL: FFN ( x w1 b1 w2 b2 -- y )
        LINEAR GELU LINEAR ;

maki> OPTIMIZE FFN SHAPE batch=1 seq=128 dim=4096 TARGET sm_87
fusion:       LINEAR+BIAS+GELU fused
memory:       6 global passes removed; hot loads coalesced; 1 masked tail
schedule:     64x128x32, 4 warps, 2 stages
numerics:     golden pass
backward:     generated; gradcheck pass
profile:      memory-bound region at 92% measured roof
cache:        saved for shape/dtype/layout/target
```

Syntax may change; the required property is that the REPL owns the whole loop
and every line above is backed by a structured report field, not prose.

## Phases

Existing repo assets and dots are listed per phase; only the *new* dots
(prefix `cad-`) are minted by this plan.

### Phase 0 — Report schema and REPL words

Minimal commands and report objects, conservative implementations.

- Words: `MODEL:` `LOWER` `FUSE` `MEMORY` `TILE` `CERTIFY` `GOLDEN`
  `GRADCHECK` `PROFILE` `TUNE` `PROMOTE` `EXPLAIN` `OPTIMIZE`.
- Report schema (machine-readable, agent-consumable without scraping prose):
  model name; shape/dtype/layout keys; target; fusion plan; materialized
  tensors; estimated bytes before/after; coalescing status per hot tensor;
  schedule candidates + selection; gate verdicts; profile rows; roofline
  class; artifact/cache key; warnings and split reasons.
- Acceptance: a toy model block defines; every command returns a structured
  report; unsupported input fails closed with a named reason.
- Exists: eval-harness report discipline (`maki/eval.f`), PTX launch/profile
  tools (`tools/ptx/`). New dots: `cad-0a` (report schema), `cad-0b` (commands).

### Phase 1 — Model IR and shape/layout facts

- Node table: op kind, operand indices, shape, dtype, layout, attributes,
  materialization requirements, autograd metadata. Serializable.
- Shape keys and layout keys; region extraction for fusion candidates;
  fail-closed unsupported-op diagnostics.
- Initial op set: add, mul, scale, bias, relu, gelu(+approx), silu,
  layernorm, rmsnorm, softmax-row, matmul, linear, residual-add, cast, rope.
- Data-movement ops (driving-workload demand): reshape/view, transpose,
  slice, concat, gather — as IR layout facts first (transforms the planner
  reasons about), materializing kernels only where a copy is genuinely
  required (dot `habu-cad-la-data`).
- Exists: kernel-level IR (`lib/ptx/ir.f`), AD DAG (`lib/ptx/ad-dag.f`),
  ONNX op table (`maki/onnx.f` — fail-closed pattern to copy).
- New dot: `cad-1-ir`. Related: `habu-maki-lower-tensor`.

### Phase 2 — Mega-fusion planner

Chase fewer global-memory passes, not one kernel. Legality constraints
(materialization, layout, barriers, atomics, register pressure, backward
rule, tolerance, pinning) and a profitability model (bytes removed, launches
removed, extra FLOPs, occupancy risk). Every split has a named reason in the
report. Rollback on measured regression.

- Order: elementwise chains → matmul/linear epilogues → residual/norm →
  softmax-region with explicit reduction boundary → backward regions.
- Exists: `maki/fusion.f` (early), dots `habu-automatic-op-fusion`,
  `habu-automatic-aggressive-fusion`. New dot: `cad-2-fusion-regions`
  (region discovery + traffic estimate over the model IR; the codegen lever
  stays in the existing fusion dots).

### Phase 3 — Memory coalescing and layout planner

Track layout, stride, alignment, contiguity, vector width, lane mapping,
address space, broadcast, tail masks, bank mapping. `MEMORY` reports the
plan and the traffic delta; non-coalesced access is reported early.

- Acceptance: SAXPY-like kernels select v4 when legal; unaligned inputs fall
  back with a warning; deliberately strided access reports non-coalesced.
- Exists: v4 vectorization + tail masks (`lib/ptx/tile-v4.f`), shared-mem
  tiles (`lib/ptx/tile-smem.f`). New dot: `cad-3-memory-report`.

### Phase 4 — Schedule vocabulary and autotuner

Schedule = checked object: region, target, shape/dtype/layout keys, block
size, vector width, tile M/N/K, warps, stages, smem layout, fragment shape,
epilogue, measurement history. Candidates printed
before emission; all candidates recorded; replayable by key; winner cached
per shape/dtype/layout/target.

- Families: elementwise-v1, row-reduce-v1, softmax-row-v1, gemm-tf32-v1,
  decode-v1 (PBD-style chains — the driving workload's family; its ops and
  references arrive with the LA-port dots). The schedule object does not
  carry save/recompute policy — that is a fusion-plan field (CAD-PLAN §12).
- Exists: dot `habu-ptx-m9-bench` (bench harness; no autotuner exists yet).
  New dots: `cad-4-schedule` (the object + cache key; depends on derive dot
  for keys or hand-written compare until it lands), `cad-5-store` (the
  artifact store: on-disk layout + schema for kernels, evidence, measurement
  history, profitability facts, calibration tables), and `cad-6-tune`
  (candidate enumeration + measurement + selection built over
  `tools/ptx/bench.f`, every measurement recorded, winner cached by key).

### Phase 5 — Correctness gates

`CERTIFY` (static legality, no GPU), `GOLDEN` (device vs reference),
`GRADCHECK` (numerical derivative), optional determinism check, explicit
tolerance policy stored with artifacts. `PROMOTE` refuses on failed gates;
failures identify op/region/candidate. `GOLDEN` references are either the
CPU implementation or an external reference artifact (e.g. saved HF tensor
dumps for the driving workload) with the tolerance recorded per artifact.

- Exists: checker-as-judge eval (`maki/eval.f`), device golden runs,
  device gradcheck (SOFTMAX-ROWS-BWD), dot
  `habu-committed-device-correctness`. Gate wiring lands inside
  `cad-0b`/`cad-7-optimize`; `cad-7-optimize` also owns the two GOLDEN
  deliverables: the host model-IR reference executor (topo walk calling each
  op's scalar reference) and the external reference-artifact loader + format
  (tensor dump + per-artifact tolerance). PROFILE is mandatory to run but
  non-blocking for promotion (CAD-PLAN §11).

### Phase 6 — Profiling and roofline report

Per-kernel profile row: device time, bytes/FLOPs estimates, GB/s, GFLOP/s,
arithmetic intensity, roofline class, limiting-resource guess, comparison to
cached baseline, next-move recommendation.

- Exists: `tools/ptx/` launch/profile path, bench harness, measured Orin
  numbers in `docs/eval-triton.md`. Measurement/search glue is owned by
  `cad-6-tune`; report integration and regression detection by
  `cad-7-optimize` (regression detection needs the artifact cache).

### Phase 7 — One-REPL integration

`OPTIMIZE` composes lower → fuse → memory → tile → certify → golden →
gradcheck → profile → promote-decision (recorded, never thrown; standalone
`PROMOTE` throws), with `EXPLAIN` producing failure packets (failure class,
location, expected/actual contract, suggested repair family, minimal repro —
the `tools/repair-packet-core.f` packet discipline). Every command has a
structured output mode an agent can parse.

- New dot: `cad-7-optimize`. Related: `habu-kernel-artifact-export`
  (artifact/cache), repair packets (`tools/repair-packet-core.f`).

### Phase 8 — Tensor-core backend

One hard-coded TF32 GEMM path first (sm_87, f32 in, tf32 MMA, f32 accum,
64x64/64x128 CTA tile, 4 warps), then epilogues, double buffering, and
shape-keyed search. Fragment/smem/warp/stage/barrier tokens as checked types.

- Exists: dots `habu-tensor-core-mma`, `habu-re-express-tiled`,
  `habu-checker-capability-typed-e0c76a02` (kernel loops/smem/accumulators),
  `lib/ptx/tile-smem.f`, `lib/ptx/tile-acc.f`. No new dot; this plan adds
  the schedule-object and gate integration requirements to those dots.
- Bring-up policy (driving workload): a library/FFI GEMM is acceptable
  first so the workload's end-to-end path is not gated on this phase; the
  checked tensor-core path replaces it when it wins on profile.

### Phase 9 — Autograd and backward fusion

Backward regions participate in the same fusion/memory/tiling/gate loop;
save-vs-recompute is a reported decision; gradcheck gates promotion.

- Exists at the kernel level only: `lib/ptx/ad-dag.f` (kernel-IR reverse
  pass over the softmax-rows primitive set; `lib/ptx/ad.f` is its v0 token
  version; `VJP-SAVES` is a count, and the kernel-level `AD-RECOMPUTE?`
  comparator is not wired to the shared §9 cost model), device
  gradcheck, epic `habu-epic-maki-autograd` and its dot chain.
- New dot: `cad-9-backward` — the model-op adjoint registry plus the
  model-IR reverse transform emitting backward regions as IR nodes, and the
  save-vs-recompute decision under the shared cost model (CAD-PLAN §12).
  GRADCHECK (milestone 14) and the training flagship depend on it.

### Phase 9b — Training from scratch

Maki is a training tool, not only an inference optimizer: a model defined
with `MODEL:` must be trainable from random init through the same loop.

- Loss family beyond MSE/L1: Gaussian NLL with predicted log-variance,
  Mahalanobis, Huber — each with analytic gradient plus numeric gradcheck,
  tensor-scale apply, VJP registration, fail-closed on non-positive
  variance (`habu-maki-gaussian-nll`).
- The training step is a first-class region for the loop: forward + backward
  + optimizer step planned, fused where legal, and profiled as one unit;
  `PROFILE` reports the step, not only inference kernels.
- Convergence is a gate: seeded synthetic data and a loss threshold
  committed as a test, so training regressions fail like correctness
  regressions.
- Flagship: a small temporal model (windowed MLP/TCN over feature-sequence
  windows → prediction + log-variance) trained from scratch on GPU with the
  NLL loss (`habu-maki-from-scratch`).
- Exists: converging training loop at tensor scale (`maki/train.f`), GPU
  SGD demo (`maki/gpu-train.f`), optimizers incl. Adam
  (`maki/optim-tensor.f`), gradchecked autograd orchestration, and the
  batched-VJP epic chain (`habu-autograd-tensor-batched` onward).

### Phase 10 — Agent loop

Agents propose model variants, fusion rewrites, layout/schedule candidates,
approximation choices. Agents cannot bypass CERTIFY/GOLDEN/GRADCHECK or the
PROMOTE rules; PROFILE is mandatory to run, non-blocking (CAD-PLAN §11).
Failures return repair packets. Bench: proposals rejected before GPU,
golden/gradcheck pass rates, best speedup, cost per accepted improvement.

- Exists: eval harness + pass@k (`maki/eval.f`), repair packets
  (`tools/repair-packet-core.f`; `maki/eval-repair.f` is rounds/tokens
  accounting), dot `habu-eval-matrix-live`. New dot deferred until Phase 7
  lands (the loop needs `OPTIMIZE` reports to judge).

## Flagship demo

FFN block: linear → bias → GELU → linear → residual → norm. Demo output:
model defined in REPL; fusion plan; traffic before/after; coalescing report;
schedule candidates; golden pass; gradcheck pass (if backward included);
profile row; artifact promoted; comparison to unfused baseline. Success =
a model edit triggers automatic re-fusion, re-tiling, validation, profiling,
and artifact update from one REPL. The first demo win is fusion/traffic
reduction, not beating mature GEMM libraries.

- New dot: `cad-demo-ffn`. Related: `habu-small-model-end`.

## Benchmark strategy

The compute-bound plan (MMA emission, cp.async stages, persistent autotune,
fusion depth, PROMOTE-owned weight layout, launch amortization, gate-licensed
precision, roofline-directed search, and the honest finish line) is specced in
CAD-PLAN 8.1 with its owning dots (habu-tensor-core-mma, cad-6-tune,
habu-cad-weight-layout, habu-cad-launch-amortization, habu-cad-gate-licensed).

- Memory-bound fusion class: SAXPY variants, bias+activation, residual+norm,
  softmax pieces, dequant/requant chains — measure bytes removed, launches
  removed, GB/s, roof %, speedup vs unfused.
- Compute-bound class: GEMM, linear+epilogue, attention scores — measure
  roof class, GFLOP/s, epilogue-fusion benefit, shape-keyed schedule wins.
- Agent-loop class: proposals, rejections before GPU, gate pass rates, best
  speedup, cost per accepted improvement.

## Documentation deliverables

`README.md` (done), `docs/positioning.md` (done), this file, then per-phase:
`docs/fusion-planner.md`, `docs/memory-planner.md`, `docs/schedule-tuning.md`,
`docs/repl-loop.md`, `docs/tensor-core-backend.md`, `docs/agent-protocol.md`.

## Implementation rules

Every change: user benefit statement; affected REPL command; structured
report field added/changed; correctness gate added/updated; negative test if
legality changed; profile evidence if performance is claimed; artifact/cache
behavior if codegen changed. A codegen feature is done when it has a static
legality test, a device-golden test, a profile row, a failure diagnostic, and
docs if user-visible. All work is checked Habu per `docs/forth.md`; commit
gate per `CLAUDE.md`.

## Milestone order

1. README + positioning + this plan (done, this change).
2. `cad-0a` — report schema v1.
3. `cad-0b` — REPL command skeleton, conservative reports.
4. `habu-maki-gaussian-nll` — NLL/covariance loss family (independent;
   unblocks the training flagship).
5. `habu-maki-unified-single` — unified single-slot tensor value + planning
   vocabulary base (the §3 prerequisite; CAD-PLAN).
6. `cad-1` — model IR with shape/layout facts + the op registry (costs,
   numeric class, scalar references; membership gated on the reference
   existing — silu/rmsnorm/rope references are explicit sub-tasks).
7. `habu-cad-la-data` — data-movement ops as IR facts.
8. `cad-2` — elementwise region discovery + traffic estimate (traffic-only
   splits until the Phase-4/6 resource tables exist).
9. `cad-3` — coalescing report.
10. `cad-4` — schedule object + cache key.
11. `cad-5-store` — artifact store: on-disk layout + schema.
12. `cad-6-tune` — TUNE enumeration/measurement/selection over
    `tools/ptx/bench.f`; roof microbenches replace `profile.f` constants.
13. Softmax/reduction fusion boundaries (existing fusion dots over cad IR).
14. Golden gate + gradcheck gate wiring in `cad-7-optimize` (GRADCHECK
    enforcement inert until `cad-9-backward`, item 18), including the
    host model-IR reference executor and external reference artifacts.
15. Profile/roofline rows in `cad-7-optimize`.
16. `habu-ptx-kernels-rmsnorm` — RMSNorm + RoPE checked kernels.
17. `cad-demo-ffn` — end-to-end FFN demo, no tensor-core parity claim.
18. `cad-9-backward` — model-op adjoint registry + model-IR reverse
    transform + save/recompute decision (CAD-PLAN §12).
19. `habu-maki-from-scratch` — from-scratch temporal model trained on GPU
    (training flagship, Phase 9b; depends on 18).
20. Tensor-core path (existing MMA/typed-kernel dots); library-FFI GEMM
    acceptable for workload bring-up meanwhile.
21. Backward fusion + save/recompute reporting (over 18) and the fused,
    profiled training step.
22. `cad-adt-swap` — typed backbone lands as TFAM phases land.
23. Agent proposal loop over `OPTIMIZE` reports.
