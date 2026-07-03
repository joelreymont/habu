# Habu Model CAD — Implementation Plan

**Status:** reviewed plan (2026-07-04), supersedes the imported HabuCAD draft.
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
  tools (`tools/ptx/`). New dots: `cad-0-report`, `cad-0-words`.

### Phase 1 — Model IR and shape/layout facts

- Node table: op kind, operand indices, shape, dtype, layout, attributes,
  materialization requirements, autograd metadata. Serializable.
- Shape keys and layout keys; region extraction for fusion candidates;
  fail-closed unsupported-op diagnostics.
- Initial op set: add, mul, scale, bias, relu, gelu(+approx), layernorm,
  rmsnorm, softmax-row, matmul, linear, residual-add, cast.
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
epilogue, save/recompute policy, measurement history. Candidates printed
before emission; all candidates recorded; replayable by key; winner cached
per shape/dtype/layout/target.

- Families: elementwise-v1, row-reduction-v1, softmax-row-v1, gemm-tf32-v1.
- Exists: dot `habu-ptx-m9-bench` (bench + autotuner). New dot:
  `cad-4-schedule` (the object + cache key; depends on derive dot for keys
  or hand-written compare until it lands).

### Phase 5 — Correctness gates

`CERTIFY` (static legality, no GPU), `GOLDEN` (device vs reference),
`GRADCHECK` (numerical derivative), optional determinism check, explicit
tolerance policy stored with artifacts. `PROMOTE` refuses on failed gates;
failures identify op/region/candidate.

- Exists: checker-as-judge eval (`maki/eval.f`), device golden runs,
  device gradcheck (SOFTMAX-ROWS-BWD), dot
  `habu-committed-device-correctness`. Gate wiring lands inside
  `cad-0-words`/`cad-7-optimize`; no separate new dot.

### Phase 6 — Profiling and roofline report

Per-kernel profile row: device time, bytes/FLOPs estimates, GB/s, GFLOP/s,
arithmetic intensity, roofline class, limiting-resource guess, comparison to
cached baseline, next-move recommendation.

- Exists: `tools/ptx/` launch/profile path, bench harness, measured Orin
  numbers in `docs/eval-triton.md`. Folded into `cad-4-schedule` +
  `cad-7-optimize`; regression detection needs the artifact cache.

### Phase 7 — One-REPL integration

`OPTIMIZE` composes lower → fuse → memory → tile → certify → golden →
gradcheck → profile → promote, with `EXPLAIN` producing failure packets
(failure class, location, expected/actual contract, suggested repair family,
minimal repro — same shape as the eval-repair packets). Every command has a
structured output mode an agent can parse.

- New dot: `cad-7-optimize`. Related: `habu-kernel-artifact-export`
  (artifact/cache), eval-repair packets (`maki/eval-repair.f`).

### Phase 8 — Tensor-core backend

One hard-coded TF32 GEMM path first (sm_87, f32 in, tf32 MMA, f32 accum,
64x64/64x128 CTA tile, 4 warps), then epilogues, double buffering, and
shape-keyed search. Fragment/smem/warp/stage/barrier tokens as checked types.

- Exists: dots `habu-tensor-core-mma`, `habu-re-express-tiled`,
  `habu-checker-capability-typed` (kernel loops/smem/accumulators),
  `lib/ptx/tile-smem.f`, `lib/ptx/tile-acc.f`. No new dot; this plan adds
  the schedule-object and gate integration requirements to those dots.

### Phase 9 — Autograd and backward fusion

Backward regions participate in the same fusion/memory/tiling/gate loop;
save-vs-recompute is a reported decision; gradcheck gates promotion.

- Exists: `lib/ptx/ad.f` (VJP table, reverse pass, save-vs-recompute cost
  model), device gradcheck, epic `habu-epic-maki-autograd` and its dot
  chain. No new dot.

### Phase 10 — Agent loop

Agents propose model variants, fusion rewrites, layout/schedule candidates,
approximation choices. Agents cannot bypass CERTIFY/GOLDEN/GRADCHECK/
PROFILE/PROMOTE. Failures return repair packets. Bench: proposals rejected
before GPU, golden/gradcheck pass rates, best speedup, cost per accepted
improvement.

- Exists: eval harness + pass@k (`maki/eval.f`), repair packets
  (`maki/eval-repair.f`), dot `habu-eval-matrix-live`. New dot deferred
  until Phase 7 lands (the loop needs `OPTIMIZE` reports to judge).

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
2. `cad-0-report` — report schema v1.
3. `cad-0-words` — REPL command skeleton, conservative reports.
4. `cad-1-ir` — model IR with shape/layout facts.
5. `cad-2-fusion-regions` — elementwise region discovery + traffic estimate.
6. `cad-3-memory-report` — coalescing report.
7. `cad-4-schedule` — schedule object + cache key.
8. Elementwise autotuning (existing bench dot, schedule-keyed).
9. Softmax/reduction fusion boundaries (existing fusion dots over cad IR).
10. Golden/gradcheck promotion gates in `cad-7-optimize`.
11. Profile/roofline rows in `cad-7-optimize`.
12. `cad-demo-ffn` — end-to-end FFN demo, no tensor-core parity claim.
13. Tensor-core path (existing MMA/typed-kernel dots).
14. Backward fusion + save/recompute reporting (existing AD dots).
15. `cad-adt-swap` — typed backbone lands as TFAM phases land.
16. Agent proposal loop over `OPTIMIZE` reports.
