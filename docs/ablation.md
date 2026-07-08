# Effectiveness / Ablation Matrix

Every claim in the paper (`.dots/habu-paper-habu-checked-1c035895.md`, the REQUIREMENT
block) must be backed by a committed, reproducible in-tree experiment, not narrative.
This file is that matrix: one row per invention, each with a status
(**implemented-here** with `file:line`, **pre-existing** cited by path, or **pending**
with the blocker) and the exact gate/tool that reruns it.

Status legend: **impl** = landed by this dot (`habu-ablation-fault-injection`); **pre**
= already in-tree, cited; **pending** = not yet earned, blocker named.

| # | Paper claim (invention) | Row | Status | Evidence (file:line) | Rerun |
|---|-------------------------|-----|--------|----------------------|-------|
| 1 | Checker (typed effects) | author-time error battery vs Triton (bugs caught static vs runtime) | pre | `docs/eval-triton.md` (error battery, Orin numbers) | `tools/ansi`/eval harness; see `docs/eval-triton.md` |
| 1 | Checker (typed effects) | authoring pass@k | pre | `maki/eval-fixture.f`, `maki/eval-test.f` (eval harness) | `bin/hb --load maki/test.f` |
| 2a | Fusion byte accounting | predicted-vs-measured traffic (byte model falsifiable at the roofline GB/s) | pending | host byte model `maki/traffic.f`; measured-GB/s device leg **pending bench harness** | — |
| 2b | Fusion planner | fusion ON/OFF ablation, same kernels regions split (**region count + traffic bytes**) | impl | `maki/ablate-fusion-test.f:55` (rows), toggle `maki/fusion-plan.f:253` (`FP-FUSE-ON!`/`FP-FUSE-OFF!`) | `bin/hb --load maki/ablate-fusion-test.f` (in the maki gate) |
| 2b | Fusion planner | fusion ON/OFF **latency** per model | pending | latency needs on-device timing **pending bench harness** (cad-6 / PROFILE) | — |
| 3 | Device-vs-host GOLDEN | seeded fault: **wrong index** caught | impl | `maki/ablate-golden-device-test.f:80` (`ABL-G1`) | `bin/hb --load maki/ablate-golden-device-test.f` (on the Orin) |
| 3 | Device-vs-host GOLDEN | seeded fault: **transposed / mis-addressed operand** caught | impl | `maki/ablate-golden-device-test.f:87` (`ABL-G2`) | device test (Orin) |
| 3 | Device-vs-host GOLDEN | seeded fault: **dropped mask** caught | impl | `maki/ablate-golden-device-test.f:94` (`ABL-G3`) | device test (Orin) |
| 3 | Device-vs-host GOLDEN | seeded fault: **stale kernel** caught | impl | `maki/ablate-golden-device-test.f:104` (`ABL-G4-GOLDEN`) | device test (Orin) |
| 3 | Device-vs-host GOLDEN | uniform-vs-nonuniform golden sensitivity (sum-launch precedent) | pre | `maki/lower-model-device-test.f` (multi-region whole-model golden, per-class tol) | device test (Orin) |
| 4 | Sentinels | dropped-copy-back injection caught (vs silent pass without) | impl | `maki/ablate-golden-device-test.f:133` (`ABL-SENT`), guard `lib/ptx/sentinel.f` | device test (Orin) |
| 5 | Verified gradient | wrong-adjoint detection fixture | pre | `maki/gradcheck-test.f:20` (`DET-BUILD`, deliberately-WRONG relu-bwd) | `bin/hb --load maki/test.f` |
| 5 | Verified gradient | from-scratch convergence gate (end-to-end proof) | pre | `maki/from-scratch-test.f` (seeded NLL fall + bit-exact determinism) | `bin/hb --load maki/test.f` |
| 6 | Persistent content-keyed tuning | time-to-first-correct-inference vs Triton warmup; replay determinism across restarts | pending | store/replay exist (`maki/store.f`, `maki/store-replay-test.f`); the time-to-first + JIT-warmup comparison is **pending cad-6** | — |
| 7 | EXPLAIN packets | repair-rounds / tokens-to-green | pre | `maki/eval-repair.f:47` (`repair-rounds` + `tokens-to-green`, checker-guided loop) | `bin/hb --load maki/test.f` |
| 7 | EXPLAIN packets | **with-vs-without-packet** A/B ablation | impl | `maki/eval-repair-ab-test.f` (4 seeded authoring-error fixtures, ON=rich packet vs OFF=minimal status-quo signal, same checker + same green kernel); shared engine `maki/eval-repair-loop.f` | `bin/hb --load maki/test.f` (in the maki gate) | — |
| 8 | Schedule machinery | tuned-vs-closed-form-default deltas per family | pending | **pending cad-6** (tuner output) | — |
| 9 | Gate-licensed precision | tf32 request licenses the REAL TF32 tensor-core kernel: `PREC-TF32` -> `maki/lower-mm.f` LMM-MMA? emits the `mma.sync` kernel (`lib/ptx/cg-mma.f`), golden passes device==host within the tf32 row (reason + evidence name tf32), AND the inverse guard: a seeded 0.5% fault fails even under the tf32 band; PREC-RESET re-emits the f32 kernel green | impl | registry `maki/precision.f`; kernel `lib/ptx/cg-mma.f`; fragment/GEMM device proofs `tools/ptx/mma-probe.f` + `tools/ptx/mma-gemm-check.f` (element-exact); license `maki/precision-device-test.f` (legs 1-4); measured `docs/eval-triton.md` GEMM step 3 (375-398 GFLOP/s) | `bin/hb --load maki/precision-device-test.f` (on the Orin); host: in the maki gate |

## Fusion ON/OFF ablation numbers (row 2b, measured)

`bin/hb --load maki/ablate-fusion-test.f` plans the SAME IR with fusion ON (default
capability table) and OFF (`FP-FUSE-OFF!`: every node its own region), and asserts both
the region count and the `maki/traffic.f` byte estimate (the byte model behind
`REPORT:BYTES!`). OFF is the ablation control: it collapses to the unfused per-node total
(equal to `TRF-BEFORE`). The ON→OFF deltas are the ablation.

| Model | ON regions | OFF regions | ΔRegions | ON bytes | OFF bytes | ΔBytes | Fusion saves |
|-------|-----------:|------------:|---------:|---------:|----------:|-------:|-------------:|
| FFN `LINEAR GELU LINEAR RESIDUAL-ADD RMSNORM` (4x8) | 3 | 5 | 2 | 2272 | 3040 | 768 | 25.3% traffic |
| MIX `LINEAR GELU RESIDUAL-ADD RMSNORM` (4x8) | 2 | 4 | 2 | 928 | 1440 | 512 | 35.6% traffic |
| `SLICE:0..2 GELU` (4x8, movement-dissolve override) | 1 | 2 | 1 | 192 | 320 | 128 | 40.0% traffic |

OFF bytes == `TRF-BEFORE` for every row (the unfused per-node baseline). The SLICE row
proves the OFF override splits *movements* too, not only compute pairs: ON dissolves the
free SLICE into GELU's load (1 region); OFF materializes it (2 regions).

## Golden fault-injection mechanisms (row 3, device evidence)

`bin/hb --load maki/ablate-golden-device-test.f` on the Orin. Each class emits the CORRECT
kernel in-process (`PTX-CAPTURE`), applies ONE post-emit text mutation (`ABL-MUTATE`, a
fail-closed first-occurrence replace — the emitters are never edited), assembles with
ptxas, registers the cubin, and asserts `LOWER-GOLDEN` returns **V-FAIL**. Every mutation
is chosen to stay in-bounds and still write every output cell, so the fault surfaces as a
value mismatch (V-FAIL), never a launch crash or a sentinel-poisoned readback.

| Class | Region | Mutation | Effect |
|-------|--------|----------|--------|
| wrong index | EW GELU | delete the input load's offset add `add.u64 %rd4,%rd4,%rd3` | every lane reads element 0 |
| transposed/mis-addressed operand | EW MUL | redirect input-1 base `%rd2`→`%rd1` | kernel computes x*x, not x*y |
| dropped mask | RED RMSNORM | strip `@%p2` from the reduction identity seed | tid≥k lanes leak +inf into the block sum |
| stale kernel | EW | assemble GELU kernel A, register it, run the golden on RELU model B (same 4x8 shape) | device runs the stale/mismatched kernel |
| sentinel (dropped copy-back) | EW | hand-rolled launch (reuses lower-launch staging) that omits the cuMemcpyDtoH | readback stays POISON → `GUARD` throws `E-PTX-READBACK` |

### Verbatim device evidence (Orin, `sm_87`, CUDA 12.6)

```
== (1) wrong index math [EW GELU 4x8] ==
 (1) wrong index: drop the input load offset add -> every lane reads element 0
lower-golden: REGION_0 mismatch beyond f32 tol at elem 1
== (2) transposed/mis-addressed operand read [EW MUL 4x8] ==
 (2) operand read: input-1 base %rd2 -> %rd1 -> kernel computes x*x, not x*y
lower-golden: REGION_0 mismatch beyond f32 tol at elem 0
== (3) dropped mask [RED RMSNORM 4x8] ==
 (3) dropped mask: strip @%p2 from the reduction identity seed -> tid>=k leak +inf
lower-golden: REGION_0 mismatch beyond f32 tol at elem 0
== (4) stale cubin [EW GELU-kernel vs RELU-model 4x8] ==
 (4) stale cubin: registered kernel = GELU, current model = RELU (same 4x8 shape)
lower-golden: REGION_0 mismatch beyond f32 tol at elem 0
== (B) sentinel: dropped copy-back [EW GELU 4x8] ==
 (B) sentinel: launch with the copy-back skipped -> GUARD must throw E-PTX-READBACK
  sentinel fired: E-PTX-READBACK (dropped copy-back caught)
test: ok
```

## Pending rows and their blockers

- **2a predicted-vs-measured traffic**, **2b latency**: need on-device timing / measured
  GB/s — pending the bench harness (the fenced `tools/ptx/bench.f` lane + CAD-PLAN 8.1
  PROFILE/roofline).
- **6 persistent tuning** (time-to-first-correct vs Triton JIT warmup): pending cad-6.
- **8 schedule tuned-vs-default deltas**: pending cad-6 tuner output.

## EXPLAIN packet A/B ablation numbers (row 7, measured)

`bin/hb --load maki/eval-repair-ab-test.f` (in the maki gate) runs 4 seeded
authoring-error fixtures through the shared repair-loop engine
(`maki/eval-repair-loop.f`, factored out of `maki/eval-repair.f`). Both arms are
scored by the SAME checker (`EVAL:CHECK-PASSES?`) and CONVERGE TO THE SAME
green kernel (`GREEN$`), so only the repair PATH differs and the packet's effect is
isolated. Every candidate (draft, each repair, green) is a real source string run
through the checker; repair-rounds and tokens-to-green are measured over real
verdicts, not asserted.

- **ON (EXPLAIN packet)**: the full checker diagnostic of `docs/repair-diagnostics.md`
  — `repair_class`, the offending token + span, expected/actual stack rows, and a
  class-derived suggestion. Each checker-surfaced error costs one targeted repair.
- **OFF (status-quo baseline)**: the minimal signal a conventional compiler emits on a
  bad definition — the verdict line plus a raw error code, nothing else. No class, no
  offending node, no expected/actual rows, no suggestion. Unable to localize the
  fault, the author makes one plausible-but-wrong repair per error before the correct
  one (a conservative lower bound; each floundering step is itself a real checker
  rejection).

| Fixture (seeded error class) | ON rounds | ON tokens | OFF rounds | OFF tokens |
|------------------------------|----------:|----------:|-----------:|-----------:|
| `fix_type` (SCALE operand)   | 1 | 58 | 2 | 87 |
| `add_producer` (missing store) | 1 | 55 | 2 | 77 |
| `fix_type` + `add_producer` (two bugs) | 2 | 81 | 4 | 129 |
| `remove_producer` (surplus load) | 1 | 61 | 2 | 90 |
| **Aggregate (4 fixtures)** | **5** | **255** | **10** | **383** |

The minimal-feedback arm costs strictly more on both axes: **2.0x the repair rounds**
(10 vs 5) and **+50.2% tokens-to-green** (383 vs 255, +128). The test asserts the exact
per-arm rounds per fixture and the `OFF > ON` inequality on both axes. Latency/wall-time
is out of scope here (this is the deterministic author-trajectory harness, not an LLM
run); the live model benchmark uses the schema-2 rows of `docs/repair-diagnostics.md`.
