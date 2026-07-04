# CAD-PLAN — Habu Model CAD Design

**Status:** design, 2026-07-04. This is the *how*; the campaign (phases, dots,
milestones) is `docs/model-cad.md`. Substrate: `PLAN.md` (Maki + Habu-PTX),
`docs/type-families.md` (ADTs), `docs/ptx.md` / `docs/ptx-sketch.md` (kernel
DSL), `lib/ptx/` (tile/collective/AD vocabulary), `maki/` (framework).

## 1. Stance

The user designs models. The compiler owns kernel performance.

Habu/maki must do the heavy work itself — fusion boundaries, memory layout,
coalescing, vector widths, tail handling, shared-memory staging, tile sizes,
warp counts, pipeline stages, epilogues, backward paths, tuning — and the user
supplies only the model, the shapes, and the target. Every knob has a derived
default; a user-set knob is an override that the reports must display.

Five design rules govern everything below:

1. **Facts → plans → code → evidence.** Planners derive typed plans from
   typed facts. Codegen executes plans mechanically. Gates verify. The tuner
   replaces estimates with measurements. No stage guesses, and no stage
   re-derives another stage's decision.
2. **Classes, not op names.** Planners reason over op *classes* (elementwise,
   row-reduction, matmul, movement, decode). A new op is a registry entry —
   class, cost, VJP, reference — and every planner handles it with no planner
   edits.
3. **One cost model.** The same bytes/FLOPs/occupancy model produces fusion
   profitability, schedule legality, roofline classification, and next-move
   advice. Estimates are calibrated against `ptxas -v` output and device
   measurement, persistently — estimates never rot.
4. **Measured, not assumed.** Device roofs come from microbenchmarks per
   target, cached. Fusion wins are confirmed by profile or rolled back.
   No hardcoded peak numbers, no "should be faster".
5. **Fail closed, explain always.** Anything the system cannot fuse, coalesce,
   or tile optimally is reported with the blocking fact in one line. Unknown
   ops, shapes, or layouts reject with a named reason.

## 2. Pipeline

```text
MODEL: definition          concatenative composition of model ops
  → model IR               typed node table: op, shape, dtype, layout facts
  → region graph           fusion plan: regions, materialization points, splits
  → layout plan            per-tensor: layout, vector width, tails, staging
  → schedule               per-region: family + parameters (typed template)
  → kernel words           existing lib/ptx tile/collective vocabulary, checked
  → PTX → cubin → launch   ptxas per target, CUDA driver FFI
  → evidence               certify/golden/gradcheck/profile rows on the artifact
```

Each arrow produces an inspectable artifact with a REPL word (`LOWER`, `FUSE`,
`MEMORY`, `TILE`, `CERTIFY`, `GOLDEN`, `GRADCHECK`, `PROFILE`) and a
machine-readable report row (the `maki/report.f` schema, built by `cad-0a`).
`OPTIMIZE` runs the chain and *records* the promotion decision in its report
(it never throws); standalone `PROMOTE` refuses with a named throw unless the
required gates pass. `EXPLAIN` renders any failure as a repair packet.

**Execution environments.** The stages split across three environments, and
the split is part of the design: planning, static checking, plan/code
coherence, and host golden references run anywhere `bin/hb` runs (including a
CUDA-less macOS host); PTX assembly and resource capture require the CUDA
toolkit (`ptxas`, resolved via `lib/ptx/toolchain.f`); device golden,
profiling, tuning, and GPU training require the Orin. Native `bin/hb` plus
CUDA toolkit plus a reachable device on the Orin is an explicit precondition
before the first device milestone (tuning), and every report row states which
environment produced it.

## 3. Model capture: plan mode

`MODEL:` does not introduce a graph syntax. A model word is ordinary checked
Forth executed against a *planning vocabulary*: the same `LINEAR GELU LINEAR`
source text runs with tensor *descriptors* on the stack instead of tensors,
and each model op appends an IR node instead of computing.

This requires one prerequisite the current code does not have: a **unified
single-slot tensor value**. Today's eager maki ops pass a tensor as several
stack cells (`maki/linear.f`: `LINEAR ( ptr a ptr a ptr a ptr a n n n -- )`;
shape, dtype, and data travel separately), so they cannot be re-typed onto
descriptors as-is. Phase 1 therefore introduces a tensor value type (a value
struct — authorized compiler work if the checker needs it) carrying
data/shape/dtype/layout in one stack slot, and the model-op vocabulary is
defined over it. Dispatch is lexical, not dynamic: `MODEL:` capture opens the
planning package, so the ops that compile into a model body are the
descriptor-typed planning words; the eager tensor path keeps its own words
and migrates onto the tensor value module-by-module. The model *source text*
is mode-independent; the two vocabularies are kept in sync by the op
registry (§4.2), not by textual duplication.

The tensor value is owned by its own dot (`habu-maki-unified-single`), a
Phase-1 predecessor. Until it lands, Phase 0's `MODEL:` (as built by
`cad-0b`) parses op *tokens* against a fail-closed table — the model body is
not yet composed of checked words. The §3 checked capture replaces that
parser in `cad-1` when the tensor value and planning vocabulary exist.

Consequences:

- The checker checks model words as ordinary words over descriptor types —
  arity and kind discipline at author time, before any planning.
- Factoring works: a model block is a word; blocks compose into models by
  concatenation, exactly like the rest of Habu.

Nodes are content-hashed (op, attributes, input hashes, shape/dtype/layout
keys) at append time; the hash is the unit of incremental replanning (§13).

## 4. Facts

### 4.1 Tensor descriptor

```text
shape        dims + extents
dtype        f32 f16 bf16 u32 i32 (u8 for decode/quant paths)
layout       dimension order + strides + contiguity flags
alignment    base alignment class (16B / 8B / 4B) + row pitch
addr-space   global | shared | register | param
producer     node id (or input slot)
consumers    count + node ids
liveness     single-use | multi-use | external (model output)
broadcast    none | scalar | per-row | per-column
tail         extent mod vector-width facts per dimension
```

Alignment and element width are *recorded* facts, never assumptions. Today's
host maki arrays are contiguous float-**cell** buffers (8-byte doubles,
`maki/array.f`), not packed f32; device buffers come from `cuMemAlloc`, whose
alignment guarantee is recorded into the descriptor at allocation time, and
externally-bound buffers record the alignment their binding declares. Packed
f32 host buffers arrive with the unified tensor value (§3). Vectorization
(§6.2) and the schedule-cache alignment class (§7.4) key off the recorded
facts; a descriptor with no recorded alignment gets the conservative class,
never a guess.

### 4.2 Op registry

Each op declares:

```text
name           gelu
class          elementwise | row-reduce | full-reduce | matmul | movement | decode
flops/elem     8
bytes fn       derived from class (elementwise: in+out; matmul: tiles model)
accum dtype    f32 for reductions/matmul regardless of input dtype
vjp            model-op adjoint id (§12 registry; or none + reject on GRADCHECK)
numeric        exact | ulp(n) | rel-tol class (drives GOLDEN tolerance, §11)
attrs          axis, eps, approx-variant, ... (typed per op)
reference      scalar CPU word (the golden oracle at op granularity)
```

The registry is the single extension point: adding an op means one entry, one
scalar reference, one VJP. Planners never learn op names. The registry —
including the cost fields, numeric class, and reference binding — is a
`cad-1` deliverable alongside the node table. **Membership is gated:** an op
enters the op set only when its scalar reference exists; `GOLDEN` fails
closed (named reason, never a skip) on any region containing an op without a
reference. Today that means silu, rmsnorm, and rope need host scalar
references written as `cad-1` sub-tasks before they are usable ops, and
rope's device kernel arrives later with `habu-ptx-kernels-rmsnorm`.

Movement ops (reshape/view, transpose, slice, concat, gather) carry no
compute; they are layout rewrites the planner either dissolves into index
arithmetic or converts into explicit materialization (§6.3).

## 5. Fusion planner

### 5.1 Regions and iteration space

A region is a set of nodes executed as one kernel. Every region has a
canonical iteration space: the index space of its output tensor. A producer
may join a region only if its output is expressible per-lane as an affine
function of that space (identity, broadcast, stride rewrite, slice offset).

### 5.2 Legality matrix

```text
producer ↓ consumer →   elementwise   row-reduce      matmul          movement
elementwise             fuse          fuse (prologue) fuse (prologue) dissolve/mat
row-reduce              fuse (epi.*)  split (barrier) split           split
matmul                  fuse (epi.)   split           split (v1)      split
movement                dissolve/mat  dissolve/mat    dissolve/mat    dissolve
decode                  fuse (chain)  split           split           split
```

`*` a row-reduction's scalar result may feed elementwise consumers over the
same rows inside the region (softmax normalize pattern) when the schedule
keeps the row resident (block-per-rows, §7.2); otherwise split with reason
`barrier-boundary`. Two sequential same-row reductions (max then sum) fuse
with an in-block barrier — that is exactly `softmax-row-v1`. Full-tensor
reductions split into partials + final (v1).

The matrix deliberately omits two classes: `full-reduce` regions are always
two kernels in v1 (the prose rule above), and `decode` is never a *consumer*
— no op feeds into a decode within a region in v1, so it has no matrix
column; the decode→elementwise `fuse (chain)` row stands, and decode chains
end at materialized outputs.

Matmul fuses loads-side prologues (dequant/scale) and epilogues (bias,
activation, residual add, norm scale) only. Matmul→matmul fusion is out of
scope for v1 (`matmul-boundary`).

### 5.3 Multi-use producers

A multi-use tensor is either materialized once (default) or recomputed into
each consumer region when `recompute-flops * flop-cost ≤ bytes-saved *
byte-cost` under the calibrated cost model. The decision is per-edge,
reported, and revisited by measurement like any fusion decision.

### 5.4 Resource bounds

At plan time, per region: `Σ register-estimate(op)` per class table,
shared-memory bytes from the schedule family, occupancy floor (≥ 25% v1).
Exceeding a bound splits the region at the cheapest cut — the edge whose
materialization adds the fewest estimated bytes. After assembly, `ptxas -v`
actuals replace the estimates (§9); a region whose actuals violate the plan
fails CERTIFY rather than silently running worse.

### 5.5 Algorithm

Greedy region growth over the DAG in topological order, guarded by §5.2
legality and §5.4 bounds, objective = minimize estimated global bytes moved,
tie-break on launch count. Deterministic: same IR, same plan. `FUSE` reports
ops before/after, regions, per-split reason, estimated bytes before/after.

### 5.6 Split reasons (typed, exhaustive)

```text
barrier-boundary | register-pressure | smem-pressure | occupancy |
multi-use-materialize | layout-conflict | matmul-boundary | numeric-policy |
user-pin | measured-regression
```

### 5.7 Profitability memory

Measurement closes the loop: if a fused region profiles slower than its
unfused baseline (both cached), a negative profitability fact keyed by
(region signature, shape class, target) is stored in the artifact cache, and
the planner splits that region next time with reason `measured-regression`.
Learning is data in the cache, not code changes.

## 6. Memory and layout planner

### 6.1 The coalescing contract

Adjacent lanes access adjacent addresses on the innermost varying dimension.
Every global access in a generated kernel either satisfies the contract, is
staged through shared memory to satisfy it, or is reported as
`strided(k)`/`gathered` with the blocking fact. There is no silent scattered
access.

### 6.2 Decision procedure (per region)

1. **Iteration mapping.** Innermost lane dimension = the contiguous dimension
   of the byte-majority of hot tensors. If producers and consumers disagree,
   compare layout-conversion cost (one staged pass) against the strided
   penalty (effective-bandwidth divisor table, calibrated) and pick the
   global minimum; a conversion becomes an explicit materialization node.
2. **Vector width.** Largest w ∈ {4,2,1} with `alignment ≥ w·esize`,
   unit stride, and `extent ≥ w`. Tail = `extent mod w` → masked tail path
   (the existing `tile-v4` pattern). Alignment below 4B → w=1 + warning.
3. **Broadcasts.** Scalar and per-row broadcasts hoist to registers (load
   once per block), never re-read per lane.
4. **Shared memory.** Stage when the region contains a transpose, when lanes
   reuse each other's loads (matmul tiles, row reductions wider than a
   warp), or when a layout conversion pays (the global-minimum criterion in step 1, materialized per §6.3). Bank-conflict rule:
   pad the staged row stride by one element when
   `(row-stride · esize) mod 128 == 0`.
5. **Emit plan rows** (§6.4) — codegen consumes them verbatim.

### 6.3 Movement-op dissolution

```text
reshape/view   free when contiguity permits (stride rewrite); else materialize
transpose      dissolve into lane mapping inside a staged region; else materialize
slice          offset + stride rewrite; masked tail if unaligned
concat         v1: materialize (lane-range dispatch is a later extension)
gather         prologue-only indexed read; downstream access reported gathered
```

Every materialization is a report row with a reason; the count of
materialized movement bytes is part of the traffic delta `MEMORY` shows.

### 6.4 Memory plan rows

```text
tensor  access                     detail
x       coalesced-v4 global load   16B aligned, tail masked N mod 4
w       staged shared              bank-pad +1, reused 8x per tile
bias    broadcast register         hoisted, per-block
y       coalesced-v4 global store  16B aligned
traffic before → after             bytes, per region and total
warnings                           strided(k)/gathered/unaligned facts
```

## 7. Tiling and schedules

### 7.1 Schedule = typed template instance

A schedule never free-forms: it instantiates a *family* with a bounded,
enumerable parameter space. This is what makes tuning tractable and replay
exact.

```text
schedule fields:
  region id, family id, target (sm_87 ...), shape class, dtype key,
  layout key, family parameters, expected registers, expected smem,
  measurement history (appended by TUNE/PROFILE)
```

### 7.2 Families (v1) and parameter spaces

```text
elementwise-v1    block ∈ {128,256,512} × vec ∈ {1,2,4} × grid-stride ∈ {y,n}
row-reduce-v1     lanes/row ∈ {32,64,128,256} × rows/block ∈ {1,2,4} × vec
softmax-row-v1    row-reduce-v1 × online-softmax ∈ {y,n}
gemm-tf32-v1      BM,BN ∈ {64,128} × BK ∈ {32,64} × warps ∈ {4,8} ×
                  stages ∈ {1,2} × epilogue ∈ {none,bias,bias+act}
decode-v1         block/row × ballot-compaction (PBD-style chains)
```

Default selection is closed-form, before any tuning:

- elementwise: 256 threads, max legal vec, grid-stride on.
- row-reduce: lanes/row = min(256, next-pow2(rowlen/8)); rows/block fills a
  warp multiple; two-pass unless the family supports online accumulation.
- gemm: smallest tile with `blocks/SM ≥ 2` by the occupancy model, then
  largest BK that fits smem for the stage count.

The defaults must land within ~25% of tuned on the benchmark classes in
`docs/model-cad.md`; if they don't, the default formula is the bug to fix —
users get good performance before the tuner ever runs.

### 7.3 Legality pruning

Candidates violating smem capacity, register bounds, or occupancy floor are
pruned by the cost model before emission; pruning reasons are recorded so
`TILE` can show why a candidate is absent, not just which survived.

### 7.4 Keys and shape classes

Cache and replay key: `(region signature, shape class, dtype key, layout key,
alignment class, target, engine hash, ptxas version)`. Shape class: exact
extents ≤ 64, else power-of-two bucket + tail flag. A query outside every
measured band reports "unmeasured shape class — using defaults" rather than
silently trusting a distant winner.

## 8. Tuner

`TUNE` enumerates the family's *searched* space — the epilogue is fixed by
the fusion plan, never searched, so the searched spaces are: elementwise ≤
18, row-reduce ≤ 36, softmax-row ≤ 72 (the online-softmax bit doubles it),
gemm-tf32 ≤ 32 (BM,BN × BK × warps × stages), decode ≤ 8. The bound "≤ ~100
points per family" keeps exhaustive search cheap. `TUNE` measures each
candidate on device with the same harness `PROFILE` uses, records *every*
candidate (never only the winner) into measurement history, selects by median
device time, and caches the winner by key (§7.4). Replay by key reproduces a
recorded run. A new measurement that regresses against the cached baseline
flags the report and keeps the baseline until re-promoted. Search stays
exhaustive in v1; the bounded spaces make that cheap, and smarter search is a
drop-in later. The measurement/enumeration machinery is built by
`cad-6-tune` over the CUDA-event timing harness (`tools/ptx/bench.f`); no
autotuner exists today to reuse.

### 8.1 Compute-bound strategy (the beat-Triton plan)

Measured reality first (docs/eval-triton.md, real Triton 3.5.1 on this Orin):
memory-bound kernels are PARITY at the streaming ceiling (~63 GB/s both) —
nobody beats DRAM — and no GEMM comparison has been measured yet. The
compute-bound plan, in dependency order:

1. **Tensor-core MMA emission** (`habu-tensor-core-mma`): checked emitters for
   `mma.sync.aligned` TF32 first (the family is already named gemm-tf32), then
   fp16/bf16 with f32 accumulate. We emit PTX text directly — no LLVM between
   the schedule decision and the instruction. Without this lever there is no
   compute-roof contest at all.
2. **`cp.async` + multi-stage SMEM pipelining**: the schedule family already
   parameterizes `stages`; the emitter honors it with double/triple-buffered
   `cp.async` staging. Pure emitter work on existing machinery.
3. **Persistent autotuning beats JIT autotuning** (`cad-6-tune` + the §13
   store): Triton tunes at JIT time, in-process, per deployment, with generic
   configs on sm_87. We tune once on the real device, key by §7.4, store the
   winner with evidence, and replay with zero warmup — so we can also afford
   larger search spaces, paid offline.
4. **Fusion depth is the real Orin lever**: this target is memory-starved, so
   most "compute-bound" work is composites whose intermediates spill. The
   planner owns the whole IR with exact bytes: GEMM with prologue
   dequant/epilogue bias+activation in one kernel (slice 3), and the
   attention megafusion (QK^T -> softmax -> V, SMEM-resident;
   `habu-re-express-fused`, `habu-ptx-m11-attention`). End-to-end model
   latency is the honest metric, and fewer launches moving fewer bytes wins
   it even at equal per-kernel FLOPs.
5. **Whole-model decisions a kernel DSL cannot make**: weight layout owned at
   PROMOTE time (pre-transpose/pre-swizzle into the artifact —
   `habu-cad-weight-layout`); launch amortization on Jetson-class overheads
   (persistent kernels / a graph-style driver loop —
   `habu-cad-launch-amortize`); precision policy LICENSED by the gates —
   TF32/FP16 applied only where GOLDEN + gradcheck prove it safe
   (`habu-cad-precision-policy`).
6. **Roofline-directed search**: PROFILE's classification (§9) spends tuner
   candidates only on regions actually under the compute roof.

Sequencing: slice-3 GEMM -> register-blocked GEMM tile + the FIRST measured
GEMM-vs-Triton baseline [LANDED 2026-07-04: lower-mm.f blocked 64x64 tile,
device-golden green; fp32 GFLOP/s at 512..2048 square = ours naive ~55 flat,
ours blocked 357 rising to 381 (6.5-7.0x), Triton autotuned TF32-dot 1636
rising to 1891 (4.6-5.0x over our blocked) — docs/eval-triton.md "GEMM: the
FIRST measured compute-bound column"] -> pipeline the blocked GEMM (step 2)
[2A LANDED 2026-07-05: bk=16->32 family floor + ld.shared.v4 B load, blocked
379, 397, 403 GFLOP/s at 512, 1024, 2048 (+6% over the bk16 baseline), goldens
green, 48 regs and 16 KB smem] -> on-device PROFILE/roofline ->
cp.async stages (step 2B) -> MMA family -> cad-6 tune -> attention megafusion ->
end-to-end model latency vs torch.compile on the detector-class workload. Honest finish line: parity on
the pure compute roof (tensor cores are tensor cores), win on everything
around it — fusion depth, zero-warmup replay, layout ownership, launch count —
which is where end-to-end latency lives.

## 9. Cost model and calibration

- **Bytes:** per region, unique global reads + writes after fusion, with
  broadcast discount and recompute duplication. This same number is the
  fusion objective, the `MEMORY` traffic report, and the roofline
  denominator — one model, three consumers, falsifiable against measured GB/s.
- **Occupancy:** registers/thread (class table) and smem/block (family) →
  blocks/SM → occupancy. Floors prune schedules; estimates are replaced by
  assembled actuals, and the class table is corrected persistently when
  actuals diverge — the model self-calibrates. The capture itself is new
  work: today `lib/ptx/toolchain.f` runs `ptxas` without `-v` and nothing
  queries `cuFuncGetAttribute`; parsing `ptxas -v` (toolchain-side) and/or
  reading function attributes (device-side) lands with the schedule work
  (`cad-4`/`cad-6-tune`).
- **Roofs:** per target, measured once by microbenchmark and cached with the
  target key. The streaming-bandwidth microbench exists
  (`tools/ptx/bandwidth.f`); the f32 and tf32-tensor FLOP-roof microbenches
  are new work owned by `cad-6-tune`, and together they replace the
  hardcoded constants in `tools/ptx/profile.f` (`MEM-ROOF-GBS-X1000`,
  `FP32-ROOF-GFLOPS-X1000`) — the one place the current code violates design
  rule 4. Profile rows classify regions memory-/compute-/launch-bound by
  arithmetic intensity against measured roofs and state % of roof.
- **Next move:** a fixed advice table keyed by classification: memory-bound →
  fuse producer / improve coalescing; compute-bound → tensor cores / tiling;
  launch-bound → fuse launches; low occupancy → reduce registers or tile.

## 10. Codegen contract

Plans are law. The kernel emitter lowers a region + layout plan + schedule to
checked kernel words (the existing `KERNEL:`/tile/collective vocabulary) and
may not re-decide anything: vector widths, staging, tile shapes, and tail
paths come from the plan. Two enforcement points:

1. Generated kernels are *checked words* — the emitter's output must pass the
   checker (address spaces, extents, masks, uniformity) before any device
   step. Codegen that emits unverifiable code is a bug that fails loudly.
2. CERTIFY includes plan/code coherence: emitted access patterns are compared
   against the memory plan rows, and `ptxas -v` resources against the
   schedule's expectations. Divergence fails certification; it never ships as
   a silent performance surprise.

## 11. Gates and evidence

```text
CERTIFY     static: checker pass, plan/code coherence, barrier/uniformity
            legality — runs anywhere. The resource-bound leg (assembled
            registers/smem vs plan) needs the CUDA toolkit (ptxas), not a
            device: no kernel executes, but it is not host-portable.
GOLDEN      device output vs reference: the op-registry scalar reference
            composed over the region, or an external reference artifact
            (saved tensor dumps) with per-artifact tolerance.
GRADCHECK   central finite difference vs generated backward, per VJP and per
            fused backward region.
DETERMINISM optional repeated-run check; schedules using atomics are marked
            nondeterministic and are opt-in.
PROFILE     device timing + roofline row + baseline comparison. Mandatory to
            run before promotion, but non-blocking: a regression flags the
            report and keeps the cached baseline; it does not veto.
```

`GOLDEN`'s reference machinery is owned explicitly: composing per-op scalar
references over a region requires a **host model-IR reference executor** (a
topo-order walk of the node table calling each op's reference on host
tensors) — a `cad-7-optimize` deliverable; the **external reference artifact
loader** and its on-disk format (tensor dump + recorded tolerance per
artifact) are part of the same dot, driven by the LocateAnything-port
workload.

Tolerance policy is a function, not a vibe: `|a−b| ≤ atol + rtol·|b|` with
per-dtype defaults (f32: atol 1e-6, rtol 1e-5; f16/bf16: atol 1e-3, rtol
1e-2), tightened to exact for movement ops and loosened only by an op's
declared numeric class (e.g. `gelu-approx` under an explicit tolerance
attribute). Reductions and matmul accumulate f32 regardless of input dtype;
fusion never changes accumulation dtype (`numeric-policy` split otherwise).

`PROMOTE` writes the artifact — PTX, cubin hash, plan, schedule, evidence
rows, tolerances — keyed as §7.4, and refuses unless CERTIFY + GOLDEN
(+ GRADCHECK when a backward exists) all pass. There is no force flag.

## 12. Autograd and training

Model-level backward is **new work** (`cad-9-backward`), designed as VJP
substitution over the model IR: a model-op adjoint registry (the §4.2 `vjp`
field points here) plus a reverse transform that emits backward regions *as
model-IR nodes*, so backward elementwise chains fuse, backward reductions
schedule as row-reduce, and epilogue gradients fuse into backward matmuls
through the same planners. What exists today operates one level down and is
reused as substrate, not mistaken for the model-level pass: `lib/ptx/ad-dag.f`
is the kernel-IR reverse pass over the softmax-rows primitive set, and
`lib/ptx/ad.f` is its v0 token-substitution predecessor whose `VJP-SAVES`
records only a save *count* — the save-vs-recompute *decision* is part of
`cad-9-backward`.

- **Save vs recompute** is a fusion-planner decision under the §9 bytes/FLOPs
  model (saving = a write + a read; recompute = FLOPs + upstream reads),
  reported per tensor.
- **The training step is one plan unit:** forward + backward + optimizer
  update planned together; the optimizer update (elementwise class) fuses
  into the final backward region when legal. `PROFILE` reports the step.
- **Convergence is a gate:** seeded data + loss threshold committed as tests
  (`maki/train.f` pattern), so a planner change that breaks training fails CI
  like a wrong answer would.

## 13. Incremental replanning and caching

Everything is content-addressed: IR nodes by (op, attrs, input hashes, keys);
regions by node-hash sets; plans and schedules by the §7.4 key. A model edit
re-hashes only the downstream cone — upstream regions keep their plans and
artifacts, so the edit→report loop stays interactive. The artifact cache is
the single store for kernels, evidence, measurement history, profitability
facts, and calibration tables. This store is **new**: the only content-keyed
cache in the repo today is the AOT build-image cache
(`tools/hb-build-lib.f`, `lib/content-key.f`), which is keyed by source
digest, not by region/shape/dtype/layout/target — the CAD store gets its own
on-disk layout and owning dot (`cad-5-store`), and
`habu-kernel-artifact-export` is the externalization of *this* store once it
exists.

**Backend decision (2026-07-04).** The store is line-oriented append rows
while it remains a private, per-workspace, regenerable cache (KB–MB scale;
exact-key GET/PUT + full-scan load; single writer): keys are opaque
identities matched whole, never field-parsed on read, so the row format is a
serialization boundary, not a data model — the data model is the typed SKEY
record (`habu-cad-adt-swap`). The seam is `STORE-QUERY`/`STORE-PUT`: when the
store becomes shared, concurrently written, or queried (the cad-6 device
measurement corpus is the expected trigger), swap the backend behind that
seam — `OBJSTORE` (`lib/object-cache.f`, content-addressed files, in-tree) or
SQLite via FFI (dlopen precedent: `libcuda`) — without touching callers. JSON
stays an export format only (`lib/json-write.f` is emit-only; a store would
need a checked parser that buys nothing for same-program data).

## 14. EXPLAIN and repair packets

Every failure — legality, gate, resource, unmeasured shape — renders as:

```text
failure class      (typed enum, one of the §5.6 reasons or gate failures)
site               region / op / tensor id + source location of the model word
expected           the contract (typed)
actual             the observed fact (typed)
blocking fact      the single fact that fired the rule
repair family      fixed table: layout-conflict → transpose materialization or
                   producer layout change; register-pressure → split point or
                   smaller tile; tolerance → approx off / dtype up; ...
minimal repro      smallest region that reproduces
```

This is the same packet discipline the checker's repair tooling uses
(`tools/repair-packet-core.f`, the `habu_repair_packet` shape) — the field
mapping: site → `word`/`file`/`line`, expected/actual → same names, failure
class → `reason`, repair family → `repair_class`, suggestion ↔ the repair
family's canonical move; EXPLAIN adds blocking fact and minimal repro on
top. `maki/eval-repair.f` is unrelated accounting (repair rounds and
tokens-to-green for the eval matrix).

## 15. Typed backbone hooks

When the type-families campaign lands (`docs/type-families.md`,
`docs/model-cad.md` §TFAM map): op classes and split reasons become enum
families; gate verdicts and fusion decisions become sum families eliminated
by `MATCH`; descriptors, plan rows, and schedules become product families;
and evidence becomes *evidence families* — `certified<region>`,
`golden<artifact>` — so `PROMOTE`'s signature can require evidence values and
a gate bypass becomes untypeable, not just forbidden. Until then the same
data lives in checked records behind accessor words whose signatures will
not change (`cad-adt-swap`).

## 16. Extension playbook

- **New op:** registry entry (class, cost, numeric class, attrs) + scalar
  reference + VJP entry + `T{ }T` tests. No planner changes.
- **New schedule family:** template + parameter space + default formula +
  legality bounds + emitter case. Tuner and cache pick it up from the family
  registry.
- **New target:** run the roof microbenchmarks, record resource caps
  (smem/block, registers, warp size), add the ptxas arch flag. Plans and
  tuning re-key automatically.

## 17. v1 scope

In: elementwise and row-reduction mega-fusion with prologue/epilogue matmul
fusion; coalescing/vectorization/tail planning; the five §7.2 families on
sm_87; golden/gradcheck/profile gates; exhaustive tuning; artifact cache;
incremental replanning; training-step planning for the from-scratch flagship.

Out (explicitly, with the §5.6/§6.3 reasons reported when hit): matmul→matmul
fusion, cross-block softmax single-kernel, concat lane-dispatch fusion,
CUDA graphs, persistent kernels, multi-GPU, autotuned search beyond
exhaustive, non-NVIDIA targets.
