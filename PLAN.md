# Reusable Maki/Habu-PTX DSL plan

## Goal

Build reusable, checked GPU and model-building DSL blocks first, then assemble a
nanoGPT-class tiny causal Transformer from those blocks as the capstone proof.

The reusable product is the important part: PTX planning, kernel construction,
device runtime, profiling, fusion, tiling/MMA, collectives, autograd, Maki graph
lowering, and model execution must be generic enough for other GPU projects. The
nanoGPT path may have its own thin model DSL, but it must not own the generic
kernel machinery.

Completion means the Orin runs the tiny causal GPT end to end: forward pass,
autodiff backward, AdamW update, loss decrease, CPU/device parity within
dtype-matched tolerance, and profile evidence explaining every memory-vs-compute
choice.

## Non-negotiables

- New implementation, tests, tooling, benchmark reducers, and code generators are
  checked Habu Forth run by `bin/hb`.
- Public/library Forth uses `package`, `require`, `case` where it clarifies
  selector logic, small typed words, and useful stack effects. Unchecked code is
  only a named, tested boundary with a dot for the missing typed capability.
- The public DSLs must be beautiful: domain words compose into readable programs;
  raw PTX strings, `/tmp` drivers, CUDA handles, register names, and SAXPY-shaped
  assumptions stay behind typed package internals.
- Legacy global PTX tile/collective words are migration debt, not the final DSL.
  New reusable PTX surface must be exported from `package PTX`; callers outside
  that package use `PTX:*`, and package-isolation negatives prove bare global
  names no longer leak as public API.
- Generic packages are model-agnostic. `lib/ptx/*` and `tools/ptx/*` must not
  reference `maki/`; `maki/` adapts model graphs to generic PTX plans; the
  nanoGPT DSL sits above Maki. This must be enforced by lint, including generic
  `tools/ptx/` paths, not only held by review.
- Target details are explicit data, not hidden constants. Architecture, CUDA/ptxas
  resolution, device caps, supported PTX features, and roof tables flow through
  planning, header emission, ptxas, launch, and profile rows. Orin/sm_87 is the
  first fixture, not the generic API.
- SAXPY remains only a smoke fixture for load/marshal/copy/launch/sync/golden.
  It is not the abstraction boundary.
- Kernel decisions start with roofline classification: memory-bound work fuses
  to reduce HBM traffic; compute-bound work tiles and uses tensor-core MMA where
  dtype/tolerance allow it.
- Every performance claim has a CUDA-event `gpu_elapsed_ns` profile row with
  shape, dtype, 3D launch shape, bytes, FLOPs, GB/s, GFLOP/s, selected roof, and
  roof utilization.
- Master only moves after the exact rebased tree passes the owning Maki gate from
  `maki/README.md`, PTX/device slices, host/filemap/trust/signature lints, and
  the full native gate.

## Layering

The dependency direction is fixed:

1. `lib/ptx/*`: generic checked GPU language blocks. No Maki or model concepts.
2. `tools/ptx/*`: generic device proof, ptxas, profiling, and benchmark tooling.
3. `tools/ptx/test-support*` or equivalent neutral helpers: reusable device
   golden/gradcheck support that stays independent of Maki.
4. `maki/*`: tensors, graph/model ops, CPU reference, autograd orchestration, and
   lowering from Maki graphs to PTX plans.
5. `maki/gpt*.f`: tiny GPT model DSL and fixtures using the reusable Maki/PTX
   blocks.

Every DSL layer must answer four questions in tests and docs:

- what the user writes in domain words;
- what the checker proves before runtime;
- what the planner/fuser/tiler chooses;
- what device/profile evidence is emitted.

## Current Baseline And Blockers

These are facts to recheck before editing the relevant files:

- `tools/ptx/bench.f`, `tools/ptx/profile.f`, and
  `tools/ptx/device-support.f` are the intended generic `PTX` package surface.
  They are not accepted as final until `habu-make-ptx-device-c0eb12a3` is closed:
  CUDA return codes, private temp roots, ptxas resolution, cleanup, and fail-closed
  hardware gates must be proven.
- Current benchmark launch state is mostly 1D. GEMM/MMA profiling needs generic
  2D/3D grid and block support before profile/autotune claims count.
- Some generic PTX tools still leak upward into Maki. `tools/ptx/softmax-gradcheck.f`
  has been migrated off `maki/`, but `tools/ptx/matmul-device-test.f` still reuses
  Maki eval-device helpers by load-order convention. These must be moved to
  neutral PTX helpers plus Maki adapters, or to `maki/`, and `maki-dep-lint` must
  scan generic `tools/ptx/` paths and run in the PTX-focused lint slice.
- Some device proof commands still rely on fixed `/tmp` artifacts or hard-coded
  ptxas paths (`cuda-load.f`, `cuda-launch.f`, `softmax-gradcheck.f`,
  `gradcheck.f`, `ptxas-smoke.f`, `matmul-device-test.f`). Phase 1 must retrofit
  or replace every such proof before any downstream device evidence counts.
- `sm_87`, CUDA 12.6 paths, and Orin roof constants are still hard-coded in parts
  of the PTX stack. The generic layer needs an explicit target-capability and roof
  table before non-Orin reuse is claimed. Header emission currently starts below
  the library layer, so target-aware header rendering must be owned by a PTX
  library package while the lower arch emitter keeps only primitive line emission.
- `lib/ptx/tile.f`, `lib/ptx/tile-v4.f`, `lib/ptx/collective.f`,
  `lib/ptx/tile-loop.f`, `lib/ptx/tile-smem.f`, and `lib/ptx/tile-acc.f`
  provide checked tile pieces that higher-level kernel DSLs must compose.
- `habu-ptx-m5-mask-eb0716f1` remains a blocker for divergent collective and
  barrier-reachability rejection. Reduction fusion, attention, and collective DSL
  claims must block on it.
- `habu-fix-ptx-collective-997cfcce` blocks softmax, loss, attention, and
  attention-gradient claims until row sum, softmax fwd/bwd, malformed launch, and
  collective edge cases are device-proven.
- `lib/ptx/cg-matmul.f` and `lib/ptx/cg-attention.f` are performance-critical
  seeds, but both still contain raw PTX islands. GEMM has a checked surface but
  fixed 64x64x16 assumptions; attention is fixed `N<=128,D<=64`, noncausal, and
  an unchecked boundary.
- `maki/fusion.f` is currently an elementwise string-concat proof with
  scale/add/ReLU and a fixed `/tmp/fuse-driver.f`; it must become a graph/fusion
  layer over generic PTX planning and private temp roots.
- `maki/onnx.f` maps `Gemm` to `SAXPY` and has no graph/protobuf import. The GPT
  capstone will use native Maki graph construction first; ONNX import remains a
  generic model-ingest block and must route `MatMul`/`Gemm` to GEMM planning when
  it is in scope.
- `maki/attention.f` is a CPU reference for unmasked attention. The capstone needs
  causal semantics in CPU and device paths.
- `maki/softmax.f` and `maki/celoss.f` already own CPU softmax/CE pieces. New CE
  work is PTX fused logits-domain CE lowering plus device gradcheck, not a second
  generic CE implementation.

## Capstone Workload

The capstone is a deliberately small but real causal GPT:

- integer token ids over a tiny committed corpus fixture;
- token embedding plus learned positional embedding;
- one or more Transformer blocks: LayerNorm, QKV projections, causal attention,
  output projection, residuals, MLP projection, GELU, projection back, residual;
- logits projection and logits-domain fused softmax-cross-entropy next-token loss;
- backward pass through embedding, projections, attention, LayerNorm, GELU,
  residuals, and loss;
- AdamW update over model parameters;
- deterministic CPU Maki reference and device execution path.

First Orin target: sequence length 16-64, embedding width 32-128, one head, one
block, and a tiny vocabulary. The generic DSLs must not bake in those dimensions.

Required causal semantics:

- an inclusive lower-triangular mask is applied before softmax;
- output at position `i` cannot depend on token positions `j > i`;
- backward gradients preserve the same dependency rule;
- tests include `L=1`, `L=2`, partial rows, future-token sentinel values, and a
  noncausal negative/rejection path.

Required input and numeric semantics:

- token ids are integer typed or checked-converted, bounded by vocabulary size,
  require an explicit vocab length, and reject negative, fractional, NaN, and
  `id == vocab` inputs. Tests cover max-valid ids and repeated-id gradient
  accumulation.
- model shape contracts reject zero/negative sequence length, context length,
  vocabulary size, `d_model`, head dimension, QKV/projection/residual/LayerNorm
  mismatch, logits/target mismatch, scratch size, and overflowed byte/FLOP
  products with named errors;
- fused CE accepts logits rows plus integer class ids, uses logsumexp, validates
  target range and row shape, and has dtype-specific absolute/relative tolerances
  for huge positive/negative logits, equal logits, target `0`, target `vocab-1`,
  `-1`, `vocab`, and malformed one-hot boundaries.
- probability-space one-hot CE and raw `FLN` over probabilities are not accepted
  on the GPT path. Logits-domain CE must be a replacement boundary with named
  domain errors for nonpositive log inputs, NaN/Inf, underflowed rows, malformed
  class ids, and evidence that the fused path never computes `ln(softmax(x))`.

## Generic DSL Blocks

### 1. Beautiful DSL Contract

Files: `docs/ptx.md`, `docs/kernel-principles.md`, new examples near each DSL
test, and package-local tests.

Purpose: make the user-facing language read like the domain while keeping the
checker-visible types precise.

Required contract:

- PTX users write typed kernel pipelines in words for spans, matrices, tiles,
  reductions, barriers, fragments, and plans; they do not write PTX strings in
  public code.
- Maki users write model/graph words for tensors, parameters, layers, losses, and
  optimizers; they do not choose CUDA handles or register names.
- The nanoGPT DSL is a thin vocabulary over generic Maki ops: token embedding,
  learned position, transformer block, causal attention, MLP, logits, CE, and
  optimizer.
- Every public DSL example has a checked positive test and at least one negative
  fixture showing the checker/tooling rejects an illegal composition.
- The plan must carry concrete acceptance snapshots for the PTX kernel DSL, Maki
  graph DSL, GPT model DSL, benchmark DSL, and profile DSL. Each snapshot names:
  the domain words the user writes, the checker-visible effect being proven, the
  planner/fuser/tiler choice, and the profile row fields emitted.

Acceptance:

- examples for elementwise fusion, GEMM, attention, CE loss, and tiny GPT read as
  domain-level Habu words, not raw emit strings;
- public words are package-qualified where appropriate and have meaningful typed
  effects;
- every later DSL block has at least one package-qualified domain-level positive
  example and one illegal-composition negative fixture;
- no public PTX/Maki/GPT example exposes raw PTX strings, register names,
  string-builder APIs, SAXPY names, `/tmp` paths, ptxas paths, CUDA handles, or
  CUDA Driver plumbing.

### 2A. PTX Target Capability DSL

Files: add `lib/ptx/target.f`, `lib/ptx/target-test.f`, and a target-aware header
library such as `lib/ptx/header-target.f` if the existing header path cannot
consume target records without an upward dependency. Update `src/arch/ptx/emit.f`
so it remains only the primitive PTX line emitter; `lib/ptx/cg.f`, ptxas tools,
launch/profile tooling, and planner code consume the same target record.

Purpose: make target/device facts explicit data instead of scattered Orin
constants.

Required public shape:

- a checked target record carries PTX ISA version, `.target` architecture, ptxas
  arch flag, CUDA/ptxas command resolution, device caps, supported PTX features,
  dtype support, memory roof, FP32 roof, and tensor-core roof table;
- the first committed target fixture is Orin/sm_87/CUDA 12.6, but public word
  names stay generic and fixture values stay data;
- target records flow through header rendering, ptxas invocation, device launch
  validation, profile rows, and planner roof selection;
- missing target, unsupported dtype, unsupported feature, header/ptxas arch
  mismatch, PTX ISA/ptxas mismatch, and unknown roof selection reject with named
  errors.

Acceptance:

- tests prove one record drives header `.target`, ptxas `-arch`, launch/device
  caps, and selected profile roof;
- tests reject missing target, mismatched header/ptxas arch, unsupported dtype,
  unsupported PTX feature, and PTX ISA/ptxas mismatch;
- public-surface leak tests include target/header/profile fields and reject
  hard-coded Orin/sm_87/CUDA path/roof constants outside whitelisted fixture data;
- planner integration is accepted by the planner dot, not by the initial target
  record dot, so the target dot can close before `lib/ptx/plan.f` exists.

### 2. PTX Kernel Planning DSL

Files: add `lib/ptx/plan.f`, `lib/ptx/plan-test.f`, and negative tests if the
rejections need a separate slice. `lib/ptx/plan.f` owns its `require`s and exports
only package-public planner words; each test owns its setup. Update `FILEMAP.md`,
`tools/filemap-lint.f`, `test/gate-stdlib-cases.f`, and
`test/gate-stdlib-inline-lib.f`. Do not edit `lib/std.manifest` unless a flat
`lib/<module>.f` API is added. Update `lib/ptx/test-prelude.f` only if planner
words become shared PTX test setup.

Purpose: classify a kernel or fused region before codegen. The planner computes
bytes, FLOPs, arithmetic intensity, selected roof, bound class, lowering choice,
resource envelope, and benchmark metadata.

Milestones inside this block:

- plan record/schema and renderer;
- overflow-safe bytes/FLOPs/shape-product helpers shared with profile, device
  allocation, and Maki shape validation;
- elementwise and reduction traffic classifier;
- GEMM/GEMV/MMA shape classifier;
- attention/flash-attention classifier;
- resource-envelope validator for launch, shared memory, registers, parameter
  bytes, alignment, dtype, and target features;
- bench/profile metadata adapter.

Required public shape:

- elementwise/reduction plans choose `fuse` or `split` based on traffic and legal
  fusion barriers;
- GEMM plans choose scalar, tiled FP32, TF32/FP16 MMA, GEMV, or fail-closed based
  on shape, dtype, alignment, tolerance, and roofline target;
- attention plans choose fused/flash attention when the score matrix would
  otherwise round-trip through HBM;
- every plan exposes the fields needed to set `PTX:BENCH-*` state and to pass
  bytes/FLOPs into `PTX:BENCH-REPORT`;
- target capability records carry arch, PTX ISA feature flags, device caps,
  ptxas command, memory roof, FP32 roof, tensor-core roofs, and dtype support;
- unsupported dtype, alignment, shape, resource, or overflow cases throw named
  errors.

Acceptance:

- focused tests classify SAXPY/fused elementwise as memory-bound, tiled square
  GEMM as compute-bound, decode GEMV as memory-bound, and attention as a fusion
  target;
- tests cover empty, one, small, large, overflow, unsupported dtype/alignment,
  tensor-core roof selection, and launch/resource rejection;
- tests cover max-cell and near-overflow products for bytes, FLOPs, allocation,
  profile math, Maki indexing, and planner resource estimates;
- omitting `lib/ptx/plan.f` or `lib/ptx/plan-test.f` from FILEMAP or PTX gate
  wiring fails `filemap-lint` or the relevant PTX stdlib slice;
- planner examples satisfy the beautiful DSL contract: domain-level positive,
  illegal-composition negative, and no public raw emit strings;
- no CUDA is required for planner tests.

### 3. Device Runtime, Profiling, And Benchmark DSL

Files: `tools/ptx/device-support.f`, `tools/ptx/bench.f`,
`tools/ptx/profile.f`, their tests, and workload-specific benchmark drivers.

Purpose: provide a kernel-agnostic device proof and optimization harness. This is
generic infrastructure, not SAXPY tooling.

Required capabilities:

- scoped CUDA resource protocol for devices, contexts, modules, events, memory,
  and temp artifacts, with cleanup on every failure path and handle-zeroing after
  release. The public contract should expose named scope words such as
  `PTX:WITH-DEVICE`, `PTX:WITH-MODULE`, `PTX:WITH-EVENTS`, and
  `PTX:WITH-DEVICE-MEMORY`, or an equivalent checked scope DSL;
- fail-closed ptxas/CUDA/device errors through `PTX` named errors; raw `CALL* drop`
  is forbidden in device proof paths;
- private temp roots and `PTX:PTXAS-RUN-DEFAULT` for every generated PTX/cubin;
  no prerequisite `/tmp/*.cubin` tests;
- benchmark/profile is split into named reusable parts: typed row schema, GPU
  CUDA-event timing runner, explicit host timing runner, warmup/sample reducer,
  rejection formatter, stable report formatter, and workload adapters;
- `BENCH-GRID3!`, `BENCH-BLOCK3!`, dynamic shared-memory configuration, packed
  parameter-layout migration, parameter offset/size overflow checks,
  shared-memory/resource caps, stale-state reset checks, and rewritten
  `KERNEL-PREPARE-LAUNCH` / `KERNEL-LAUNCH` contracts;
- target capability inputs for arch, ptxas path, device caps, and roof table;
- typed `PTX:PROFILE-ROW` record, formatter, and rejector: label, op kind, shape,
  dtype, grid3, block3, iters, warmups, samples, work items, bytes, FLOPs,
  `gpu_elapsed_ns`, per-iteration ns, GB/s, GFLOP/s, selected roof, roof
  utilization, and rejection reason;
- a timing-kind enum keeps GPU rows, host rows, and rejected rows distinct. Host
  timing cannot be printed as `gpu_elapsed_ns`, and GPU rows reject missing,
  zero, negative, NaN/Inf/subnormal elapsed time, `iters <= 0`, incomplete fields,
  or invalid sample statistics;
- generic `PTX` public APIs do not expose workload or target fixture names/values:
  move `SAXPY-FLOPS` and `TRIAD-BYTES` into workload-specific smoke/adapters, move
  roof constants into target capability records, and make `PTXAS-RUN` consume
  target capability data or reject a missing target;
- low-level CUDA/module/parameter plumbing is private package implementation or
  explicitly whitelisted expert API. Maki/GPT examples must use the high-level
  profile/bench DSL and leak tests forbid direct coupling to CUDA handles;
- timing policy: CUDA events only for GPU rows, explicit host timing rows when
  needed, warmup count, sample count, median/min policy, variance threshold, and
  named rejection for zero/negative elapsed time.

Acceptance:

- fail-injection tests cover ptxas failure, module load, allocation, event create,
  first-event create, second-event create, launch, sync, copy/readback, cleanup,
  cleanup failure, partial acquisition, double release, and idempotent release;
- every existing device proof (`cuda-load.f`, `cuda-launch.f`, `gradcheck.f`,
  `softmax-gradcheck.f`, `ptxas-smoke.f`, `matmul-device-test.f`, SAXPY/v4/scatter
  proofs) is either retired or rewritten to emit into `PTX:TEMP-DIR!`, assemble
  through `PTX:PTXAS-RUN-DEFAULT`, use named RC wrappers, clean private roots on
  success/failure, and produce generic profile rows where relevant;
- generic `tools/ptx/` gates reject `maki/` requirements; shared device golden
  and gradcheck helpers live in neutral PTX support, with Maki-specific adapters
  under `maki/`;
- generic profile rows exist for elementwise fusion, softmax/loss, GEMM/MMA, and
  attention;
- tests reject host timing masquerading as GPU timing and incomplete profile rows;
- sample reducer tests cover warmups, sample count, median/min policy,
  zero/negative/NaN/Inf/subnormal rejection, variance rejection, overflow-safe
  per-iteration math, `iters <= 0`, timing-kind mismatch, and explicit rejection
  reasons;
- public-surface leak tests reject `SAXPY`, `TRIAD`, `Orin`, `sm_87`, fixed CUDA
  paths, and roof constants in generic `PTX` public words except whitelisted smoke
  fixtures;
- benchmark output is stable enough for gate assertions and optimization RCA.

### 4. Kernel Construction DSL

Files: extend `lib/ptx/tile*.f`, `lib/ptx/launch.f`, and existing checked PTX
fixtures only where current vocabulary cannot express the plan output. Add or
refactor package files as needed so tile, v4, shared-memory, loop, accumulator,
and collective words are exported from `package PTX`, not from the global
dictionary.

Purpose: make kernel bodies read as typed GPU operations. The stack carries spans,
matrices, grid/row contexts, shared-memory tiles, accumulators, fragments,
barrier phases, and uniform values.

Required capabilities:

- PTX package migration for legacy global tile/v4/smem/loop/acc/collective words:
  public exported words are `PTX:*`, private helpers stay package-local, callers
  outside `package PTX` are rewritten, and package-isolation negatives prove bare
  globals no longer resolve;
- checked loop/combinator surface for K loops and streaming reductions;
- shared-memory tile types distinct from global spans;
- accumulator types that cannot be stored until finalized;
- v4/vector and scalar residual paths under one typed surface;
- barrier/uniformity constraints that reject divergent collectives;
- resource-envelope types for launch dimensions, shared memory, register budget,
  parameter bytes, and static/dynamic limits.

Acceptance:

- checked positive fixtures certify representative elementwise, reduction,
  tiled-GEMM, and attention-shaped bodies;
- negative fixtures reject mixed masks/extents, global/shared confusion,
  divergent collective use, storing unfinished accumulators, illegal barrier
  phases, and resource-envelope violations;
- package negatives reject unqualified use of public PTX words from outside
  `package PTX`, duplicate public exports, and accidental legacy global aliases;
- `habu-ptx-m5-mask-eb0716f1` is wired into this phase, and collective/attention
  phases cannot claim static safety until divergent collective and barrier
  reachability negatives pass;
- kernel-construction examples satisfy the beautiful DSL contract;
- `trust-lint`, signature lint, and PTX stdlib slices remain green, with trust
  rows updated for every migrated `TRUSTED:` site.

### 5. Fusion DSL

Files: `lib/ptx/ir.f`, `lib/ptx/plan.f`, and new PTX-owned fusion files for the
generic region/barrier/planner data. `maki/fusion.f` becomes only a MAKI graph
adapter. Maki integration tests live under `maki/` and the Maki gate.

Purpose: fuse memory-bound chains and epilogues automatically while keeping
legality explicit. Concatenation is the proof shape, but graph region selection,
barriers, and profile evidence are real generic blocks.

Required capabilities:

- region builder over typed graph nodes with named split reasons;
- same-shape elementwise fusion, including bias, residual add, GELU/ReLU, scale,
  and affine transforms;
- alias, layout, shape-change, unsupported-reduction, dtype, register-pressure,
  and resource barriers;
- GEMM/attention epilogue hooks for bias, activation, dropout-mask-free paths,
  residuals where safe, and layout-preserving projections after the underlying
  GEMM/attention blocks exist;
- reduction fusion for LayerNorm and logits-domain softmax-cross-entropy after
  collective legality is proven.

Acceptance:

- first stage retires the fixed `/tmp/fuse-driver.f`, SAXPY-shaped driver, and
  opcode-to-raw-string public path;
- `FUSION:DRIVER` is replaced by a path-parameterized/private-temp driver or by a
  generic PTX region API that reports named split reasons;
- fused and unfused device tests produce the same values within tolerance;
- profile rows show reduced global bytes for fused regions;
- fusion examples satisfy the beautiful DSL contract;
- fusion failures throw named errors and do not silently split without reporting.

### 6. Tiling, GEMM, MMA, And Autotune DSL

Files: `lib/ptx/cg-matmul.f`, `lib/ptx/tile-smem.f`, `lib/ptx/tile-acc.f`, new
MMA files if needed, and device/profile tests under `tools/ptx/`.

Purpose: solve compute-bound kernels by feeding the compute roof.

Required capabilities:

- generic GEMM shape contract for square, tall-skinny, tail, and decode/GEMV
  regimes. Shapes required by the capstone (`L=16-64`, `d_model=32-128`, QKV,
  projection, logits, and MLP matrices) must be positive-supported; fail-closed
  rejection is allowed only outside the capstone envelope;
- tiled FP32 GEMM through checked `KERNEL:` bodies;
- TF32/FP16 MMA fragment types with lane-layout fixtures;
- shared-memory staging, padding/swizzle policy, larger BK, double buffering, and
  `ldmatrix` as explicit planner/codegen choices;
- shape-key autotune hooks that benchmark candidates through the generic PTX
  benchmark API and report the selected plan.

Acceptance:

- fragment-layout device tests pass against FP32 references with TF32/FP16
  tolerance;
- GEMM device tests cover square, M/N/K tails, tall-skinny, and decode/GEMV
  regimes;
- raw-emit islands are retired or explicitly dotted/audited at function level:
  `MM-STAGE`, `MM-KSTEP`, `MM-WRITE`, `EMIT-MATMUL`, and each `EMIT-ATTN` phase;
- same-run candidate profile rows explain selected plans; hardware-specific
  thresholds are optional and must use the Block 3 warmup/sample/variance policy;
- every rung has a profile row on the selected roof;
- GEMM/MMA examples satisfy the beautiful DSL contract;
- the checked surface prevents illegal fragment/dtype/alignment combinations.

### 7. Collectives, Softmax, Attention, And Loss Blocks

Files: `lib/ptx/collective.f`, `lib/ptx/cg-collective.f`,
`lib/ptx/cg-attention.f`, `maki/attention.f`, `maki/softmax.f`,
`maki/celoss.f`, and device/gradcheck tools.

Purpose: make reduction-heavy model kernels reusable and checked before the GPT
capstone depends on them.

Required capabilities:

- close `habu-fix-ptx-collective-997cfcce` before downstream claims;
- block on `habu-ptx-m5-mask-eb0716f1` before divergent collective, barrier, or
  reduction-fusion claims;
- typed causal-mask token and negative fixtures for noncausal use where causal
  attention is required;
- CPU causal attention reference lands before device attention claims. The
  current unmasked `ATTN-FWD` may remain a reference for noncausal attention, but
  GPT-path code must reject or avoid it explicitly;
- online softmax with running max/sum rescaling and no score-matrix HBM
  materialization for flash attention;
- logits-domain fused softmax-cross-entropy over logits rows plus integer class
  ids, with `p - onehot` backward. Existing probability-space one-hot CE is
  quarantined from the GPT path until it has named domain guards, and final GPT CE
  must use logsumexp over logits directly;
- fixed `N`/`D` limits are temporary blockers, not final acceptance. The final
  generic attention block must support the capstone envelope or the capstone
  dimensions must be narrowed explicitly before implementation;
- explicit size limits while fixed shapes remain, and fail-closed tests above
  supported `N`, `D`, block, and shared-memory limits.

Acceptance:

- CPU attention reference and PTX attention agree on causal outputs;
- device tests include forward golden, malformed launch, size-limit rejection,
  and future-token sentinel cases;
- backward causal sentinels prove loss at position `i` has zero gradient with
  respect to token positions `j > i`; tests cover row 0, last row, and partial
  tiles with masked identities applied before max/sum;
- softmax/loss forward and backward have CPU numeric gradchecks and device
  finite-difference gradchecks;
- logits-domain CE tests prove no GPT-path kernel computes `ln(softmax(x))` or
  consumes malformed one-hot rows;
- CE tests cover huge positive/negative logits, equal logits, target `0`, target
  `vocab-1`, `-1`, `vocab`, and malformed one-hot boundaries;
- attention/loss examples satisfy the beautiful DSL contract;
- no generated GPT-path attention/loss kernel relies on unchecked score-matrix
  materialization.

### 8. Autograd And VJP DSL

Files: `lib/ptx/ad*.f`, `maki/autograd*.f`, `maki/*-test.f`, and device
gradcheck tools.

Purpose: make backward kernels a checked product of the same typed language, not
a trusted hand-written shadow.

Required capabilities:

- tensor-scale VJP table for matmul, attention, LayerNorm, GELU, residual,
  embedding/gather, softmax-cross-entropy, and optimizer-visible parameter use;
- generated backward kernels that certify under the checker;
- scatter-add as the conservative load/gather adjoint default;
- save-vs-recompute policy for attention and LayerNorm, with explicit cost model
  and equivalence tests;
- closure of unresolved `SAVED-*` stubs for any GPT-path backward, or a named dot
  and explicit exclusion from final capstone acceptance;
- saved values are keyed by graph identity, run/training-step identity, op
  instance, row, shape, and saved-value kind, or are recomputed before mutation;
  global unkeyed `SAVED-*` names are not accepted in generated GPT-path PTX;
- fail-closed rejection for unsupported control flow or unsupported tensor ops.

Acceptance:

- every VJP entry has a CPU numeric gradcheck and a device finite-difference
  gradcheck when it lowers to PTX;
- generated backward kernels have checked positive fixtures and negative tests
  for unsupported shapes/control flow;
- tests cover two nonlinear op instances and forward-mutation-before-backward so
  saved-value aliasing cannot pass; tests also cover two graph instances and two
  repeated training steps using the same op shapes;
- autograd examples satisfy the beautiful DSL contract;
- transformer-block gradient parity matches the CPU reference within tolerance.

### 9. Maki Device Tensor And Graph Lowering DSL

Files: `maki/tensor*.f`, `maki/gpu.f`, `maki/train.f`, `maki/optim*.f`,
`maki/onnx.f`, `maki/fusion.f`, `maki/attention.f`, `maki/layernorm.f`,
`maki/gelu.f`, `maki/embedding.f`, `maki/softmax.f`, `maki/celoss.f`, plus new
files only when one concern cannot fit an existing module.

Purpose: provide generic model graph construction and device-resident execution
over the PTX blocks.

Required capabilities:

- typed device tensor handle over PTX span/matrix metadata, dtype, shape, layout,
  and allocation lifetime;
- explicit tensor shape policy. Either the generic graph supports rank/stride
  metadata for batch/time/channel/logit shapes, or the capstone explicitly
  documents `batch=1` flattened 2D as a temporary limitation before
  implementation. Tests cover B/T/C/logits/targets, stride/layout mismatch, and
  overflowed byte products;
- reusable token/class-id owner before GPT: integer storage or checked conversion,
  explicit vocab length, named validation errors, and CPU/device embedding gather
  plus scatter-add tests for fractional, NaN, negative, max-valid, `id == vocab`,
  huge id, empty ids, and repeated ids;
- public Maki ops on the GPT path have shape/domain guards before they act:
  softmax/CE, LayerNorm, attention, embedding, matmul/projection, optimizer state,
  scratch buffers, and tensor indexing reject zero/negative lengths, mismatches,
  NaN/Inf where unsupported, and overflowed products with named errors;
- `maki/gpu.f` stops being a `/tmp/saxpy.cubin` side path: global `G-*` helpers
  become package-scoped MAKI adapters over generic `PTX:DEVICE-*` /
  `PTX:BENCH-*`, private temp roots, generic kernel handles, and named
  fail-closed errors;
- parameter table and optimizer state layout for AdamW, including decoupled
  weight decay, no-decay groups for norm/bias parameters when those parameters
  exist, nonzero-epsilon behavior, missing-state rejection, and CPU/device
  one-step parity;
- graph representation for generic model ops, not just GPT. The graph IR owns
  node ids, op schema, inputs/outputs, topo order, dtype/shape/layout, op-instance
  ids for saved values, parameter ownership, alias/mutation rules, and lowering
  metadata;
- lowering from high-level ops to PTX plans and fused regions;
- cross-op buffer lifetime, cleanup, and launch parameter plumbing;
- deterministic CPU reference and device execution path;
- native Maki graph construction for the GPT capstone before ONNX import is used;
- Transformer graph shape contract covering sequence/context/vocab/d_model,
  head_dim, QKV, projections, residuals, LayerNorm, logits, targets, scratch, and
  overflowed byte products;
- ONNX import work changes `ONNX-LOWER` from a string table to graph node
  construction. Supported `MatMul`/`Gemm` attributes are explicit; unsupported
  transpose/broadcast/alpha/beta cases reject; valid nodes route to GEMM planning
  plus epilogue fusion, never to SAXPY.

Acceptance:

- a chained device-resident add -> GEMM -> softmax/attention -> optimizer proof
  runs without round-tripping every intermediate through host memory;
- unsupported model features reject with named errors;
- every new Maki module/test is added to the canonical Maki gate in
  `maki/README.md`;
- every new Maki device/capstone test is added to the Orin Maki-device gate in
  `maki/README.md`. `habu-add-maki-orin` first adds the existing `maki/gpu*`,
  `maki/eval-device*`, and `maki/eval-author*` proofs to that canonical command;
  later capstone dots add `maki/gpt*` entries there;
- `FILEMAP.md` and `tools/filemap-lint.f` hardcode required entries for current
  Maki GPU/eval-device files and future public DSL/capstone entry files;
- graph IR tests cover two graph instances, topo-order rejection, alias/mutation
  rejection, saved-value key collisions, parameter/state ownership, and lowering
  from valid graph nodes to PTX plans;
- Maki graph examples satisfy the beautiful DSL contract;
- no Python/host glue is introduced.

### 10. nanoGPT Model DSL

Files: new `maki/gpt*.f` files if needed, plus fixtures under `maki/`.

Purpose: provide the thin capstone-specific vocabulary that assembles a tiny GPT
from generic Maki/PTX blocks.

Required capabilities:

- model definition words for token embedding, learned position, Transformer
  block, causal attention, MLP, logits, CE loss, and AdamW update;
- deterministic toy corpus fixture and next-token target construction;
- explicit vocab length, token-id validation, and repeated-token gradient
  accumulation;
- affine LayerNorm is the default nanoGPT contract: gamma/beta forward,
  parameter gradients, parameter table entries, and AdamW/no-decay policy. If the
  capstone deliberately narrows to affine-free LayerNorm, that simplification must
  be recorded before implementation and the final claim must say so;
- invokes the generic Maki AdamW optimizer graph op and proves one-step parity and
  loss decrease; `maki/gpt*` must not introduce optimizer or device-lowering
  public APIs;
- CPU reference path and device path driven from the same graph/model definition;
- capstone profile matrix showing which regions fused, tiled, used MMA, or stayed
  scalar and why.

Acceptance:

- tiny forward-only GPT block matches CPU first;
- one training step matches CPU gradients and AdamW update within tolerance;
- a short training run shows loss decrease on the deterministic toy corpus;
- token tests cover `-1`, fractional, NaN, max-valid, repeated ids, and
  `id == vocab`;
- optimizer tests distinguish decoupled AdamW from coupled L2 when gradients are
  zero, cover nonzero `eps`, missing optimizer state, shape mismatch, and
  no-decay parameter groups;
- GPT examples satisfy the beautiful DSL contract;
- profile rows explain every memory-bound fusion and compute-bound tiling/MMA
  decision.

## Ordered Implementation Phases

Each phase is reviewable, dot-splittable, and independently verifiable.

| Phase | Outcome | Existing dots to reconcile | Primary verification |
|---|---|---|---|
| 0 | Reviewed `PLAN.md`; other plan files reduced to pointers; existing dots reconciled with YAML `blocks:` front matter | plan-related open dots | Markdown review; `dot-dep-lint`; dot review |
| 1 | Fail-closed PTX device runtime plus explicit target records, target-aware header/ptxas/profile use, 2D/3D bench/profile package | `habu-make-ptx-device-c0eb12a3`, `habu-add-ptx-target-ba119d76`, `habu-add-ptx-public-063a2a93`, device proof dots, grid3/block3 dot | `tools/ptx/*-test.f`, fail-injection, target-coherence tests, Orin hardware gate, PTX lint slices |
| 2 | Generic PTX planner DSL with schema, overflow-safe shape math, roofline/fusion/tile/resource decisions, and bench metadata adapter | `habu-add-ptx-planner-30b93e8c` | planner positives/negatives, FILEMAP/gate wiring |
| 3 | Checked kernel construction DSL plus PTX package migration covers loops, smem, accumulators, M5 uniformity/barriers, resources | `habu-checker-capability-typed-e0c76a02`, `habu-re-express-tiled-9cc4a73a`, `habu-ptx-m5-mask-eb0716f1` | PTX stdlib positives/negatives, package isolation negatives, trust/signature lints |
| 4 | Collective semantics and softmax/loss foundations are device-proven | `habu-fix-ptx-collective-997cfcce`, `habu-ad-softmax-rows-8c9552fb` | row sum, softmax fwd/bwd, CE gradcheck |
| 5 | Fusion DSL v1: generic elementwise regions, private temp roots, named split reasons | `habu-automatic-op-fusion-329aac27` | fused/unfused correctness and byte profile rows |
| 6 | Tiled GEMM, MMA, and autotune blocks are device-correct and profiled | `habu-tiled-gemm-codegen-76075375`, `habu-tensor-core-mma-11f23a94`, `habu-ptx-m9-bench-1393e18c` | GEMM/MMA device tests, GFLOP/s rows, autotune rows |
| 7 | Flash/causal attention forward body and CPU/device causal reference are checked and profiled, without claiming backward before VJP support | `habu-ptx-m11-attention-fa7b0598`, `habu-ptx-m6-perf-6b979497`, `habu-re-express-fused-09d77c22` | causal attention forward golden, mask negatives, future-token sentinels |
| 8 | Generic CPU/checker autograd/VJP blocks, saved-value lowering, and attention/Transformer backward gradchecks land | `habu-autograd-transformer-block-e2d41299`, `habu-autograd-end-to-ee4d918b` | CPU numeric gradchecks, checked generated backward fixtures, attention VJP device gradcheck |
| 9 | Maki graph IR, token/class-id validation, device tensor/runtime, device gradchecks, and generic graph lowering land | `habu-maki-lower-tensor-e6bbca3d`, `habu-maki-training-loop-5cc4a9a5`, `habu-add-maki-orin-7b88fb4b` | chained device-resident graph proof, token/shape negatives, device finite-difference gradchecks, Maki gates |
| 10 | Thin nanoGPT DSL and capstone run on Orin | `habu-small-model-end-f7cc1b39`, AdamW integration dots | forward parity, one-step train parity, loss-decrease run, profile matrix |

Before new dots are created, reconcile stale or missing IDs. Update existing dots
instead of duplicating them where they already own the gap. Closed Adam work is
not reopened as generic optimizer work; remaining AdamW work is decoupled
weight-decay and device/model integration.

Phase 0 dot reconciliation must rewrite or split these named dots with YAML
`blocks:` front matter before implementation begins. The epic dot must either be
non-schedulable or block on every required leaf so `dot ready` cannot select it
while prerequisites remain open:

| Dot | Required blocker coverage |
|---|---|
| `habu-small-model-end-f7cc1b39` | device runtime, planner, M5, collectives, GEMM/MMA, attention, autograd/VJP, Maki lowering, AdamW |
| `habu-ptx-m11-attention-fa7b0598` | device runtime, M5, collectives, GEMM/MMA where used; split forward attention from attention VJP or add autograd/VJP blockers before backward claims |
| `habu-tiled-gemm-codegen-76075375` | device runtime, grid3/block3 bench/profile, planner, checked kernel construction |
| `habu-tensor-core-mma-11f23a94` | target capability, device runtime, planner, checked kernel construction, GEMM shape contract |
| `habu-autograd-transformer-block-e2d41299` | collectives/loss, attention, saved-value lowering, Maki graph contracts |
| `habu-autograd-end-to-ee4d918b` | generic autograd/VJP blocks, Maki device tensor/runtime for device gradchecks |
| `habu-maki-lower-tensor-e6bbca3d` | PTX device runtime, planner, fusion legality, graph IR, token/class validation, GEMM/attention/loss blocks |

Add or update a `dot-dep-lint` rule so prose dependency markers such as `Deps:`,
`Needs:`, or `Blocks:` without matching YAML `blocks:` fail. A dot with one
unrelated YAML blocker is insufficient; marker IDs must either match `blocks:` or
the prose marker must be removed.

## Dot Split Required After Plan Review

Do not create vague umbrella-only work. Split or update dots so every leaf has:
files, root cause/gap, concrete fix, dependencies, verification command, and
YAML `blocks:` front matter for enforced ordering. Prose-only dependencies do
not count.

Required dot groups and current owners:

- `nanoGPT reusable DSL epic` `[dot:habu-nanogpt-reusable-dsl-9e0854a1]`:
  integrates the leaf work and defines final acceptance; its front matter must
  block on every required leaf or be made explicitly non-schedulable.
- `PTX device runtime` `[dot:habu-make-ptx-device-c0eb12a3]`
  `[dot:habu-habu-native-kernel-548b0d4c]`: fail-closed CUDA, ptxas, temp roots,
  cleanup, 2D/3D launch, profile rows.
- `target capabilities` `[dot:habu-add-ptx-target-ba119d76]`
  `[dot:habu-add-ptx-public-063a2a93]`: explicit arch/PTX feature/cap/roof table
  threaded through header emission, ptxas, launch, and profiles, with generic
  public-surface leak tests. Planner threading is accepted by the planner dot once
  `lib/ptx/plan.f` exists.
- `PTX independence lint` `[dot:habu-wire-ptx-independence-87f54e59]`: extend
  `maki-dep-lint` over generic `tools/ptx/` and migrate existing Maki-dependent
  PTX helpers; wire the check into `lint-libs-ptx-tool` in addition to `lint-tools`,
  with a negative fixture proving a `tools/ptx/` `maki/` token fails.
- `dot dependency enforcement` `[dot:habu-reject-prose-dot-c03bf141]`: reject
  prose dependency markers unless every marker ID is represented in YAML
  `blocks:`.
- `PTX planner DSL` `[dot:habu-add-ptx-planner-30b93e8c]`: `lib/ptx/plan.f`,
  planner tests, negative tests, FILEMAP/gate wiring.
- `kernel construction DSL` `[dot:habu-checker-capability-typed-e0c76a02]`
  `[dot:habu-ptx-m5-mask-eb0716f1]`: typed loops, shared memory, accumulators,
  barriers, resources, and PTX package migration for legacy global tile words.
- `collectives and CE` `[dot:habu-fix-ptx-collective-997cfcce]`
  `[dot:habu-ad-softmax-rows-8c9552fb]`
  `[dot:habu-add-logits-domain-a1489686]`: close collective proof, softmax
  fwd/bwd, logits-domain fused CE, device gradcheck.
- `fusion legality` `[dot:habu-automatic-op-fusion-329aac27]`: graph/fusibility
  analysis, barriers, named errors, element fusion first, epilogues/reductions
  after dependencies.
- `GEMM checked body` `[dot:habu-tiled-gemm-codegen-76075375]`
  `[dot:habu-re-express-tiled-9cc4a73a]`: re-express existing GEMM under typed
  loops/shared/acc and shape contracts.
- `MMA fragment DSL` `[dot:habu-tensor-core-mma-11f23a94]`: fragment types, lane
  layout, dtype/alignment gates.
- `autotune shape keys` `[dot:habu-ptx-m9-bench-1393e18c]`: benchmark candidates,
  cache/report selected plans.
- `flash attention checked body` `[dot:habu-ptx-m11-attention-fa7b0598]`
  `[dot:habu-re-express-fused-09d77c22]`: causal masking, online softmax, no
  score HBM materialization.
- `transformer VJPs` `[dot:habu-autograd-transformer-block-e2d41299]`: matmul,
  attention, LayerNorm, GELU, residual, embedding.
- `Maki device tensor/runtime` `[dot:habu-maki-lower-tensor-e6bbca3d]`
  `[dot:habu-add-maki-orin-7b88fb4b]`: device-resident buffers, graph lowering,
  graph IR, token/class validation, launch plumbing, cleanup, filemap ownership,
  and canonical Orin gate ownership.
- `AdamW integration` `[dot:habu-integrate-adamw-in-3687a100]`: decoupled weight
  decay, parameter/state tables, CPU/device parity, lowering, loss-decrease
  acceptance.
- `nanoGPT DSL` `[dot:habu-small-model-end-f7cc1b39]`: tiny GPT graph words,
  deterministic corpus, CPU reference, device run, profile matrix.

## Verification Gates

Focused gates per PTX device/profile change that exist today:

```sh
bin/hb --load tools/ptx/profile-test.f
bin/hb --load tools/ptx/device-support-test.f
bin/hb --load tools/ptx/bench-test.f
bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- lint-tools
bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- lint-libs-ptx
bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- lint-libs-ptx-neg
bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- lint-libs-ptx-tool
```

Phase 1 must add target/profile/device-support tests to the PTX tool slice and
must either rewrite or retire `tools/ptx/ptxas-smoke.f` under the same target and
private-temp-root rules.

Device proof slices on Orin, as applicable, must use private temp roots and the
generic device-support/bench/profile APIs. The current fixed-`/tmp` proofs
(`cuda-load.f`, `cuda-launch.f`, `softmax-gradcheck.f`, `gradcheck.f`,
`ptxas-smoke.f`, and
legacy matmul proof code) do not count until rewritten or replaced:

```sh
bin/hb --load tools/ptx/matmul-device-test.f
bin/hb --load tools/ptx/saxpy-v4-tail-device-test.f
bin/hb --load tools/ptx/scatter-add-gradcheck.f
bin/hb --load tools/ptx/indexed-scatter-gradcheck.f
```

Future phase-owned Orin device gates. These commands are contracts for the owning
phase/dot to create and wire before their claims count; they are not all runnable
on the current tree:

```sh
bin/hb --load tools/ptx/softmax-device-test.f
bin/hb --load tools/ptx/softmax-gradcheck-device-test.f
bin/hb --load tools/ptx/mma-device-test.f
bin/hb --load tools/ptx/attention-device-test.f
bin/hb --load tools/ptx/ce-loss-device-test.f
bin/hb --load tools/ptx/profile-matrix-test.f
```

Maki gates:

- CPU/off-device Maki gate: run the current canonical command in `maki/README.md`;
- Orin Maki-device/capstone gate: `maki/README.md` must own a second canonical
  command for current `maki/gpu*`, `maki/eval-device*`, `maki/eval-author*`, Maki
  device lowering, and future `maki/gpt*` tests;
- off-device behavior is explicit: CPU-only tests pass; Orin-only tests either
  skip with a named SKIP reason outside Orin or fail closed on Orin;
- add every new Maki module/test to the appropriate command;
- do not duplicate a shortened Maki load list in this file.

Commit and merge gates:

- run `tools/typed-local-diff-lint.f` on the `jj diff --git` artifact for any
  Forth change;
- run exact lint slices for touched areas:

```sh
bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- lint-tools
bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- tool-lints
```

- for every touched non-`src`/`lib` Forth file with `TRUST`, `TRUSTED:`,
  `0 set-check`, or new public definitions, run the combined source-list gate:

```sh
bin/hb --load tools/check.f -- --strict-signatures --source-list <ordered owning files...>
```

  Standalone trust/signature/boundary CLIs are supplemental audits, not the
  acceptance gate. Update `TRUSTED.md` rows/tests for each new boundary;
- run `filemap-lint`, including newly hardcoded required entries for public DSL
  files, current Maki GPU/eval-device files, and canonical device/capstone entry
  tests;
- run the full native gate from `docs/bootstrap.md` before moving `master`.

## Goal Coverage Matrix

| User goal or constraint | Plan coverage |
|---|---|
| Generic reusable infrastructure, not nanoGPT-only code | Layering, generic DSL blocks 2A-9, dot groups |
| Beautiful DSL | Block 1 plus concrete DSL snapshots and acceptance in every later DSL |
| nanoGPT-class capstone | Capstone workload, block 10, phase 10 |
| PTX package with device/bench support | Blocks 2A-3, phase 1, verification gates |
| Kernel fusion and memory-vs-compute optimization | Blocks 2, 5, 6, 7 and roofline non-negotiables |
| Generic profiler/benchmarker for any kernel | Block 3 and profile row requirements |
| Generic target/device reuse beyond nanoGPT | Layering, Block 2A target capability records, PTX independence lint |
| Typed checked Habu, packages, require, useful effects | Non-negotiables and gate rules |
| Device proof on Orin before claims | Blocks 3, 6, 7, verification gates |
| Token/class-id correctness for GPT | Capstone semantics, Maki graph lowering, nanoGPT DSL |
| Logits-domain CE and causal attention | Blocks 7-8 plus capstone workload |
| Maki graph lego blocks before GPT | Block 9 graph IR, token/shape validation, phase 9 |
| Dots after review | Dot split section with `blocks:` requirement and epic non-schedulability |
| Green master only | Non-negotiables and commit/merge gates |

## Review Protocol

Before dot creation or implementation, review this `PLAN.md` adversarially:

- completeness and goal coverage against reusable DSL blocks and the nanoGPT
  capstone;
- specificity and realism against the current codebase;
- edge cases: shapes, overflow, dtype tolerance, device errors, unsupported ops,
  resource cleanup, benchmark flakiness;
- hidden dependencies: load order, packages, FILEMAP/gate wiring, trust rows,
  Maki one-way dependency, existing dot edges.

Accepted findings must be folded into this file before dots are created.

## Completion Definition

The goal is not complete until current evidence proves all of these:

- `PLAN.md` has been reviewed and split into dots with dependency edges.
- Generic reusable PTX/Maki DSL blocks are implemented before the nanoGPT-specific
  assembly depends on them.
- All leaf dots needed for the tiny GPT path are implemented or explicitly closed
  with evidence that stronger completed work covers the same requirement.
- The tiny GPT model runs on Orin end to end: forward, backward, update, loss
  decrease.
- CPU/device parity and gradcheck evidence exist for every participating kernel
  or accepted tolerance boundary.
- Profile rows explain every memory-bound fusion and compute-bound tiling/MMA
  decision.
- Required focused, device, Maki/PTX, lint, and full gates pass on the exact tree
  intended for master.
