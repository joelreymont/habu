# Maki training loop

`maki/train.f`: the forward → loss → backward → optimizer cycle that **provably
reduces the loss**, at both scalar and tensor scale, pure checked Habu.

## What it does (verified)
- A 1-weight model: loss 36 → ~0.
- A whole weight **tensor** `[0,0]` trains to the optimum `[3,4]` with tensor loss
  180 → ~0 over 50 SGD epochs — composing the tensor arrays (`array.f`) + gradient
  (`autograd.f`) + loss (`loss.f`) + optimizer (`optim.f`).

## On the GPU (validated)
A maki SGD step `w -= lr·g` lowers onto the checked SAXPY kernel and matches the CPU
`T-SGD!` bit-for-bit; 3 SGD epochs of `y=w·x` / MSE run the optimizer on the Orin and
converge (`maki/gpu-train.f`). See `docs/eval-triton.md`.

## Design intent + roadmap
- **One training step = a fused kernel graph.** forward (GEMM + fused bias/act epilogue)
  → loss → backward (transposed matmuls + reductions) → optimizer apply (fused
  elementwise). Each stage picks its lever by the roofline (`docs/kernel-principles.md`):
  GEMMs compute-bound (tiling + tensor-core MMA), elementwise/norm/optimizer
  memory-bound (fuse).
- **Capstone:** train+eval a small real model (MLP / attention block) end-to-end on the
  Orin matching the CPU reference — dotted `habu-small-model-end` +
  `habu-maki-training-loop` (gradient checkpointing: policy below, host
  implementation `maki/checkpoint.f`).

## Gradient checkpointing (policy — this section IS the spec)

`maki/checkpoint.f` implements exactly this section; every policy question cites it.

### Terms (anchored to the fusion planner)

- **Segment** = one fusion region of the CAPTURED FORWARD IR (`maki/fusion-plan.f`
  `FP-BUILD`), snapshotted before `BW-BUILD` appends the backward region.
- **Boundary activation** = a forward node the planner materializes (`MIR-MAT@`
  set after `FP-MARK`): region output, multi-use producer, materialize/gathered
  movement, or the model output. Cross-segment edges always run through a
  boundary — that is the planner's own invariant, and the policy leans on it.
- **Interior** = a forward node with the flag clear. Under the device plan it
  exists only inside its region's fused kernel; on the host it is a buffer the
  fused plan would never write to memory.

### The policy (v1)

- **SAVED** across the forward→backward cut: segment-boundary activations only
  (model inputs are caller-owned bindings and always available).
- **RECOMPUTED**: segment interiors, on demand during the backward walk, at
  segment granularity, into ONE shared scratch window that all segments overlay
  — at most one segment's interior is live at a time.
- **Memory bound**: live forward-activation buffers ≤ `B + max_s I_s`
  (`B` = boundary count, `I_s` = segment `s`'s interior count); saved across
  the cut = `B`. The full-materialization baseline (`EX-RUN`) keeps all `N_fwd`
  forward buffers, so peak live buffers for a whole step are
  `B + max_s I_s + N_bwd` checkpointed vs `N_fwd + N_bwd` full.
- **Recompute count**: a segment's interior is re-run at most once per backward
  pass under `BW-BUILD`'s reverse emission order; rematerialization is on-demand
  and idempotent (a segment's interiors depend only on saved boundaries, model
  inputs, and earlier same-segment interiors), so any need order stays correct —
  order affects only the count. A chain whose interiors all sit in one segment
  (the committed MLP and attention models) incurs ZERO recompute: the scratch
  window still holds that segment after the forward pass. Recompute begins when
  ≥ 2 segments carry interiors.

### Justification against the `maki/saved.f` flop-byte model

- A **boundary** SAVE costs only the backward read: the boundary write is
  already in the forward plan (the planner materializes it regardless), so the
  marginal cost is `bytes(t)` — half of `SAVED-SAVE-COST`, cheaper than any
  recompute. Boundaries are therefore always saved.
- Saving an **interior** would ADD a boundary write the fusion plan eliminated
  and split the fused kernel — its true cost is a plan change, not
  `2*bytes(t)`. Recomputing it re-runs only its own segment's interior:
  segment flops × the flop-byte ratio (`SAVED-FBR`) plus re-reads of the
  segment's already-saved boundary inputs. One-contraction-per-region plus
  boundaries-at-region-edges bound recompute depth to a single segment — never
  a transitive chain.
- **Matmul-floor mapping**: `saved.f`'s v1 floor (matmul/linear operands always
  saved) holds structurally for cross-region operands — no class emits into a
  contraction, so a contraction's operands are region inputs = saved boundaries
  or model inputs. The one divergence: a contraction that is itself interior
  (matmul + fused epilogue, e.g. attention's `q@kt` under `SCALE`) is re-run
  with its segment, where the per-tensor SV model would save its output. v1
  keeps uniform segment recompute (the memory bound stays exact and the host
  proof stays simple); a calibrated per-segment save-exemption at segment
  granularity is roadmap, not implemented.

### Correctness obligation (assertable)

Host execution is deterministic: re-running a segment's interior over the same
saved boundary and input values writes bit-identical cells. Checkpointed
backward MUST therefore produce bit-identical (exact `f=`) losses, gradients,
and rematerialized interiors vs the full-materialization path — any tolerance
is a bug. `maki/checkpoint-test.f` asserts exact equality on the committed MLP
and attention models plus a 3-linear MLP whose interiors span two segments
(the case where the scratch overlay actually destroys and recomputes values).

### v1 domain (fail-closed)

- Segments must be contiguous node-index intervals (true for chain models);
  otherwise `E-CK-INTERVAL` — the shared scratch overlay relies on it during
  the forward pass.
- One backward node may read interiors of at most one segment. `BW-BUILD`
  guarantees this (an adjoint reads only its forward node's operands and
  output — same-segment interiors or boundaries); `E-CK-CROSS` guards
  hand-built IRs.
