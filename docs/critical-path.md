# Critical path — tiny transformer trained end-to-end on the Orin

The shortest route from today's working pieces to a **small GPT-shaped model that trains
end-to-end on the Orin** — forward, autodiff backward, optimizer step — with final loss
matching a CPU reference within dtype-matched tolerance. This file orders the existing
dots into that path and names the one genuine gap.

Honest framing (`docs/ptx.md`): the win is *checked source → earlier, located failures*
for the stack-discipline bug class, not beating Triton on raw FLOP/s. Background:
`docs/kernel-principles.md` (roofline + the three bounds), `docs/autograd.md` (AD as a
syntactic reversal), `docs/ptx-sketch.md` (the v0 type system).

Dot dependency convention (from existing dots): a dot's `blocks:` list names its
**prerequisites** — the dots that must land first. `tools/dot-dep-lint.f` checks that
every referenced ID exists.

## Where it stands

The PTX backend already runs end-to-end on the Orin (sm_87, ptxas 12.6): a checked SAXPY
lowers to PTX, assembles to cubin, launches, and matches the CPU golden value
(`tools/ptx/cuda-launch.f`). The reverse-mode AD transform exists (`lib/ptx/ad.f`) for
linear plus unary-nonlinear (`EXP.`/`BLOCK-MAX`) plus binary-nonlinear (`*.`/`B-`) ops,
with algebraic-simplify and save-vs-recompute. Maki has tensor types, SGD, numeric
gradcheck, and a training loop that converges at tensor scale. The open frontier is the
compute roof (tensor-core MMA) and the fusion / IR-opt layers.

## The ordered path

Each rung depends on the rungs above it.

| # | Step | Dots | State |
|---|------|------|-------|
| 1 | Barrier-safe grid model + shared-mem tile type | `habu-gemm-codegen-needs-0735e3ad`, `habu-checker-capability-typed-e0c76a02`, `habu-checker-capability-b-cb3b5ec1` | open |
| 2 | Tiled GEMM as a checked `KERNEL:` body | `habu-tiled-gemm-codegen-76075375`, `habu-re-express-tiled-9cc4a73a` | open |
| 3 | Tensor-core MMA codegen (the compute roof) | `habu-tensor-core-mma-11f23a94` | open |
| 4 | Parallel softmax + fused attention `KERNEL:` | `habu-ptx-m6-perf-6b979497`, `habu-fix-ptx-collective-997cfcce`, `habu-re-express-fused-09d77c22`, `habu-ptx-m11-attention-fa7b0598` | open/active |
| 5 | Device gradcheck gate + scatter-add + transformer-block VJPs | `habu-ptx-ad-device-2b511851`, `habu-make-ptx-device-c0eb12a3`, `habu-ad-scatter-add-dc9a3184`, `habu-autograd-transformer-block-e2d41299` | open |
| 6 | Cross-entropy loss kernel | `habu-ce-loss-kernel` (NEW) | proposed |
| 7 | Adam + training loop | `habu-maki-adam-optimizer-de0b7af0`, `habu-maki-training-loop-5cc4a9a5` | open |
| 8 | Assemble the Transformer; small model end-to-end | `habu-small-model-end-f7cc1b39` | open |

Cross-cutting enabler under steps 2–3: `habu-ptx-ir-opt-b90390f0` (PTX IR + fold, DCE,
CSE, peephole) — unblocks fusion and MMA scheduling.

RoPE forward/backward is small (orthogonal rotation; adjoint is rotation by the negative
angle) and is currently folded into step 8 (model assembly); split it into its own dot if
it grows.

## The one gap — cross-entropy loss kernel

No loss kernel exists yet. Proposed dot (`.dots/`):

```yaml
---
title: Fused softmax-cross-entropy loss kernel (forward + p-minus-onehot backward)
status: open
priority: 2
issue-type: task
created-at: "2026-06-29T16:00:00.000000+02:00"
blocks:
  - habu-fix-ptx-collective-997cfcce
  - habu-ptx-ad-device-2b511851
---
```

GAP: the training loop needs the next-token loss and its gradient. Build a checked
one-block-per-row KERNEL: over the vocab axis. Forward computes the numerically stable
loss L = logsumexp(z) - z_y via BLOCK-MAX then a streaming BLOCK-SUM of exp(z - m),
reusing the SOFTMAX-ROWS collective path (lib/ptx/collective.f, tools/ptx/softmax-cg.f);
no explicit division, no overflow. Backward is the closed form dz = p - onehot(y): the
softmax minus the one-hot target, the seed cotangent for the whole transformer backward
(the softmax Jacobian and the log reciprocal cancel). Emit the backward via the AD pass,
register the p-minus-onehot adjoint in the VJP table, and lock it with the device
finite-difference gradcheck (habu-ptx-ad-device). MEMORY: the logits (batch x seq x vocab)
are often the largest activation, so fuse and never round-trip p to HBM; read z once,
write the scalar loss and dz directly. VERIFY: device-correct vs an FP32 reference within
tolerance; gradcheck passes; certifies as checked Habu.

## The umbrella — proposed epic

```yaml
---
title: "EPIC: critical path - tiny transformer trained end-to-end on the Orin"
status: open
priority: 2
issue-type: task
created-at: "2026-06-29T16:00:00.000000+02:00"
blocks:
  - habu-small-model-end-f7cc1b39
  - habu-maki-training-loop-5cc4a9a5
  - habu-maki-adam-optimizer-de0b7af0
  - habu-ce-loss-kernel
  - habu-autograd-transformer-block-e2d41299
  - habu-tensor-core-mma-11f23a94
  - habu-re-express-fused-09d77c22
---
```

VISION: a small model trains end-to-end on the Orin, loss decreasing, gradients and final
loss matching a CPU reference within tolerance; every kernel certifies as checked Habu and
every backward is gradchecked. The body lists the ordered eight-step path above; the
`blocks:` edges name the leaf dots it integrates.

## Wiring it in

Steps 1–5, 7, 8 are already dotted — do not recreate them. Only the cross-entropy leaf and
the umbrella are new. To wire the full chain, add each rung's prerequisites to the
dependent dot's `blocks:` list, then run:

```
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/vector.f lib/fs.f \
  tools/lint/text.f tools/lint/intern.f tools/dot-dep-lint-core.f tools/dot-dep-lint.f
```

Editing existing dots and committing is feature work — do it on a branch under the commit
gate, never directly on `master`.
