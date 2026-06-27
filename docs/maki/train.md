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
  `habu-maki-training-loop` (gradient checkpointing).
