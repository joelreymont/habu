# Maki — Status

Maki is the ML framework layer on Habu + Habu-PTX. This is maki's own status doc,
kept outside the Habu trust root and self-check. The root stale-status lint skips
`maki/` because maki owns its own status once it is extracted from the Habu trust
root; root Habu self-check counts still belong only in root `STATUS.md`.

## Built

- **Tensor-scale execution** (`maki/array.f`, `maki/array-test.f`) — real float
  tensors (contiguous cell buffers) with whole-tensor ops: fill, sum (reduction),
  elementwise add, and an **in-place tensor SGD step** (`w -= lr*g` over the whole
  array). The optimizer runs at TENSOR scale on the host now — the same ops the
  Habu-PTX kernels run on device. Runnable.
- **Tensor shape + dtype metadata** (`maki/tensor.f`, `maki/tensor-test.f`) — the
  v0 tensor type foundation: 2D shape arithmetic (element count, broadcast
  compatibility/result) and the sm_87 dtype set (f32 / f16 / bf16 / u32 / i32)
  with byte sizes. Pure checked Habu, runnable.
- **Optimizers** (`maki/optim.f`, `maki/optim-test.f`, `maki/optim-tensor.f`) —
  SGD, SGD+momentum, L2 weight decay, and bias-corrected Adam as float update
  rules (the per-weight math), plus tensor Adam apply over parameter/gradient/
  moment buffers. Runnable.
- **Losses** (`maki/loss.f`, `maki/loss-test.f`) — MSE + its gradient, and L1
  (the per-element rule). Runnable.
- **Autograd orchestration** (`maki/autograd.f`, `maki/autograd-test.f`) — tensor
  ops (ADD/MUL/RELU) paired with their VJP (backward) rules, at the element level,
  **with NUMERIC gradient verification**: each analytic VJP is checked against the
  central finite difference. This is the gradcheck the type system cannot give —
  the strongest form of the verified-gradient thesis, demonstrated numerically.

- **Training loop** (`maki/train.f`, `maki/train-test.f`) — the forward → loss →
  backward → optimizer cycle that **provably reduces the loss**, at both scalar and
  **TENSOR** scale: a 1-weight model trains loss 36→~0, and a whole weight TENSOR
  [0,0] trains to the optimum [3,4] with tensor loss 180→~0 over 50 SGD epochs
  (composing the tensor arrays + gradient + loss + optim). Runnable on the host.

- **ONNX import** (`maki/onnx.f`, `maki/onnx-test.f`) — the op-coverage lowering
  table (Add/Mul/Relu/Softmax/Gemm → maki/Habu-PTX entries) with a **fail-closed**
  policy: an unsupported op is rejected, never silently approximated. Runnable.
- **Eval harness core** (`maki/eval.f`, `maki/eval-test.f`) — the thesis's judge:
  the CHECKER scores each candidate kernel (certify = pass), with pass@1/pass@k
  tallying. The model-generation + repair arm is external; this is the correctness
  gate it is scored against. Runnable.

The optimizer/loss/autograd element rules apply per-weight. Current checked
device proofs cover selected lowered kernels and GPU SGD demos; a general tensor
handle/lowering API that maps arbitrary Maki tensor ops onto Habu-PTX remains
future work.

- **On device + eval matrix vs real Triton (DONE 2026-06-27).** Checked kernels
  emit PTX, assemble (`ptxas -arch=sm_87`), and run correct-vs-CPU on the Orin
  (SAXPY, SOFTMAX-ROWS within 1 ULP; maki trains 3 SGD epochs on the GPU; the
  auto-derived SOFTMAX-ROWS-BWD passes a device finite-difference gradcheck). The
  eval matrix now compares **checked Habu-PTX vs real Triton 3.5.1 + torch 2.9.1+cu126
  run on this Orin** (no reflash; see `../docs/eval-triton.md`): both catch name/type
  errors before running (Habu-PTX at author time, Triton at compile), but the
  stack-discipline class (missing store, wrong arity) is caught at **author time** by
  Habu-PTX's checker with zero GPU and only at **runtime** by Triton (3/5 battery bugs
  slipped); bandwidth scalar Habu-PTX 42.5 GB/s vs Triton 63 GB/s, with checked
  v4 Habu-PTX now reaching ~63 GB/s parity after the codegen vectorization fix. A
  **model-driven pass@k** (independent Claude subagents authoring kernels, k=5/task/
  target, graded through each full device loop) adds: SAXPY 5/5 both; softmax Triton
  5/5, Habu 3/5 → 5/5 after diagnostic-guided repair (1–2 rounds) — every Habu failure
  an author-time static reject with a located diagnostic. The earned claim: a checked
  target shifts the stack-discipline error class left to author time at competitive
  bandwidth — NOT "faster than Triton" and NOT a higher first-try rate.

## Next (see root PLAN.md + dots)

- Tensor handle over a Habu-PTX `matrix`/`span` (storage + shape + dtype together).
- Autograd orchestration: the tensor-op VJP table lowering onto the Habu primitive
  VJP table; the user-facing define-forward → checked-backward API.
- General tensor-to-PTX lowering for optimizer/loss/autograd rules; ONNX import;
  training/eval loop; eval harness.

## Underneath (Habu-PTX, in `lib/`)

The checked kernel vocabulary maki builds on: M4 tile ops (`lib/ptx/tile.f`,
checked SAXPY), M6 collectives + softmax (`lib/ptx/collective.f`, checked
SOFTMAX-ROWS), and the AD primitives + verified-gradient kernel
(`lib/ptx/autograd-test.f`, checked SOFTMAX-ROWS-BWD).
