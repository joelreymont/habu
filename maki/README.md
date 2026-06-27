# Maki — an ML framework on Habu + Habu-PTX

Maki is the ML *framework* layer: tensor/array types, autograd orchestration,
optimizers, ONNX import, the training/eval loop, and the LLM-target eval harness.
It is built **on** Habu and its checked PTX kernel backend. See the root
[`PLAN.md`](../PLAN.md) for the full design and the dot chain.

## Boundary (BLOCKING)

- **One-way dependency: `maki → habu`, never the reverse.** Maki loads Habu
  libraries (`lib/*.f`, the PTX vocabulary `lib/ptx*.f`); Habu core, the gate, and
  the fixpoint must never reference `maki/`. (A dependency lint is owed —
  dot `habu-add-maki-one`; until it lands, the seam is held by review + the gate's
  explicit allow-lists.)
- **Fenced out of the trust root.** `maki/` is **not** in `TRUSTED.md`, **not** in
  the byte-for-byte fixpoint, and **not** a native-gate dependency. It is
  application Forth run by `bin/hb`, naturally outside the self-hosting fixpoint.
- **Still strictly checked/typed Habu.** The fence excludes maki from the *trust
  manifest*, not from the *checker*. Maki definitions use real typed effects and
  are verified through maki's own `bin/hb --load` path.
- **Extractable.** Treat the habu↔maki seam as an API even in-repo; extract `maki/`
  to its own repo when the Habu-PTX API stabilizes.

## Maki gate (its own, outside the Habu trust root)

Maki runs through its own `bin/hb --load` path — the Habu libraries it needs, then
the maki components and their tests (each test runs on load, printing `test: ok`):

```
bin/hb --load lib/errors.f lib/string.f lib/test.f \
  lib/ptx/header.f lib/ptx/tile.f lib/ptx/collective.f \
  maki/tensor.f      maki/tensor-test.f \
  maki/optim.f       maki/optim-test.f \
  maki/loss.f        maki/loss-test.f \
  maki/autograd.f    maki/autograd-test.f \
  maki/train.f       maki/train-test.f \
  maki/onnx.f        maki/onnx-test.f \
  maki/eval.f        maki/eval-test.f
```

## Components (v0, all runnable + tested)

`tensor` (shape/dtype) · `autograd` (VJP rules + numeric gradcheck) · `optim`
(SGD family) · `loss` (MSE/L1) · `train` (a loop that converges) · `onnx`
(fail-closed op lowering) · `eval` (checker-as-judge + pass@k). The element/scalar
rules lower onto the checked Habu-PTX kernels once codegen lands.

## On the Orin GPU (validated on hardware)

The element/scalar rules now lower onto the checked Habu-PTX kernels and run on
the device, each verified correct-vs-CPU on the Orin:

- **A maki tensor op (AXPY)** runs on the GPU (`maki/gpu.f` + `gpu-test.f`).
- **A maki SGD step** `w -= lr·g` lowers onto the checked SAXPY kernel and matches
  CPU `T-SGD!` bit-for-bit (`maki/gpu-sgd-test.f`).
- **maki trains on the GPU** — 3 SGD epochs of `y=w·x` / MSE run the optimizer on
  the device; weights converge `[2,4,6,8]→[1.125,1.375,1.625,1.875]` and the loss
  falls 84→1.3125 (`maki/gpu-train.f` + `gpu-train-test.f`).
- **A checked `SOFTMAX-ROWS` kernel** emits its own PTX (block reduction via
  shared-mem + bar.sync) and runs within 1 ULP of the CPU golden
  (`tools/ptx/softmax-cg.f` → `tools/ptx/softmax-launch.f`).
- **GB/s:** the SAXPY kernel sustains ~42.9 GB/s on the Orin (`tools/ptx/bandwidth.f`).

Still owed (no "better target" thesis claim until these land): the auto-derived
`SOFTMAX-ROWS-BWD` device gradcheck (needs the reverse-pass AD: fan-out/nonlinear
cotangent threading + SAVED-* buffers — dot `habu-ad-reverse-pass`), and the full
comparative eval matrix (pass@k / tokens-to-green / repair rounds vs Triton — the
external LLM + Triton arm).

## Status

See [`STATUS.md`](STATUS.md). Active work is in the root dot chain (`maki-*` /
`habu-maki-*` dots).
