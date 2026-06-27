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
bin/hb --load lib/errors.f lib/string.f lib/float.f lib/fmt.f lib/test.f \
  src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/cg-collective.f \
  lib/ptx/header.f lib/ptx/tile.f lib/ptx/collective.f \
  maki/array.f       maki/array-test.f \
  maki/tensor.f      maki/tensor-test.f \
  maki/optim.f       maki/optim-test.f \
  maki/loss.f        maki/loss-test.f \
  maki/autograd.f    maki/autograd-test.f \
  maki/train.f       maki/train-test.f \
  maki/onnx.f        maki/onnx-test.f \
  maki/eval.f        maki/eval-test.f \
  maki/eval-fixture.f maki/eval-repair.f
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
- **Auto-derived softmax gradient passes a device gradcheck** — a real reverse-mode
  AD engine (`lib/ptx/ad-dag.f`) symbolic-executes the forward into a value-numbered
  DAG and emits `SOFTMAX-ROWS-BWD` (recompute + cotangent pass with fan-out sums);
  on the Orin it matches central finite differences of the forward to <1e-2
  (`tools/ptx/softmax-bwd-cg.f` → `tools/ptx/softmax-gradcheck.f`).
- **GB/s:** the SAXPY kernel sustains ~42.9 GB/s on the Orin (`tools/ptx/bandwidth.f`).

- **Device-golden autograder (task-general)** — `maki/eval-device.f` grades a
  candidate by `certify AND run-correct`: `GRADE-CANDIDATE` certifies, spawns
  `bin/hb` to emit the candidate's PTX, ptxas-assembles, runs on the Orin, and
  compares the task golden. A SAXPY that computes `x+y` (forgetting the scale)
  *certifies* yet is graded TYPED-WRONG by the device gate, while the correct one
  is GREEN — so device-gated pass@k (1/3) is stricter than certification pass@k
  (2/3). `maki/eval-device-sm.f` adds the **softmax** task: the type-identical
  `B-`/`B/` confusion (subtract vs divide) certifies but is caught as TYPED-WRONG,
  proving the gate works for block-reduction kernels, not just SAXPY.

- **Eval matrix vs real Triton (validated on the Orin)** — Triton 3.5.1 +
  torch 2.9.1+cu126 run on this Orin (sm_87), no reflash; full reproduction and the
  side-by-side matrix in [`docs/eval-triton.md`](../docs/eval-triton.md). Real
  Triton JIT-compiles each kernel for sm_87 and runs, so the comparison is
  apples-to-apples. Results:
  - **Error-catch timing (the thesis mechanism):** over the SAXPY error battery,
    **both** targets catch name/type errors before running — Habu-PTX at *author*
    time (static stack-effect check, zero GPU), Triton at *compile* time. The
    distinguishing class is **stack discipline** (missing store, wrong arity, extra
    op): Habu-PTX rejects these at author time with a located diagnostic and **no
    GPU work**; in Triton the analogous kernels **compile clean and are caught only
    at runtime** — 3 of 5 battery bugs slipped to runtime, including a *missing
    store* that silently produced `0.0`. Semantic value bugs (x+y) neither catches
    statically; both need the device-golden run (`maki/eval-device.f`).
  - **Bandwidth:** Triton 63.0 GB/s vs Habu-PTX 42.5 GB/s (N=2²⁰, 200 iters) —
    same order, Triton ~1.5×. The gap is the launch path (Habu-PTX still uses the
    deprecated `cuLaunchGrid`; dotted), not codegen; both are launch/occupancy
    bound well under the Orin's ~200 GB/s peak.
  - **Earned claim:** a checked stack-effect target is a viable Triton replacement
    that **shifts the stack-discipline error class left to author time** — caught
    statically, zero GPU — where Triton finds it only at runtime, at competitive
    bandwidth. **Not** earned: any "faster than Triton" claim (currently ~1.5×
    slower on this microbench) or that the checker catches *semantic* errors.

- **Checker ablation (complementary, internal)** — `maki/eval-compare.f` ablates
  the one variable in isolation: Habu-PTX **with vs without** its own static
  checker. Over a 9-candidate SAXPY fixture (3 correct, 5 type/stack, 1 semantic):
  **with** the checker, 5/6 bugs are caught before execution (4 GPU runs);
  **without** any static check, 0/6 are caught before execution and all 9 must run.
  This isolates the checker's contribution from Triton's confounds (different
  compiler, language, launch path).

The Habu-PTX-side metric machinery — checker-as-judge pass@k, device-golden grading
(task-general), repair-rounds, tokens-to-green, GB/s, the checker ablation, and now
the **real-Triton eval matrix** — is built, run on hardware, and committed.

## Status

See [`STATUS.md`](STATUS.md). Active work is in the root dot chain (`maki-*` /
`habu-maki-*` dots).
