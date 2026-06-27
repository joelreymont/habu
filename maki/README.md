# Maki — an ML framework on Habu + Habu-PTX

Maki is the ML *framework* layer: tensor/array types, autograd orchestration,
optimizers, ONNX import, the training/eval loop, and the LLM-target eval harness.
It is built **on** Habu and its checked PTX kernel backend. See the root
[`PLAN.md`](../PLAN.md) for the full design and the dot chain.

## Boundary (BLOCKING)

- **One-way dependency: `maki → habu`, never the reverse.** Maki loads Habu
  libraries (`lib/*.f`, the PTX vocabulary `lib/ptx*.f`); Habu core, the gate, and
  the fixpoint must never reference `maki/`. Enforced by `tools/maki-dep-lint.f`
  (token-scans `src/`, `lib/`, `test/` for any forbidden `maki/` path reference and
  throws on a hit), wired into the native gate lint slice as `maki-dep-lint` +
  `maki-dep-lint-fixtures`.
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
  lib/fs.f lib/fs-mutate.f lib/ffi.f \
  src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/cg-vec.f lib/ptx/cg-collective.f \
  lib/ptx/header.f lib/ptx/tile.f lib/ptx/tile-v4.f lib/ptx/collective.f \
  maki/array.f       maki/array-test.f \
  maki/tensor.f      maki/tensor-test.f \
  maki/optim.f       maki/optim-test.f \
  maki/loss.f        maki/loss-test.f \
  maki/autograd.f    maki/autograd-test.f \
  maki/train.f       maki/train-test.f \
  maki/onnx.f        maki/onnx-test.f \
  maki/eval.f        maki/eval-test.f \
  maki/fusion.f      maki/fusion-test.f \
  maki/eval-fixture.f maki/eval-repair.f \
  maki/device-smoke.f
```

The leading `lib/ffi.f` is the device-FFI canary: a stale `bin/hb` (predating the
AAPCS64 FFI-ABI primitives) fails to load it, so the gate stops early at the FFI
layer instead of erroring cryptically deep in a device tool. `maki/device-smoke.f`
then runs a live `cuInit`/`cuDeviceGet` smoke on the Orin (SKIPPED off-device).

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
- **Checked softmax backward fixture exists** — the reverse-mode AD work can emit
  `SOFTMAX-ROWS-BWD` (recompute + cotangent pass with fan-out sums), and the
  checked fixture covers its stack/type surface. The device finite-difference
  gradcheck gate remains the hard blocker before claiming derivative correctness
  for generated PTX gradients.
- **GB/s:** the scalar SAXPY kernel sustains ~42.9 GB/s on the Orin; the checked
  v4 tile path reaches ~63 GB/s, matching the Triton SAXPY baseline at the
  streaming-memory ceiling.

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
  - **Bandwidth — parity at the hardware ceiling.** Scalar Habu-PTX was 42.5 vs
    Triton 63.0 (RCA: scalar `ld.global.f32` vs Triton's vectorized loads). A checked
    **v4** tile vocab (`lib/ptx/cg-vec.f` + `tile-v4.f`, `ld.global.v4.f32`) lifts
    Habu-PTX to **63 GB/s = Triton**. Proven to be the memory ceiling, not codegen:
    unrolled v4 (K=1–8, up to 8 loads in flight) is flat at 63; occupancy 40×
    saturated (Orin NX, 4 SMs); EMC already maxed. Both targets sit at the DRAM wall,
    so neither can be *faster* on a memory-bound kernel (see `docs/eval-triton.md`).
  - **Model-driven pass@k (live):** independent Claude subagents (k=5/task/target,
    given op semantics only) authored SAXPY and softmax kernels, graded through each
    target's full device loop. SAXPY pass@1 5/5 both; softmax Triton 5/5, Habu-PTX
    3/5 → 5/5 after diagnostic-guided repair (1–2 rounds). Both targets are highly
    reachable; every Habu failure was an **author-time static reject** with a located
    order diagnostic (zero GPU) that drove repair to green, where Triton's analogous
    errors surface only at runtime. (Softmax pass@1 gap is confounded by a prompt
    spec error; see [`docs/eval-triton.md`](../docs/eval-triton.md) for the full
    method + caveats.)
  - **Earned claim:** a checked stack-effect target is a viable Triton replacement
    that (a) **shifts the stack-discipline error class left to author time** — caught
    statically, zero GPU, where Triton finds it only at runtime — and (b) reaches
    **bandwidth parity** at the hardware ceiling. **Not** earned (and not the point):
    being *faster* than Triton on a memory-bound kernel — both saturate DRAM. The
    place a checked target wins on performance is **fusion** (below): moving less
    memory, automatically and provably.

- **Automatic op-fusion — proven correct, for free** (`maki/fusion.f`). This is the
  good bit. In a *concatenative* checked DSL, fusion is **not a compiler pass — it is
  word concatenation.** A maki/ONNX elementwise subgraph lowers to one register-
  resident kernel just by mapping each node to its tile word(s) and concatenating:
  the op-graph `[Mul, Add, Relu]` becomes

  ```
  K ( span<…,extent-n> span<…,extent-n> uniform<f32> -- )
    {: x y a :} x GRID-CTX-V4 {: g :} x g LOAD-V4 a SCALE-V4 y g LOAD-V4 ADD-V4 RELU-V4 y g STORE-V4 ;
  ```

  — every intermediate stays on the (register) stack, so the emitted PTX is **2 loads
  + 1 store, no global round-trips**. The checker types the whole sequence in one
  shot, so the fused effect is **proven correct automatically** (or fails closed). On
  the Orin: `relu(a·x+y)` device-golden PASS, **63 GB/s — parity with hand-fused
  Triton (63.4)**, but produced *automatically* and *verified*, where the Triton
  author must hand-fuse, unchecked and error-prone. Fusion is where the checked
  concatenative target genuinely beats the Triton authoring path: same speed, but the
  composition is the program and the type system proves it.

- **Checker ablation (owed)** — a true no-checker ablation is not yet implemented:
  `maki/eval-compare.f` still goes through `GRADE-CANDIDATE`, so checker-rejected
  candidates are short-circuited rather than run through an unchecked emit/device
  path. Dot `habu-implement-true-no-afa79f63` owns the real ablation before this
  result should be cited.

The Habu-PTX-side metric machinery — checker-as-judge pass@k, device-golden
grading (task-general), repair-rounds, tokens-to-green, GB/s, and the real-Triton
eval matrix snapshot — has run on hardware. Durable in-tree grader and true
no-checker ablation follow-up work remains dotted.

## Status

See [`STATUS.md`](STATUS.md). Active work is in the root dot chain (`maki-*` /
`habu-maki-*` dots).
