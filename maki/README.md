# Maki — an ML framework on Habu + Habu-PTX

Maki is the ML *framework* layer: tensor/array types, autograd orchestration,
optimizers, ONNX import, the training/eval loop, and the LLM-target eval harness.
It is built **on** Habu and its checked PTX kernel backend. See the root
[`PLAN.md`](../PLAN.md) for the full design and the dot chain.

## Load Maki (one file)

```
bin/hb --load maki/maki.f
```

[`maki/maki.f`](maki.f) is the one-file entry point. It `require`s the host
framework (model authoring, training, losses, optimizers, ONNX import, the
checker-as-judge eval core) and re-exports a **curated top-level `MAKI:`
surface** via the package `EXPORT` capability (`docs/forth.md` "Packages"). After
one load you have two access modes:

- call the curated workflow words as `MAKI:WORD` — the losses `MAKI:MSE` /
  `MAKI:NLL` / `MAKI:HUBER` / `MAKI:L1` / `MAKI:MAHALANOBIS` / `MAKI:CE` (with
  their gradients and `TT-` tensor forms), the optimizer steps `MAKI:SGD` /
  `MAKI:SGD-MOM` / `MAKI:ADAM` / `MAKI:WEIGHT-DECAY`, model import `MAKI:IMPORT` /
  `MAKI:IMPORT-FILE`, and the eval core `MAKI:CHECK-PASSES?` / `MAKI:PASS@1?`; or
- drill into a subsystem package directly — `LOSS:`, `OPTIM:`, `ONNX:`, `EVAL:`,
  `PLAN:` (the model-builder DSL), `TENSOR:`, `REPORT:` — for that package's full
  surface.

`EXPORT` is tail-preserving (one body, two names, zero runtime cost), so the
curated set is exactly the model-authoring / train / eval words whose bare tail
reads unambiguously at the top level and does not collide with an existing
`MAKI:` word. Stem-clarified vocabularies (`PLAN:LINEAR`, `REPORT:RENDER-HUMAN`),
the tensor-value store internals, ONNX proto/encode, and the device/GPU +
device-golden grading layer (`GPU:*`, `EVAL:GRADE-*`) stay drill-in — see the
curation criterion in the [`maki/maki.f`](maki.f) header.
[`maki/maki-test.f`](maki-test.f) is the consumer fixture: it loads only
`maki/maki.f` and proves each `MAKI:` alias is the identical word as its
subsystem name.

## Boundary (BLOCKING)

- **One-way dependency: `maki → habu`, never the reverse.** Maki loads Habu
  libraries (`lib/*.f`, the PTX vocabulary `lib/ptx*.f`); Habu core, the gate, and
  the fixpoint must never reference `maki/`. Enforced by `tools/maki-dep-lint.f`
  (token-scans `src/`, `lib/`, `test/` for any forbidden `maki/` path reference and
  throws on a hit), wired into the native gate lint slice as `maki-dep-lint` +
  `maki-dep-lint-fixtures`.
- **Layered package namespaces (the runtime package feature, `docs/forth.md` "Packages").**
  `package NAME` / `public` / `private` / `end-package` gives each module a real wordlist
  namespace; a bare `WORD` reference from habu core does not resolve, enforcing the one-way
  seam at the *dictionary* level. The layering:
  - **`MAKI` is the public interface** — model import, training, eval, and CPU reference
    kernels live in reopened `package MAKI` blocks and export the intended API as
    `MAKI:WORD`. Implementation helpers stay private to the package.
  - Multi-file `MAKI` modules compose with `require`, not repeated raw includes. Each file
    declares its dependencies, reopens `package MAKI`, defines private helpers by default,
    and switches to `public` only for the module boundary. Tests require the module they
    exercise and reopen `package MAKI` for bare test calls.
  - Specialized internal vocabularies get their own package when they are a separate
    language surface. `FUSION` is the worked example; its renderer body is `private` and
    only the driver-level API is exported. The PTX kernel vocabulary (`lib/ptx`) is the
    canonical future internal module — a future `package PTX`.
  - Cross-package calls use the qualified `PKG:WORD` form (or reopen the package for bare
    names). Cross-cutting error constants keep the global `E-MK-*` form.
- **Fenced out of the trust root.** `maki/` is **not** in `TRUSTED.md`, **not** in
  the byte-for-byte fixpoint, and **not** a native-gate dependency. It is
  application Forth run by `bin/hb`, naturally outside the self-hosting fixpoint.
- **Still strictly checked/typed Habu.** The fence excludes maki from the *trust
  manifest*, not from the *checker*. Maki definitions use real typed effects and
  are verified through maki's own `bin/hb --load` path.
- **No host glue in `maki/`.** Maki implementation, tests, tooling, and reducers
  are checked Habu. Do not commit `.py` or other host-language helpers under
  `maki/`; external Python/Triton references belong in docs only.
- **Extractable.** Treat the habu↔maki seam as an API even in-repo; extract `maki/`
  to its own repo when the Habu-PTX API stabilizes.

## Maki Test Suite (outside the Habu trust root)

Maki runs through its own checked test-suite entry point. The suite lists only
test files; each test declares its own `require` dependencies. The runner prints
the group, test name, pass/fail status, and elapsed time:

```
bin/hb --load maki/test.f
```

`maki/device-smoke.f` is the device-FFI canary: a stale `bin/hb` (predating the
AAPCS64 FFI-ABI primitives) fails to load it, so the gate stops early at the FFI
layer instead of erroring cryptically deep in a device tool. `maki/device-smoke.f`
then runs a live `cuInit`/`cuDeviceGet` smoke on the Orin (SKIPPED off-device).

## Components (v0, all runnable + tested)

`tensor` (shape/dtype) · `autograd` (VJP rules + numeric gradcheck) · `optim`
(SGD family + Adam) · `loss` (MSE/L1) · `train` (a loop that converges) · `onnx`
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
  candidate by `certify AND run-correct`: `EVAL:GRADE-CANDIDATE` certifies, spawns
  `bin/hb` to emit the candidate's PTX, ptxas-assembles, runs on the Orin, and
  compares the task golden. A SAXPY that computes `x+y` (forgetting the scale)
  *certifies* yet is graded TYPED-WRONG by the device gate, while the correct one
  is GREEN — so device-gated pass@k (1/3) is stricter than certification pass@k
  (2/3). `maki/eval-device-sm.f` adds the **softmax** task: the type-identical
  `B-`/`B/` confusion (subtract vs divide) certifies but is caught as TYPED-WRONG,
  proving the gate works for block-reduction kernels, not just SAXPY.
- **Unified authoring grader** — `maki/eval-author.f` `GRADE-AUTHOR ( a u task -- verdict )`
  dispatches a candidate to its task's device-golden grader (`TASK-SAXPY` → `EVAL:GRADE-CANDIDATE`,
  `TASK-SOFTMAX` → `EVAL:GRADE-SM`), failing closed on an unknown task. This replaces the
  throwaway `/tmp` grade scripts so the model-driven authoring matrix is reproducible from
  the committed tree. Orin-only device suite (run alongside `eval-device-test.f` /
  `eval-device-sm-test.f` / `eval-author-test.f`):

  ```
  bin/hb --load lib/errors.f lib/string.f lib/float.f lib/fmt.f lib/test.f \
    lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/ffi.f \
    src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/header.f lib/ptx/launch.f lib/ptx/cg-collective.f lib/ptx/tile.f \
    lib/ptx/collective.f maki/eval.f maki/eval-device.f maki/eval-device-sm.f \
    maki/eval-author.f maki/eval-author-test.f
  ```

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

- **Checker ablation (measured)** — `maki/eval-compare.f` now scores each SAXPY
  candidate twice: the checked arm rejects 5/6 bugs before GPU execution, while the
  no-checker arm emits, assembles, and device-runs all 9 candidates. On the Orin
  fixture, no-checker catches 0 bugs before execution; all 6 buggy candidates reach
  the device golden and return wrong output, while the 3 correct candidates pass.

The Habu-PTX-side metric machinery — checker-as-judge pass@k, device-golden
grading (task-general), repair-rounds, tokens-to-green, GB/s, and the real-Triton
eval matrix snapshot — has run on hardware. Remaining follow-up work is the durable
in-tree device-grader cleanup and live sampled matrix expansion.

## Status

See [`STATUS.md`](STATUS.md). Active work is in the root dot chain (`maki-*` /
`habu-maki-*` dots).
