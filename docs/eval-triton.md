# Eval matrix — checked Habu-PTX vs real Triton (Orin sm_87)

The thesis is comparative: a *checked* kernel target (Habu-PTX) versus the
unchecked, runtime-only target the field actually uses (Triton). This doc records
the **real-Triton** column — Triton compiled and run on the same Orin — and the
side-by-side matrix against the Habu-PTX column produced by our own tools.

Triton is the external system under comparison; it is Python because Triton is
Python. It is **not** repo automation and nothing in the gate runs it — the
reproduction scripts below are the canonical reference (kept out of the tree so
`host-lint` stays green; the Habu-side reducers `tools/ptx/bandwidth.f`,
`maki/eval/compare.f`, `maki/eval/device.f` are the live Habu column).

## Installing Triton on the Orin (no reflash)

The device is JetPack 6.2.1 / L4T R36.4.4, CUDA driver **12.6** (`cuDriverGetVersion
= 12060`), Python 3.10, sm_87. The GPU driver is BSP-pinned at 12.6 — a CUDA-13
driver would need a destructive JetPack-7 reflash, which we do **not** do. The
matching torch is the trick:

- The pypi *default* torch is `cu130` → needs a ≥13.0 driver → `cuda available: False`.
- The Jetson-native torch index (`pypi.jetson-ai-lab.dev`) is unreachable here.
- But `download.pytorch.org` carries `torch-2.9.1+cu126` aarch64/cp310 — **cu126
  matches the 12.6 driver exactly**, so `torch.cuda.is_available()` is True.

```bash
curl -L -o torch.whl \
  "https://download.pytorch.org/whl/cu126/torch-2.9.1%2Bcu126-cp310-cp310-manylinux_2_28_aarch64.whl"
pip3 install --user ./torch.whl 'triton==3.5.1'   # triton 3.5.1 matches torch 2.9.1
```

Caveat that does **not** block the comparison: this generic SBSA wheel ships
datacenter cubins (sm_80/90), **not** sm_87, so torch's *prebuilt ATen* kernels
fail with `cudaErrorNoKernelImageForDevice`. Triton is unaffected — it
**JIT-compiles each kernel for the live device (sm_87) via ptxas at runtime**. We
therefore only use torch for device allocation + H2D/D2H memcpy (no ATen GPU
kernels), and let Triton compile and run the actual kernel.

## Reproduction scripts

### A real Triton SAXPY on the device (`run_saxpy.py`)

```python
import torch, triton, triton.language as tl
@triton.jit
def saxpy(xp, yp, a, n, BLOCK: tl.constexpr):
    o = tl.program_id(0)*BLOCK + tl.arange(0, BLOCK); m = o < n
    x = tl.load(xp+o, mask=m); y = tl.load(yp+o, mask=m)
    tl.store(yp+o, a*x+y, mask=m)
N, a = 4, 3.0
x = torch.full((N,), 2.0, dtype=torch.float32).to('cuda')   # H2D copy, no kernel
y = torch.zeros(N, dtype=torch.float32).to('cuda')
saxpy[(triton.cdiv(N,256),)](x, y, a, N, BLOCK=256); torch.cuda.synchronize()
print(y.cpu().tolist())   # -> [6.0, 6.0, 6.0, 6.0]  (golden a*x+y = 6.0)
```

### Bandwidth, matched to `tools/ptx/bandwidth.f` (`bench.py`)

N = 2²⁰, BLOCK = 256, ITERS = 200, bytes = ITERS·3·N·4 (read x, read y, write y),
GB/s = bytes / elapsed_ns. Warm up once (JIT compile) before timing.

```python
import torch, triton, triton.language as tl, time
@triton.jit
def saxpy(xp, yp, a, n, BLOCK: tl.constexpr):
    o = tl.program_id(0)*BLOCK + tl.arange(0, BLOCK); m = o < n
    tl.store(yp+o, a*tl.load(xp+o,mask=m)+tl.load(yp+o,mask=m), mask=m)
N, BLOCK, ITERS = 1<<20, 256, 200
x = torch.zeros(N, dtype=torch.float32).to('cuda')
y = torch.zeros(N, dtype=torch.float32).to('cuda')
grid = (triton.cdiv(N, BLOCK),)
saxpy[grid](x, y, 2.0, N, BLOCK=BLOCK); torch.cuda.synchronize()   # warmup
t0 = time.perf_counter_ns()
for _ in range(ITERS): saxpy[grid](x, y, 2.0, N, BLOCK=BLOCK)
torch.cuda.synchronize()
ns = time.perf_counter_ns() - t0
print(f"GB/s={(ITERS*3*N*4)/ns:.1f}")
```

### Error-catch battery (`battery.py`)

For each candidate, classify *when* the bug is caught: COMPILE (jit/compile
raises) · RUNTIME (compiles+runs, output ≠ golden) · GREEN. Golden x=2,y=0,a=3 →
a·x+y = 6.0. The candidates mirror the Habu fixture in `maki/eval/compare.f`.
Kernels: `correct`, `semantic x+y` (forgot the scale), `missing store` (computes,
never `tl.store`), `operands swapped` (a·y+x), `undefined name` (uses `xx`),
`scalar-as-pointer` (`tl.load(a+o)` where `a` is a scalar).

## Results (recorded on the Orin)

```
torch 2.9.1+cu126 | triton 3.5.1 | cuda True | dev Orin (8,7)

Triton  SAXPY  N=1048576 iters=200  GB/s = 63.0
Habu-PTX SAXPY N=1048576 iters=200  GB/s = 42.5   (tools/ptx/bandwidth.f)

Triton error-catch battery (6 candidates):
  correct            -> GREEN
  semantic x+y       -> RUNTIME  (got 2.0)
  missing store      -> RUNTIME  (got 0.0)
  operands swapped   -> RUNTIME  (got 2.0)
  undefined name     -> COMPILE
  scalar-as-pointer  -> COMPILE
  => GREEN=1  COMPILE-caught=2  RUNTIME-caught=3
```

### Internal no-checker Habu ablation

`maki/eval/compare.f` also runs an internal Habu-PTX ablation over the SAXPY
fixture: the checked arm grades through `EVAL:GRADE-CANDIDATE`, while the no-checker
arm emits a throwaway `0 set-check` driver and then attempts ptxas + device golden
for every candidate. On the Orin run, the checked arm caught 5/6 bugs before GPU
execution and ran only the 3 green + 1 semantic-wrong candidates. The no-checker
arm caught 0/6 before execution: all 9 candidates emitted and assembled, the 3
correct candidates passed, and all 6 buggy candidates reached the device golden
as wrong output.

### The matrix — where each target catches the bug

| conceptual bug                         | Habu-PTX (checked)              | Triton (real)        |
|----------------------------------------|--------------------------------|----------------------|
| correct kernel                         | GREEN                          | GREEN                |
| undefined word / name                  | **author** — unknown word      | compile              |
| wrong-kind argument (type misuse)      | **author** — type/role error   | compile              |
| missing store (compute, don't write)   | **author** — stack-effect ≠ decl | **runtime** (0.0)  |
| wrong arity / extra op / underflow     | **author** — stack effect      | **runtime** / compile|
| operands swapped (a·y+x, type-identical) | device — golden              | runtime — golden     |
| semantic x+y (forgot the scale)        | device — golden                | runtime — golden     |

### What the data earns (and what it does not)

- **Both** targets catch name/type errors before running — Habu-PTX at **author
  time** (static stack-effect check, zero GPU), Triton at **compile time**.
- The distinguishing class is **stack discipline** (missing store, wrong arity,
  extra op, underflow). Habu-PTX rejects these at author time with a located
  diagnostic and **no GPU work**; in Triton the analogous kernels **compile clean
  and are caught only at runtime** — 3 of the battery's 5 bugs slipped to runtime,
  including a *missing store* that silently produced `0.0`.
- **Semantic** value bugs (x+y) neither target catches statically; both need a
  golden/device run. (Our device-golden grader `maki/eval/device.f` is that run.)
- **Bandwidth:** scalar Habu-PTX measured 42.5 GB/s vs Triton 63.0 GB/s; RCA found
  the gap was codegen vectorization (`ld.global.f32` scalar loads/stores vs
  Triton's `ld.global.v2`). Implementing a checked **v4** tile vocabulary
  (`lib/ptx/cg-vec.f` + `tile-v4.f`: `ld.global.v4.f32` / `st.global.v4.f32`,
  4 elements/thread) lifts Habu-PTX to **63 GB/s, matching Triton** with
  device-golden correctness and ptx-stdlib certification.

### Beating the ceiling (why 63 is the wall, not a codegen gap)

We then tried to *beat* Triton. The data says 63 GB/s is the streaming-memory
ceiling for this kernel on this device, not a remaining SAXPY codegen limit.

- **More memory-level parallelism is flat.** Unrolled grid-strided v4 with K=1,2,4,8
  chunks/thread (4→32 elements/thread, up to 8 v4 loads in flight) all measure
  **63 GB/s**. If the kernel were MLP- or instruction-bound, more chunks would help.
- **Occupancy is 40× saturated.** The device is an **Orin NX (4 SMs, 6144 threads at
  full occupancy)**; N=2²⁰ launches 262 144 threads. Not occupancy-bound.
- **The EMC clock is already at max** (3199 MHz, verified via bpmp) even at 15W;
  locking clocks (`jetson_clocks`) changed nothing.
- So Habu-PTX-v4 and Triton both sit at ~63 GB/s, roughly the achievable streaming
  bandwidth for a read/read/write triad on this Orin NX. At N=16M both edge to ~66.

**Conclusion:** on a *memory-bound* kernel you cannot beat the memory system, and
Triton is already at it. The correct v4 result is parity at the ceiling. To go
faster you must move less memory (fuse ops so intermediates are never written and
re-read) or measure a compute-bound kernel where FMA throughput and tiling, not
DRAM, are the bottleneck.

### Fusion: where the checked concatenative target actually wins

Moving less memory is exactly where a *concatenative checked* DSL beats the Triton
**authoring path** — and it costs nothing, because **fusion is not a compiler pass,
it is word concatenation.** A fused elementwise chain is just a sequence of checked
tile words; the intermediates stay on the (register) stack, so only the inputs load
and the result stores. `maki/fusion.f` lowers a maki/ONNX subgraph by mapping each
node to its tile word(s) and concatenating — the op-graph `[Mul, Add, Relu]` becomes:

```
K ( span<…,extent-n> span<…,extent-n> uniform<f32> -- )
  {: x y a :} x GRID-CTX-V4 {: g :} x g LOAD-V4 a SCALE-V4 y g LOAD-V4 ADD-V4 RELU-V4 y g STORE-V4 ;
```

The emitted PTX is **2 loads + 1 store, no global round-trips** (every intermediate
register-resident), and the checker types the whole sequence in one shot — so the
fused effect is **proven correct automatically**, or fails closed. On the Orin:
`relu(a·x+y)` device-golden PASS, **63 GB/s = hand-fused Triton's 63.4** — parity,
but produced *automatically* and *verified*, where the Triton author hand-fuses
unchecked (and can silently get the fused math wrong). Same speed; the difference is
that in Habu the **composition is the program** and the type system proves it. This,
not raw single-kernel speed, is the performance argument for the checked target.

The plan to contest the compute-bound column (tensor-core MMA, cp.async
stages, persistent content-keyed autotuning vs Triton's per-process JIT tuning,
fusion depth, PROMOTE-owned layout, launch amortization, gate-licensed
precision) is docs/compute-campaign.md; this doc stays the measured record.

**Earned claim:** in this measured SAXPY/softmax slice, a checked stack-effect
target **shifts the stack-discipline error class left to author time** — caught
statically, with a located diagnostic and zero GPU — where Triton finds it only at
runtime, at competitive bandwidth. **Not** earned: any broad "faster than Triton"
claim (SAXPY v4 reaches parity at the memory ceiling, not a win) or that the
checker catches *semantic* errors (it does not; that is the device-golden gate's
job).

## Multi-op authoring: the advantage compounds on a FUSED kernel

Extending the authoring loop from single ops to a fused chain `y = relu(a*x + y)`
(scale → add → relu). Independent subagents (k=3/target) authored it; **pass@1 =
3/3 both** — the fused chain is reachable in either target. The difference is again
*where bugs are caught*, and a longer fused chain has MORE structural ways to break.
Input x=2, y=-10, a=3 (so relu(a*x+y)=relu(-4)=0 distinguishes a missing relu):

| fused-relu error              | Habu-PTX (checked)          | Triton (real)            |
|-------------------------------|-----------------------------|--------------------------|
| correct relu(a*x+y)           | GREEN                       | GREEN                    |
| missing relu (semantic)       | runtime (device, got -4)    | runtime (got -4)         |
| **missing store (structural)**| **author** — stack effect   | **runtime** (silent -10) |
| **extra op after store**      | **author** — stack effect   | (runtime/compile)        |
| span-as-uniform / scalar-as-ptr (type) | **author** — type  | compile                  |

The win is the **structural** class: Habu's checker types the *whole fused chain*
end to end, so a missing store or an extra op is rejected at author time with a
located diagnostic (`at 'relu-v4' ... tile<f32,a,b>` left on the stack); in Triton
the identical missing-store **compiles clean and silently writes -10**, caught only
by a device run. Both catch name/type errors before running (Habu author, Triton
compile); neither catches the *semantic* missing-relu statically (both need the
device golden). So the checked-target advantage does not wash out as kernels get
bigger — it **compounds**, because fusion (which we get for free, see above) adds
exactly the stack-discipline surface the checker covers and Triton does not.

Graders: `/tmp/grade_habu_fused.sh` (v4 emit → ptxas → device golden vs CPU) and
`/tmp/triton-compare/grade_one_fused.py`; external arm, same as the rest.

## Model-driven pass@k + repair (live experiment)

The error-catch matrix above is fixture-based (the candidate behaviours are fixed
by language semantics, so it is an objective measurement, not a curated sample).
To get the **pass@k / repair-rounds** column the thesis asks for, the generator
must be an *independent stochastic model*, not a fixture we curate. So we used
**independent Claude `general-purpose` subagents as the generator** (k=5 per task
per target), each given the task + op *semantics* (not the assembled answer), and
graded every sample through the target's full loop: Triton via compile +
device-golden (`/tmp/triton-compare/grade_one.py`, `grade_softmax.py`), Habu-PTX
via checker → emit → ptxas → device-golden (`/tmp/grade_habu*.sh`, wrapping the
committed `lib/ptx` + `maki/eval/device*.f` pipeline). Generator population: Claude
subagents — state that bias; this is not a claim about all LLMs.

Two tasks, golden x=2,y=0,a=3→6.0 (SAXPY) and softmax([1,2,3,4]):

| task    | Triton pass@1 | Habu-PTX pass@1 | Habu-PTX after diagnostic-guided repair |
|---------|---------------|-----------------|-----------------------------------------|
| SAXPY   | 5/5           | 5/5             | — (0 repairs needed)                    |
| softmax | 5/5           | 3/5             | 5/5 (one fixed in 1 round, one in 2)    |

Unbiased pass@k for the Habu softmax round (n=5, c=3): pass@1 = 0.6, pass@2 = 0.9,
pass@3 = 1.0.

What this shows, and the honest caveats:

- **Both targets are highly reachable by the model.** For SAXPY both are 5/5; the
  novel checked DSL is as authorable as Triton (which the model knows from
  training) once given a short op spec. So the thesis is **not** "the checked
  target has a higher first-try rate" — on these tasks it does not.
- **The differentiator is the failure mode, not the rate.** All 5 Triton samples
  passed both tasks (no Triton failures to repair here — the model knows Triton
  cold). Every Habu-PTX failure was an **author-time static reject** with a
  precise expected-vs-actual *order* diagnostic and **zero GPU**, e.g.
  `at 'row-store' expected: tile span rowctx  actual: span rowctx tile`. Feeding
  that raw diagnostic back, the model repaired to green in 1 round (sample sh4) and
  2 rounds (sh3, whose first repair was a different wrong order — caught again,
  precisely, with no GPU). The earlier fixture battery shows the contrast: Triton's
  analogous stack-discipline/order errors surface only at **runtime** as a wrong
  number, with no located signal to repair from.
- **Caveat — the softmax pass@1 gap is confounded.** Our softmax prompt mislabelled
  ROW-STORE's argument order as `(span ctx tile)`; the real order is
  `(tile span ctx)`. The two Habu failures followed the wrong spec literally; the
  three passes used the natural stack idiom (leave the tile on the stack, append
  span+ctx). So 3/5 reflects our prompt error as much as the DSL — do **not** read
  it as "softmax is harder in Habu-PTX." It is, however, a faithful demonstration
  that when the *human spec* is subtly wrong, the checker catches the resulting
  error at author time and the located diagnostic drives repair to green — exactly
  where an unchecked target would emit a silently wrong number.
- **Repair asymmetry is not measured.** Triton produced no failures in these rounds,
  so there is no Triton repair-rounds number to compare against Habu's 1–2. The
  defensible claim is only about *when* and *how located* the failure signal is
  (author-time, type-precise vs runtime, wrong-number), not that one repairs in
  fewer rounds.

Reproduction status: this was a real one-off device run, but the model-generation
and grading wrapper scripts lived under `/tmp`. Treat the table as a recorded
snapshot until dot `habu-commit-checked-habu-a8ab5f56` lands the checked Habu
grader in-tree and dot `habu-re-run-habu-20318fcf` reruns softmax with the
corrected `ROW-STORE` prompt.

## GEMM: the FIRST measured compute-bound column (2026-07-04)

docs/compute-campaign.md step 1. The compute-bound contest starts here: square fp32 GEMMs
C = A·B at 512, 1024, and 2048, all three columns measured **in the same session
on the same Orin** (JetPack 6.2.1, sm_87, torch 2.9.1+cu126 / triton 3.5.1 per
the install recipe above).

Protocol (identical timing on both sides): per shape, one warmup launch, then
ITERS launches timed with **CUDA events** (start/stop event record +
elapsed-time), GFLOP/s = 2·S³·ITERS / elapsed. ITERS = 200, 80, and 30 for
512, 1024, and 2048 respectively. A = B = 1.0, C = 0 (values are immaterial to
timing). No L2 clearing between iterations on either side (see the do_bench
note below), so cache warmth is symmetric.

- **Habu columns:** `tools/ptx/gemm-bench.f` (in-tree, checked; run
  `bin/hb --load tools/ptx/gemm-bench.f` on the device). It emits + ptxas-assembles
  both in-tree kernels and times them via `tools/ptx/bench.f`:
  `MMN` = naive one-element/thread global-K-loop (`lib/ptx/cg-matmul-naive.f`,
  the same algorithm the `maki/lower/mm.f` naive fallback tile emits);
  `MM` = register-blocked 64×64 tile, shared As/Bs staging, 4×4
  accumulators/thread (`lib/ptx/cg-matmul.f`, the algorithm the `maki/lower/mm.f`
  blocked path emits).
- **Triton column:** the official-tutorial matmul (`tl.dot`, grouped ordering,
  8 autotune configs over BLOCK_M/N/K × stages × warps), fp32 in/out, on-device
  JIT for sm_87. One wheel caveat: the stock autotuner clears L2 via torch's
  `zero_()` ATen kernel, which this generic SBSA wheel lacks for sm_87
  (`cudaErrorNoKernelImageForDevice`), so the script passes a CUDA-event
  `do_bench` without the cache clear — same warm-cache conditions as the Habu
  loop. Script: `/tmp/triton_matmul.py` (verbatim below, out-of-tree per this
  doc's convention). Correctness of every timed Triton run was checked against a
  CPU torch f32 reference (max rel_err ~7.8e-4 — TF32-level, see below).

### Results (recorded on the Orin, 2026-07-04)

```
== MMN naive (1 elem/thread, global K-loop) ==
GEMM 512x512x512    iters=200  gpu_elapsed_ns=976077270   GFLOP/s_x1000=55002
GEMM 1024x1024x1024 iters=80   gpu_elapsed_ns=3110794677  GFLOP/s_x1000=55226
GEMM 2048x2048x2048 iters=30   gpu_elapsed_ns=9436062500  GFLOP/s_x1000=54619
== MM register-blocked 64x64 (4x4 micro-tile/thread, shared staging) ==
GEMM 512x512x512    iters=200  gpu_elapsed_ns=150557434   GFLOP/s_x1000=356588
GEMM 1024x1024x1024 iters=80   gpu_elapsed_ns=458149383   GFLOP/s_x1000=374984
GEMM 2048x2048x2048 iters=30   gpu_elapsed_ns=1354517944  GFLOP/s_x1000=380501

torch 2.9.1+cu126 | triton 3.5.1 | cuda True | dev (8, 7)
Triton GEMM 512x512x512    iters=200 time_ms=32.8  GFLOP/s=1636.1 max_abs_err=8.43e-02 rel_err=7.47e-04
Triton GEMM 1024x1024x1024 iters=80  time_ms=97.9  GFLOP/s=1755.4 max_abs_err=1.21e-01 rel_err=7.87e-04
Triton GEMM 2048x2048x2048 iters=30  time_ms=272.6 GFLOP/s=1890.5 max_abs_err=1.96e-01 rel_err=7.80e-04
  best_config (all shapes): BLOCK_M=128 BLOCK_N=128 BLOCK_K=32 GROUP_M=8 warps=4 stages=4
```

| GFLOP/s (fp32 C=A·B)    | 512³   | 1024³  | 2048³  |
|-------------------------|-------:|-------:|-------:|
| Habu naive (MMN)        |   55.0 |   55.2 |   54.6 |
| Habu blocked 64×64 (MM) |  356.6 |  375.0 |  380.5 |
| Triton (autotuned)      | 1636.1 | 1755.4 | 1890.5 |
| blocked / naive         |   6.5× |   6.8× |   7.0× |
| Triton / blocked        |   4.6× |   4.7× |   5.0× |

### What the data earns (and what it does not)

- **Register blocking is the single biggest lever, measured:** 6.5–7.0× over the
  naive tile, and the blocked GFLOP/s *climbs* with problem size (356.6 → 380.5)
  where the naive kernel is flat (~55) — the tiled-GEMM dot's "GFLOP/s climbs
  with tile size" verification, now measured with in-tree tools.
- **This is our v1 register-blocked tile vs their autotuned kernel — honest gap
  4.6–5.0×.** The Habu side has NO tensor-core MMA and NO cp.async multi-stage
  pipelining yet (the docs/compute-campaign.md steps that follow this baseline): bk=16
  staging, scalar `ld.shared`, single stage. The Triton side autotunes 8 configs
  and its `tl.dot` lowers to **TF32 tensor cores** on sm_87 — the measured
  `rel_err ~7.8e-4` against a CPU f32 reference is TF32-level precision (a pure
  f32 FMA kernel measures ~1e-6 rel here, as our device goldens do), so the two
  columns also differ in arithmetic: full-f32 fma.rn (ours, golden-gated at
  rtol 1e-4) vs TF32 dot (theirs, silently licensed).
- These absolute numbers supersede the older 15 W notes in `lib/ptx/cg-matmul.f`
  (~77 naive / ~283 blocked / ~1474 Triton): this session's device power state is
  higher; all three columns above are same-session, same-protocol.
- Next levers, in docs/compute-campaign.md order: `cp.async` multi-stage staging (the
  schedule family already carries `stages`), wider bk (family floor 32 vs the
  reused kernel's 16), vectorized `ld.shared.v4`, then the `mma.sync` TF32 family
  itself — with the precision *licensed* by the golden gate rather than assumed.

### Typed competitive rows: comparable result vs incomparable-by-policy source data

The shipped SAXPY and GEMM competitive rows are now carried as **typed BENCH
comparisons** and persisted through the sealed store (`tools/eval-triton.f`; schema
`bench/v1` in `maki/competitive-report.f` / `maki/competitive-store.f`), each side tagged
with its numeric policy (exact vs relative). That types one distinction the raw tables
above leave implicit:

- **Habu FP32 blocked tile vs Triton TF32 is incomparable-by-policy — not a competitive
  result.** Ours is full-f32 `fma.rn` (exact policy, golden-gated at rtol 1e-4); theirs is
  a TF32 `tl.dot` (relative policy). The two sides do not share a numeric policy, so the
  pairing is **incomparable**: the checked importer refuses to load it as a competitive
  result (`E-BENCH-INCOMPARABLE`). The 4.6–5.0× (step 1) and 3.9–4.3× (step 2) "gaps"
  above are the honest arithmetic/roofline story of a full-f32 tile against a TF32 kernel;
  they are kept as **separately-labelled source data**, not reported as a like-for-like
  competitive number.
- **The like-for-like GEMM competitive result is TF32-vs-TF32.** Habu's own tensor-core
  `mma.sync` TF32 tile (step 3) against Triton's TF32 `tl.dot` — both relative policy, so
  **comparable** — is the GEMM row the typed store carries as a competitive result.
- **SAXPY is comparable as-is:** both SAXPY-V4 and Triton are full-f32 (exact policy), so
  the memory-ceiling parity is a genuine competitive result, carried as the typed SAXPY
  FP32 row.

The regressions in `tools/eval-triton-test.f` pin all three: the two comparable rows
persist, report, and replay byte-stably, and the FP32-vs-TF32 pair can never load as a
competitive result.

> **RETRACTED — Orin 1.60× Triton claim (2026-07-19, dot `habu-retract-or-re-698be8b3`).** The Orin was retired as a measurement platform (Joel, 2026-07-19), so its "3026.6 GFLOP/s = 1.60× Triton" flagship GEMM head-to-head cited below is unverified, likely mistaken, and retired until a clean referee measurement exists. The GB10 spark is now the sole benchmark platform, and every GB10 conclusion in this doc stands on its own referee runs. The original text below is kept unchanged for history.

**Richer per-side evidence rows (schema-backed).** The bench/v1 comparison above is
throughput-only. The § 22.10 matrix's full per-side evidence schema —
`maki/competitive-evidence.f` (package `CEVID`) — now also carries these rows as versioned
`cevid/v1` records that add **revision**, **target** (`CAD-KIND:target-id`), **compiler**,
and the **latency / bytes / launches / memory / energy** fields over the closed metric
UNITS vocabulary `{ns, ms, gflops, gbps, bytes, count, watts}` (joules excluded — no
measured energy in the corpus). The **flagship GEMM row** (MMM-WIDE-B-M4-S1 **3026.6
GFLOP/s = 1.60x Triton 1890.5**, orin-nx-25w-918mhz, 2048³, both TF32/relative —
`tools/ptx/perf-rows.tsv`) and the **SAXPY-V4 64.209 vs Triton 63.0 GB/s** row are migrated
and pinned as byte-stable `cevid/v1` goldens in `maki/competitive-evidence-test.f`, which
also proves a mismatched numeric DOMAIN (FP32-exact vs TF32-relative) can never form a
comparison row (`E-CEVID-INCOMPARABLE`) and that cold/warm cache state is an explicit
key field. The metric UNITS vocabulary is the `maki/experiment/run-metric.f` package's
deferred fourth axis, decided here.

### Reproduction script (`/tmp/triton_matmul.py`, verbatim)

```python
import torch, triton, triton.language as tl

def get_autotune_config():
    return [
        triton.Config({'BLOCK_SIZE_M': 128, 'BLOCK_SIZE_N': 256, 'BLOCK_SIZE_K': 64, 'GROUP_SIZE_M': 8}, num_stages=3, num_warps=8),
        triton.Config({'BLOCK_SIZE_M': 64, 'BLOCK_SIZE_N': 256, 'BLOCK_SIZE_K': 32, 'GROUP_SIZE_M': 8}, num_stages=4, num_warps=4),
        triton.Config({'BLOCK_SIZE_M': 128, 'BLOCK_SIZE_N': 128, 'BLOCK_SIZE_K': 32, 'GROUP_SIZE_M': 8}, num_stages=4, num_warps=4),
        triton.Config({'BLOCK_SIZE_M': 128, 'BLOCK_SIZE_N': 64, 'BLOCK_SIZE_K': 32, 'GROUP_SIZE_M': 8}, num_stages=4, num_warps=4),
        triton.Config({'BLOCK_SIZE_M': 64, 'BLOCK_SIZE_N': 128, 'BLOCK_SIZE_K': 32, 'GROUP_SIZE_M': 8}, num_stages=4, num_warps=4),
        triton.Config({'BLOCK_SIZE_M': 128, 'BLOCK_SIZE_N': 32, 'BLOCK_SIZE_K': 32, 'GROUP_SIZE_M': 8}, num_stages=4, num_warps=4),
        triton.Config({'BLOCK_SIZE_M': 64, 'BLOCK_SIZE_N': 32, 'BLOCK_SIZE_K': 32, 'GROUP_SIZE_M': 8}, num_stages=5, num_warps=2),
        triton.Config({'BLOCK_SIZE_M': 32, 'BLOCK_SIZE_N': 64, 'BLOCK_SIZE_K': 32, 'GROUP_SIZE_M': 8}, num_stages=5, num_warps=2),
    ]

# stock autotuner do_bench clears L2 via torch zero_() (no sm_87 image in this wheel);
# time candidates with CUDA events instead - warm-cache, symmetric with the Habu loop.
def event_do_bench(fn, quantiles=None, **kwargs):
    fn(); torch.cuda.synchronize()
    start = torch.cuda.Event(enable_timing=True); stop = torch.cuda.Event(enable_timing=True)
    reps = 10
    start.record()
    for _ in range(reps): fn()
    stop.record(); torch.cuda.synchronize()
    ms = start.elapsed_time(stop) / reps
    return [ms] * len(quantiles) if quantiles is not None else ms

@triton.autotune(configs=get_autotune_config(), key=['M', 'N', 'K'], do_bench=event_do_bench)
@triton.jit
def matmul_kernel(a_ptr, b_ptr, c_ptr, M, N, K,
                  stride_am, stride_ak, stride_bk, stride_bn, stride_cm, stride_cn,
                  BLOCK_SIZE_M: tl.constexpr, BLOCK_SIZE_N: tl.constexpr,
                  BLOCK_SIZE_K: tl.constexpr, GROUP_SIZE_M: tl.constexpr):
    pid = tl.program_id(axis=0)
    num_pid_m = tl.cdiv(M, BLOCK_SIZE_M); num_pid_n = tl.cdiv(N, BLOCK_SIZE_N)
    num_pid_in_group = GROUP_SIZE_M * num_pid_n
    group_id = pid // num_pid_in_group
    first_pid_m = group_id * GROUP_SIZE_M
    group_size_m = min(num_pid_m - first_pid_m, GROUP_SIZE_M)
    pid_m = first_pid_m + ((pid % num_pid_in_group) % group_size_m)
    pid_n = (pid % num_pid_in_group) // group_size_m
    offs_am = (pid_m * BLOCK_SIZE_M + tl.arange(0, BLOCK_SIZE_M)) % M
    offs_bn = (pid_n * BLOCK_SIZE_N + tl.arange(0, BLOCK_SIZE_N)) % N
    offs_k = tl.arange(0, BLOCK_SIZE_K)
    a_ptrs = a_ptr + (offs_am[:, None] * stride_am + offs_k[None, :] * stride_ak)
    b_ptrs = b_ptr + (offs_k[:, None] * stride_bk + offs_bn[None, :] * stride_bn)
    acc = tl.zeros((BLOCK_SIZE_M, BLOCK_SIZE_N), dtype=tl.float32)
    for k in range(0, tl.cdiv(K, BLOCK_SIZE_K)):
        a = tl.load(a_ptrs, mask=offs_k[None, :] < K - k * BLOCK_SIZE_K, other=0.0)
        b = tl.load(b_ptrs, mask=offs_k[:, None] < K - k * BLOCK_SIZE_K, other=0.0)
        acc = tl.dot(a, b, acc)
        a_ptrs += BLOCK_SIZE_K * stride_ak
        b_ptrs += BLOCK_SIZE_K * stride_bk
    offs_cm = pid_m * BLOCK_SIZE_M + tl.arange(0, BLOCK_SIZE_M)
    offs_cn = pid_n * BLOCK_SIZE_N + tl.arange(0, BLOCK_SIZE_N)
    c_ptrs = c_ptr + stride_cm * offs_cm[:, None] + stride_cn * offs_cn[None, :]
    c_mask = (offs_cm[:, None] < M) & (offs_cn[None, :] < N)
    tl.store(c_ptrs, acc, mask=c_mask)

def matmul(a, b, c):
    M, K = a.shape; _, N = b.shape
    grid = lambda META: (triton.cdiv(M, META['BLOCK_SIZE_M']) * triton.cdiv(N, META['BLOCK_SIZE_N']),)
    matmul_kernel[grid](a, b, c, M, N, K,
                        a.stride(0), a.stride(1), b.stride(0), b.stride(1),
                        c.stride(0), c.stride(1))

print(f"torch {torch.__version__} | triton {triton.__version__} | cuda {torch.cuda.is_available()} "
      f"| dev {torch.cuda.get_device_capability()}")
for S, iters in [(512, 200), (1024, 80), (2048, 30)]:
    a = torch.randn(S, S, dtype=torch.float32)   # CPU RNG (no sm_87 ATen kernels in this wheel)
    b = torch.randn(S, S, dtype=torch.float32)
    ref = a @ b                                   # CPU f32 reference
    ad, bd = a.to('cuda'), b.to('cuda')
    cd = torch.zeros(S, S, dtype=torch.float32).to('cuda')
    matmul(ad, bd, cd); torch.cuda.synchronize() # warmup: JIT + autotune
    err = (cd.cpu() - ref).abs().max().item()
    rel = err / ref.abs().max().item()
    start = torch.cuda.Event(enable_timing=True); stop = torch.cuda.Event(enable_timing=True)
    start.record()
    for _ in range(iters): matmul(ad, bd, cd)
    stop.record(); torch.cuda.synchronize()
    ms = start.elapsed_time(stop)
    gflops = (2 * S ** 3 * iters) / (ms * 1e-3) / 1e9
    print(f"Triton GEMM {S}x{S}x{S} iters={iters} time_ms={ms:.1f} GFLOP/s={gflops:.1f} "
          f"max_abs_err={err:.2e} rel_err={rel:.2e}")
    print(f"  best_config: {getattr(matmul_kernel, 'best_config', None)}")
```

## GEMM step 2: pipelining the blocked tile (2026-07-05)

docs/compute-campaign.md step 2. Same device, same protocol, same shapes as the step-1
baseline above (fp32 `C = A·B`, CUDA-event timing, one warmup, ITERS 200, 80,
30 at 512, 1024, 2048, A = B = 1.0). Pure f32 throughout — **no precision
change** (the device goldens still gate `fma.rn.f32` at rtol 1e-4; TF32/MMA is
step 3). Each
increment re-emits + ptxas-assembles the same in-tree `MM` kernel
(`lib/ptx/cg-matmul.f`, also the algorithm the `maki/lower/mm.f` blocked path
emits) and re-runs `tools/ptx/gemm-bench.f` on the Orin. Every increment keeps
the `maki/lower/mm-device-test.f` and `maki/lower/model-device-test.f` goldens
green (incl. the 64×64 blocked MATMUL and LINEAR→GELU cases) — device == host
within the f32 matmul tolerance, unchanged.

- **+A `bk=32` + `ld.shared.v4` B load:** widen the K-tile from 16 to the
  gemm-tf32-v1 family floor (32), and replace the 4 scalar `ld.shared.f32` B
  operands with one `ld.shared.v4.f32` (the 4 micro-tile columns are contiguous
  in the row-major `Bs[32][64]` tile; `SH` is now `.align 16`). The 4 A operands
  stay scalar — they are column-strided in the row-major `As[64][32]` tile, and
  transposing A to vectorize them would break the contiguous global→shared
  `cp.async` copy that +B needs. Net per k-step: 5 shared-load instructions drive
  16 FMAs (was 8 → 16).
- **+B `cp.async` double-buffer (stages=2):** replace the scalar
  global→register→`st.shared` staging with `cp.async.cg.shared.global` — a direct
  16 B (4-float) global→shared copy, no register round-trip. Two SH buffers
  (`SH[32768]`, parity toggle); tile t+1 is prefetched into the *other* buffer
  while tile t computes from the current one, ordered by `commit_group` /
  `wait_group 1` (the tail iteration drains with `wait_group 0`), so the
  global-load latency overlaps the accumulate. The emitter honors the family
  `stages` parameter (2 now; 3–4 is more parity slots + a deeper `wait_group N`,
  same loop shape). This directly attacks the two stalls the baseline notes call
  out (scalar load-into-smem + the load/compute `bar.sync`).

### Results (recorded on the Orin, 2026-07-05)

```
== MM register-blocked 64x64, +A (bk=32 + ld.shared.v4 B) ==
GEMM 512x512x512    iters=200  GFLOP/s_x1000=378584 (379549 on a 2nd run)
GEMM 1024x1024x1024 iters=80   GFLOP/s_x1000=397206
GEMM 2048x2048x2048 iters=30   GFLOP/s_x1000=402526
== MM register-blocked 64x64, +B (cp.async double-buffered, stages=2) ==
GEMM 512x512x512    iters=200  GFLOP/s_x1000=416684 (416931 on a 2nd run)
GEMM 1024x1024x1024 iters=80   GFLOP/s_x1000=436774
GEMM 2048x2048x2048 iters=30   GFLOP/s_x1000=441801
```

| GFLOP/s (fp32 C=A·B)          | 512³   | 1024³  | 2048³  | ptxas       |
|-------------------------------|-------:|-------:|-------:|-------------|
| Habu blocked baseline (bk=16) |  356.6 |  375.0 |  380.5 | 48 reg / 8 KB  |
| +A bk=32 + v4 B               |  378.6 |  397.2 |  402.5 | 48 reg / 16 KB |
| +B cp.async double-buffer     |  416.7 |  436.8 |  441.8 | 56 reg / 32 KB |
| Triton (autotuned TF32)       | 1636.1 | 1755.4 | 1890.5 | —           |

- **+A over baseline: +6.2% / +5.9% / +5.8%.** No register change (48), no
  spills; smem doubles (8→16 KB) for the wider K-tile. The lift comes from the
  fewer shared-load instructions (v4 B) and fewer K-loop iterations / barriers
  (bk 16→32).
- **+B over +A: +10.1% / +10.0% / +9.8% (+16.9% / +16.5% / +16.1% over the
  baseline).** cp.async adds 8 registers (48→56, still 0 spills) and doubles smem
  again (16→32 KB, the second buffer); the overlap of the global-load latency
  with the accumulate is the win. All three shapes stay compute-bound (util rises
  with size, as the tile intends).
- **Gap to Triton: 3.9× / 4.0× / 4.3×** (was 4.6–5.0× at baseline). Both columns
  are still honest-different in arithmetic: our full-f32 `fma.rn` (golden-gated at
  rtol 1e-4) vs Triton's TF32 `tl.dot` on the tensor cores. Closing the rest of
  the gap needs the higher compute roof — `mma.sync` TF32 (step 3), not more SIMT
  pipelining. Every increment above kept the device goldens
  (`lower-mm-device-test` 64×64 blocked MATMUL + LINEAR→GELU,
  `lower-model-device-test`) green, device == host within the f32 matmul
  tolerance — the correctness harness the whole exercise is built on.

## GEMM step 3: TF32 tensor-core `mma.sync` micro-tile (2026-07-05)

docs/compute-campaign.md step 3. The FP32 CUDA-core roof (~940 GFLOP/s) caps the `fma.rn.f32`
tile (step 2 topped at 442); TF32 tensor cores sit on a *higher* roof, so the
step-3 lever is `mma.sync`, not more SIMT tiling. Same device / protocol / shapes.

The new `MMM` kernel (`lib/ptx/cg-mma.f`) keeps step 2's 64×64 block and cp.async
double-buffered `As[64][32]`/`Bs[32][64]` staging *verbatim* (shared
`MM-PIPE-KLOOP-WITH` scaffold) and swaps ONLY the compute inner: the 4×4
`fma.rn.f32` micro-tile becomes warp-level
`mma.sync.aligned.m16n8k8.row.col.f32.tf32.tf32.f32` tiles. 8 warps tile the
64×64 output as warp_row=warpid>>1 (0..3) × warp_col=warpid&1 (0..1); each warp
owns 16×32 = 4 MMA n-tiles, loading one 16×8 A fragment per MMA-K substep and
reusing it across the 4 n-tiles (16 f32 accumulators/lane). Operands are
`cvt.rna.tf32.f32`; the accumulator stays f32.

**Fragment layout was proven in isolation FIRST** (the course's #1 "correct in
NumPy, garbage on device"): `tools/ptx/mma-probe.f` runs ONE mma tile with
committed integer operands and checks it **element-exact** vs a host matmul (128
cells, 0 mismatches). Then `tools/ptx/mma-gemm-check.f` proves the full
K-looping kernel (staging + accumulation + the warp/D-fragment store mapping)
**element-exact** at 64³ (4096 cells) and 128³ (16384 cells). Licensed: with
`PREC-TF32` requested for the matmul class, `maki/lower/mm.f` emits this kernel
and `maki/precision-device-test.f` LOWER-GOLDEN passes **device==host within the
tf32 row (rtol 2e-3, 4096 elems)** — the passing verdict IS the running license;
a seeded 0.5% store fault still FAILS under tf32, and PREC-RESET re-emits the f32
kernel green under the f32 row.

### Results (recorded on the Orin, 2026-07-05)

```
== MMN naive (1 elem/thread)                       ==  55.0 / 55.2 / 54.6
== MM  register-blocked 64x64, cp.async (fp32)     == 416.2 / 436.8 / 441.8
== MMM tensor-core TF32 mma.sync 64x64, cp.async   == 375.6 / 393.5 / 398.5
```

| GFLOP/s (C=A·B)                     | 512³   | 1024³  | 2048³  | ptxas          | arith |
|-------------------------------------|-------:|-------:|-------:|----------------|-------|
| MMN naive (fp32)                    |   55.0 |   55.2 |   54.6 | —              | f32   |
| MM cp.async blocked (fp32)          |  416.2 |  436.8 |  441.8 | 56 reg / 32 KB | f32   |
| **MMM `mma.sync` TF32**             |  375.6 |  393.5 |  398.5 | 38 reg / 32 KB | tf32  |
| Triton (autotuned TF32)             | 1636.1 | 1755.4 | 1890.5 | —              | tf32  |

- **The MMA kernel is device-correct and on the tensor-core path, but at THIS rung
  it does not yet beat the tuned f32 blocked tile** (398 vs 442 at 2048³, ~90%),
  and is ~21% of Triton. This is the honest, roofline-predicted result: the doc's
  reuse→stage ladder for staged MMA on this device lands ~371–398, well under the
  940 FP32 roof — feeding the tensor cores, not saturating them.
- **Why it's starved, from the evidence:** MMM uses only **38 registers** (vs MM's
  56) and 0 spills — it is *not* register-bound. The bottleneck is the fragment
  feed: 4 scalar `ld.shared.f32` A + 8 scalar B loads per substep hit shared-bank
  conflicts, and 48 `cvt.rna.tf32` per staged tile add ALU overhead, so the MMA
  units wait. A fragment is reused only 4× (16×32 warp tile), below the 8× a
  16×64 warp tile gives.
- **What cad-6 tuning should search next (the standard high-perf suite, each a
  roofline move):** `ldmatrix` for the fragment loads (kills the scalar-load bank
  conflicts and packs 4 tf32/`.b32`, halving the `cvt` count); a 16×64 warp tile
  (4-warp / 128-thread cooperative staging → 8× A-reuse); larger BK (fewer
  `bar.sync`, more compute/sync); and a swizzled bank-conflict-free shared layout.
  Reuse → stage → pipeline is the predicted order; step 3 landed the tile + the
  license, and each remaining rung is dotted on `habu-tensor-core-mma`.

### Step 3c: `ldmatrix` fragment-load ablation — NEGATIVE (measured 2026-07-05)

Dot `habu-mma-ldmatrix-fragment`. The step-3 diagnosis ("load/ALU-bound: scalar
shared fragment loads + cvt overhead") predicted `ldmatrix` as the biggest MMA
jump. We built it, proved it element-exact, measured it — and **falsified the
diagnosis**. Same device / protocol / shapes as steps 1–3.

`lib/ptx/cg-mma.f` now emits the fragment feed in one of three modes
(`MMA-LMODE`, fixed at emit time), an honest single-variable ablation:

- **mode 0** — rung-1 baseline: scalar `ld.shared.f32` + `cvt.rna.tf32.f32`
  (4 A + 8 B loads, 48 cvt per staged tile);
- **mode 1** — cvt-drop: scalar `ld.shared.b32`, **no cvt** (`mma.sync` reads the
  top bits of the raw f32 as tf32 — truncation instead of RNE, a <1-ulp tf32
  difference inside the licensed rtol 2e-3);
- **mode 2** — `ldmatrix.sync.aligned.m8n8.x4.shared.b16` for the 16×8 A fragment
  (a tf32 = 2 adjacent b16 halves, so the fragment is exactly 4 congruous 8×8 b16
  tiles = ONE ldmatrix.x4 replacing the 4 scalar A loads) + raw `ld.shared.b32`
  B, no cvt. B keeps scalar loads: a B-side ldmatrix needs a transposed/swizzled
  `Bs` (the `habu-mma-larger-bk` rung's layout change).

**Fragment-first proof, then kernel:** `tools/ptx/mma-probe.f` `MP-LDM-ALL` runs
the single-warp isolation tile with ldmatrix-staged A — **element-exact, 0/128
mismatches** (committed alongside the scalar probe). `tools/ptx/mma-gemm-check.f`
then sweeps ALL THREE modes through the full K-looping `MMM` — **element-exact at
64³ (4096 cells) and 128³ (16384 cells) in every mode**. The licensed golden
(`maki/precision-device-test.f`) stays green: tf32 license passes, the seeded
0.5% store fault still fails, PREC-RESET re-emits f32 green.

**Results (Orin, 2026-07-05, `tools/ptx/gemm-bench.f`, two full runs, ±0.1%):**

| GFLOP/s (C=A·B)                        | 512³  | 1024³ | 2048³ | ptxas             |
|----------------------------------------|------:|------:|------:|-------------------|
| MMM mode 0: scalar+cvt (rung-1)        | 376.1 | 393.5 | 398.5 | 38 reg / 0 spill  |
| MMM mode 1: scalar raw, no cvt         | 375.7 | 394.5 | 400.4 | 40 reg / 0 spill  |
| MMM mode 2: ldmatrix A + raw B, no cvt | 370.0 | 388.9 | 394.3 | 43 reg / 0 spill  |
| MM cp.async blocked (fp32, reference)  | 416.9 | 436.8 | 441.8 | 56 reg / 0 spill  |
| Triton (autotuned TF32, step-1 record) | 1636.1| 1755.4| 1890.5| —                 |

- **Killing all 48 `cvt`/tile is FLAT** (mode 1 vs 0: −0.1% / +0.3% / +0.5%) —
  the cvt ALU work was fully hidden.
- **Replacing the 4 scalar A loads with ONE `ldmatrix.x4` is ~1.2% SLOWER**
  (mode 2 vs 0), with MORE registers (43 vs 38) and fewer instructions (326 vs
  390 PTX lines). The scalar-load bank conflicts were also hidden.
- **Conclusion: at this rung the tensor cores are NOT fragment-feed-bound.** The
  throughput is invariant to the fragment-load mechanism, so the limiter is what
  the ablation holds constant: the per-warp `mma.sync` dependency structure (A
  reused only 4×; each n-tile's mma waits on the 2 B loads issued immediately
  before it) and the staging/`bar.sync` cadence at BK=32. That redirects the
  effort exactly where the remaining dots point — `habu-mma-16x64-warp` (8×
  A-reuse, more independent mma per A fragment) and `habu-mma-larger-bk`
  (fewer syncs + swizzled Bs enabling a B-side ldmatrix) — and tells the cad-6
  autotuner to search warp-tile shape and BK, not load flavor, on this axis.
- The kernel default stays **mode 0** (measured-best-tied AND exact-RNE, so the
  licensed tf32 golden row is bit-identical to rung 1). The ldmatrix mechanism
  stays committed, device-proven, and selectable for the higher-reuse rung where
  the per-fragment cost amortizes 8× and Bs can be swizzled.

## Live pass@k round via the transcript harness (2026-07-12)

The first pass@k round recorded and graded entirely through the committed
transcript harness — no `/tmp` scripts, no ad hoc subagent logs. This is the
durable re-run the 2026-06-27 section's "Reproduction status" asked for: the
in-tree grader (`maki/eval/matrix-main.f`) is the judge, the generation run is a
committed transcript, and the softmax prompt carries the **corrected**
`ROW-STORE` order (`tile span rowctx`).

Protocol: generator = independent **claude-opus-4-8** subagents (`claude` CLI
print mode, `--tools "" --safe-mode`, one fresh session per sample — blind to
each other, to the grader, and to the repo), **n = 5 per task**, each given only
the task statement + the committed authoring-surface stack effects (the
`lib/ptx/tile.f` / `collective.f` / `tile-v4.f` contracts). Judge = the
committed checker via the replay harness. Repair = at most ONE round: the raw
checker diagnostic fed back to the SAME generator session, the repair recorded
as the sample's next candidate line. Candidates are verbatim model output.
Transcript: `maki/transcripts/live-habu-ptx-2026-07-12.txt`; replay:

```
bin/hb --load maki/eval/matrix-main.f -- maki/transcripts/live-habu-ptx-2026-07-12.txt
```

| task | target | n | green | pass@1-x1000 | pass@2-x1000 | pass@3-x1000 | repaired | repair-rounds | tokens-to-green | tok-est | GB/s-x10 | device | graded | tok-src |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| saxpy | habu-ptx | 5 | 4 | 800 | 1000 | 1000 | 1 | 1 | 172 | 372 | not-run | not-run | checker | proxy |
| softmax | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 292 | 638 | not-run | not-run | checker | proxy |
| fused-relu | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 150 | 380 | not-run | not-run | checker | proxy |

(The `tok-est` and `tok-src` columns were added 2026-07-13: `tok-est` is the
deterministic `GEN-TOK-EST` model-token estimate computed from the same
committed candidates at replay time; `tok-src` marks the tokens-to-green unit —
`proxy` here, since this round predates generator-reported counts.)

- **Softmax 5/5 first-try with the corrected prompt.** This discharges the
  2026-06-27 caveat: the earlier 3/5 was the mis-specified `ROW-STORE` order in
  the prompt, not softmax being harder in Habu-PTX. With the committed contract
  quoted correctly, every sample used the natural tile-on-stack idiom and
  certified first try.
- **The one live failure was exactly the checker's home turf.** The saxpy sx1
  draft built three separate `GRID-CTX` contexts (one per access) instead of
  reusing one; the checker rejected at author time, zero GPU, with the located
  diagnostic `in k: at '+.' expected: tile<f32,a,fresh-mask-live-a>
  tile<f32,a,fresh-mask-live-a> actual: tile<f32,b,fresh-mask-live-b>
  tile<f32,a,fresh-mask-live-a>` — the fresh-mask discipline catching a
  duplicated-context bug that would compile clean in an unchecked target (each
  load/store is individually well-formed; only the composition is wrong). Fed
  that raw diagnostic, the same session repaired to the single-context idiom in
  one round. Unbiased pass@k (n=5, c=4): pass@1 = 0.8, pass@2 = pass@3 = 1.0.
- **The fused chain stayed 5/5** (`relu(a*x+y)` over the v4 vocabulary),
  consistent with the 2026-06-27 fused-relu round: chain length added no
  first-try failures when the surface contracts are stated precisely.
- Honest scope: host-only round (GB/s + device columns stay `not-run`; the
  device-golden arm is unchanged from the measured sections above). Generator
  population is Claude subagents — a statement about this model family, not all
  LLMs. There is no Triton arm in this round: nothing here revises the recorded
  Triton column.
- Durability: the transcripts are committed static files, so the graded tallies
  above are deterministic replay — `maki/eval/live-test.f` (in the maki suite)
  re-replays the transcript and pins every cell of this table.

## Live authoring round: collective / 2D-GEMM / attention tasks (2026-07-13)

The second live round extends the task set past the 1-D tile kernels to the
three OFF-DEVICE authoring tasks graded by the new emit-structural autograder
(`maki/eval/emit.f`): **sumnorm** (row sum-normalize over the collective
vocabulary — same surface as softmax; the prompt names the block-wide sum
reduction, and the forbidden `max.f32`/`ex2.approx` gates catch a softmax
pattern-match), **gemm** (the tiled-GEMM checked phase pipeline
`MM-BEGIN MM-K-LOOP MM-STORE`), and **attention** (the fused phase-token
pipeline `ATTN:START .. ATTN:FINISH`, where omitting or reordering a phase is a
checker reject). GREEN for these tasks = certify + child-process PTX emit +
structural gates (required instructions present, forbidden patterns absent);
the device-golden leg is Orin-gated and recorded as a SKIP in the suites.

Protocol: generator = independent **opus-model subagents** (orchestrator Agent
tool, one fresh blind session per sample, zero tool uses), **n = 5 per task**,
each given only the shared authoring preamble + the task statement
(`maki/transcripts/prompts-live-2026-07-13/`). Judge = the committed checker
via the replay harness, plus the emit-structural grade per distinct candidate
shape. Repair budget = one diagnostic-guided round; **no sample needed it**.
Transcript: `maki/transcripts/live-habu-ptx-2026-07-13.txt`; replay:

```
bin/hb --load maki/eval/matrix-main.f -- maki/transcripts/live-habu-ptx-2026-07-13.txt
```

| task | target | n | green | pass@1-x1000 | pass@2-x1000 | pass@3-x1000 | repaired | repair-rounds | tokens-to-green | tok-est | GB/s-x10 | device | graded | tok-src |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|
| sumnorm | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 194 | 472 | not-run | not-run | checker | proxy |
| gemm | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 50 | 320 | not-run | not-run | checker | proxy |
| attention | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 90 | 469 | not-run | not-run | checker | proxy |

- **15/15 first-try green, and every distinct shape also passed the
  emit + structural gates** (GREEN 2 through `GRADE-SUMNORM` / `GRADE-GEMM` /
  `GRADE-ATTN`): no sample pattern-matched softmax into sumnorm (the forbidden
  `max.f32` / `ex2.approx` gates stayed silent), every gemm sample composed the
  full three-phase pipeline (fma + cp.async present), and every attention
  sample threaded the phase tokens in the one type-correct order.
- **What GREEN(2) does NOT prove (honest limit).** The emit-structural gates
  check instruction presence/absence, not numeric correctness, so a same-type
  ROLE or VALUE swap grades GREEN while computing the wrong result: sumnorm
  in/out swap, div-by-sum-squared, gemm double-accumulate (`2·A·B`), attention
  Q/K swap and output-into-V all pass structure. These are pinned at grade 2 as
  acknowledged wrong-but-green regressions in `maki/eval/emit-test.f`; only a
  device NUMERIC golden closes the class (`habu-eval-device-numeric-c2e98ec4`).
  So the 15/15 headline means "the checked-pipeline surface is reliably
  authorable", not "the model wrote a numerically-correct kernel".
- **Token units:** `tokens-to-green` is the whitespace source-token proxy;
  `tok-est` is the `GEN-TOK-EST` model-token estimate (alnum runs + punctuation
  bytes) from the same committed candidates. Generator-reported counts are not
  exposed by the Agent tool, so the transcript stays format v1; a future round
  recorded through the `claude` CLI can carry real `usage` counts as v1.1
  `tokens` directives and the matrix will mark that row `model`.
- Honest scope: host-only round (GB/s + device columns `not-run`; the sumnorm/
  gemm/attention device goldens are tracked Orin work). Phase-word tasks (gemm,
  attention) measure *composition* authoring — the phase implementations are
  committed library emitters — so their pass@1 says the checked-pipeline
  surface is trivially and reliably authorable, not that the model wrote the
  inner PTX. Generator population is one Claude-family model; n=5 per task.
- Durability: `maki/eval/live-author-test.f` (in the maki suite) re-replays the
  transcript, pins every cell of this table, and re-grades each distinct live
  candidate through the emit-structural autograder.

## GEMM on the DGX Spark GB10 (sm_121a) — the north-star head-to-head (2026-07-19)

> **RETRACTED foil — the Orin 1.60× Triton figure referenced in this section (2026-07-19, dot `habu-retract-or-re-698be8b3`).** The Orin was retired as a measurement platform (Joel, 2026-07-19); the "on the 8-SM Orin the same wide tile family measured 1.60× Triton" / "reversing the Orin's 1.60×" baseline is unverified and likely mistaken, so read those only as an unverified prior, not a confirmed result. The GB10 measurements in this section are the sole benchmark platform and stand on their own referee runs. Original text kept unchanged for history.

The compute-bound GEMM column re-measured on the **GB10** (dot
`habu-gb10-gemm-head`), the first head-to-head against the **source-built Triton
3.8.0** (`~/Work/ml/.venv`: torch 2.9.1a0+cu130, triton 3.8.0) on the real
sm_121a device. This extends the Orin GEMM steps above; the roofline it is
scored against is `docs/codegen-verdict.md` (GB10: 48 SM, sustained **2411 MHz**
under GEMM — verified `nvidia-smi`, ≤49 °C / ≤48.5 W, **not** throttled — so
fp32 CUDA roof = 48·128·2·2.411e9 = **29.6 TFLOP/s**; tf32 tensor roof
**≈50 TFLOP/s**, marketing-derived estimate, flagged).

**Headline (honest, and it reverses the Orin result): on the GB10, Habu's tuned
TF32 tensor-core tile reaches 0.59–0.80× of Triton 3.8's TF32 `tl.dot` — peak
0.80× at 2048³. Triton wins on this device.** On the 8-SM Orin the same wide
tile family measured **1.60×** Triton (3026 vs 1890 at 2048³, §22.10); on the
48-SM GB10 that inverts. Triton wins **even though** the GB10's 99 KB
shared-memory-per-block cap prunes **31 of its 45** tf32 autotune configs
(`OutOfResources: shared memory, Required: … Hardware limit: 101376` — the #8182
consumer-Blackwell smem-geometry family): its surviving small-tile kernel still
out-throughputs Habu's hand-tuned wide tile and reaches **~91 %** of the tf32
roof at 4096³, where Habu's tile plateaus at **~56 %**.

**Round-2 update (2026-07-19, dot `habu-4-warp-mma`): the 4-warp tile lifts the
peak to 0.83×.** The `habu-mma-warp-shape` 4-warp / `BM128×BN64` tile that round-1
named as the next lever is now built and measured. It wins 512³ (0.59→**0.67×**),
1024³ (0.75→**0.83×**), and 2048³ (0.80→**0.83×**); 4096³ stays with the 8-warp
tile (0.62×). Triton still wins every shape, but the gap narrows. The coupled
`num_stages≥3` half of the round-1 hypothesis was **falsified** on this device: a
built-and-proven N-stage pipeline is uniformly *slower* than 2 stages, because 3+
buffers exceed half the 100 KB smem/SM and force 1 block/SM. See "Round 2 — the
4-warp tile" below.

### Protocol (identical timing on both sides)

- **Habu column:** `tools/ptx/gemm-bench.f` `GB-GB10` (in-tree, checked;
  `bin/hb --load tools/ptx/gemm-bench.f` on the GB10). Assembler arch flows from
  the probed active target (`maki/eval/active-target.f` → `sm_121a`, no literal).
  Per shape: 1 warmup launch, then ITERS launches timed with **CUDA events**
  (`PTXBENCH:BENCH-GPU-NS`), GFLOP/s = 2·S³·ITERS / elapsed. ITERS 400, 200, 80,
  40 at 512, 1024, 2048, 4096; **best of 3 full passes** (steady-state peak; per-cell
  run-to-run spread 0–6 % on the tuned tf32 configs). A = B = 1.0, C = 0 (values
  immaterial to timing).
- **Triton column:** `/tmp/gemm-triton-gb10.py` (verbatim below, out-of-tree per
  this doc's convention — the repo's host-lint rejects committed `.py`), the
  standard grouped-ordering `tl.dot` matmul, **manual max-autotune** (a 45-config grid
  timed with CUDA events, best kept; failing configs caught and counted, so the
  referee's ceiling is honest not flattered). No `torch.compile`/inductor, so the
  inductor `is_big_gpu` 68-SM gate never applies — this is the raw-`triton.jit`
  fair referee. Same shapes, CUDA-event warm timing, **best of 3 passes**;
  `torch.matmul` same-dtype reference (tf32 `rel_err ~8e-4`, fp16 exact).
- **Clock-matched:** both columns sampled at the same 2385–2411 MHz band in the
  same session; the GB10 holds its 2418 MHz application clock (no boost to the
  3003 MHz ceiling) under sustained GEMM, so neither side has a clock advantage.

### Results — TF32 (like-for-like: Habu f32-in / tf32-mma / f32-acc vs Triton tf32)

| TFLOP/s (tf32, C=A·B)        |  512³ | 1024³ | 2048³ | 4096³ |
|------------------------------|------:|------:|------:|------:|
| Habu tuned MMA tile          |  12.9 |  25.2 |  30.3 |  28.0 |
| Triton 3.8 `tl.dot` (autotd) |  21.7 |  33.5 |  37.8 |  45.3 |
| **Habu / Triton**            | 0.59× | 0.75× |**0.80×**| 0.62× |
| Habu %-of-tf32-roof (~50 TF) |   26% |   50% |   61% |   56% |
| Triton %-of-tf32-roof        |   43% |   67% |   76% |   91% |

The **1024³** cell was lifted 23.2 → **25.2 TF** (0.69× → **0.75×**) this session by a
newly-swept config — the MFRAGS=2 128×64 tile fed by the **transposed-Bs B-`ldmatrix`
at single-buffer** (`32 8 1 1 2 4 GB-MMM-CFGW-B`), which was not in the prior wide-B
sweep (that ran MFRAGS=2 B-`ldmatrix` only double-buffered, which is occupancy-bound).
Element-exact first (`mma-gemm-check.f`, 128³/256³, added rows), then best-of-3 over
three full committed `GB-GB10` passes at 2411 MHz. The other three shapes re-measured
within best-of-3 noise of the committed values this session (512³ 13.0, 2048³ 29.1–30.0,
4096³ 27.6–28.4), so they are left at their committed best; the peak ratio is still
**0.80× at 2048³** and 512³/4096³ are unmoved. The full extended sweep is below.

Both sides are TF32 (relative numeric policy), so this is a like-for-like
competitive pairing (unlike the FP32-vs-TF32 rows above): `rel_err ~8e-4` on both
against a same-dtype `torch.matmul` reference. FP32 CUDA-core reference (Habu
`MM`, `lib/ptx/cg-matmul.f`) for the roof anchor: 8.2 / 13.2 / **14.9** / 13.0
TFLOP/s = **50 %** of the 29.6 TF fp32 roof at 2048³ — the same roof-fraction
Orin's blocked tile hit (`docs/codegen-verdict.md`), reproduced on the higher
roof. The tuned tf32 tile beats that fp32 tile ~2× (30.3 vs 14.9 at 2048³), so
the tensor-core path is real; it just does not out-run Triton on this device.

### The GB10 schedule sweep (dot phase 2 — tune, keep best per shape)

Every row is a config the correctness harness proves **element-exact** first
(`tools/ptx/mma-gemm-check.f` `MGC-ALL`, 64³…512³, 0 mismatches on the GB10 — the
throughput below is only ever reported for a verified-exact kernel). The sweep
walks the axes the tile exposes — MFRAGS (16-row M-frags/warp), BK, As pad,
`cp.async` stages, static/dynamic smem, A-`ldmatrix`, and the transposed-Bs
B-`ldmatrix` (bpad). Best-of-3 GFLOP/s; **bold = per-shape winner**:

| config (frag-mode 2 = ldmatrix-A unless noted)                    |  512³ | 1024³ | 2048³ | 4096³ |
|-------------------------------------------------------------------|------:|------:|------:|------:|
| MMM default (MFRAGS=1 BK=32 stages=2 scalar+cvt)                  |  7573 | 11650 | 13184 | 11614 |
| MFRAGS=1 BK=32 pad=8 stages=2 (swizzled, ldmA)                    | 10082 | 16716 | 18909 | 10625 |
| MFRAGS=2 BK=32 pad=8 stages=2 dyn                                 |**12911**| 20667 | 25184 | 13053 |
| MFRAGS=2 BK=32 pad=8 stages=1 static                              | 10043 |**23187**| 27294 | 23744 |
| MFRAGS=4 BK=32 pad=8 stages=2 dyn (98 KB)                         |  8744 | 21382 |**30256**| 22461 |
| MFRAGS=4 BK=32 pad=8 stages=1 static (49 KB)                      |  7082 | 15914 | 20388 | 17133 |
| MFRAGS=4 bpad=4 stages=1 dyn B-ldmatrix (`mmm-wide-b-m4-s1`)      |  6845 | 22009 | 27688 |**27961**|
| MFRAGS=4 bpad=0 stages=1 dyn B-ldmatrix                           |  4744 | 12585 | 17647 | 19331 |
| MFRAGS=4 bpad=4 stages=2 dyn B-ldmatrix (100 KB)                  |  6884 | 14501 | 18226 | 17533 |
| MFRAGS=2 bpad=4 stages=1 dyn B-ldmatrix (30 KB) — **new**         |  9346 |**25155**| 22215 | 22732 |

The per-shape optimum **moves with the shape** — the small-M wide tile
(MFRAGS=2, double-buffered scalar-B) wins at 512³ (32 blocks); the newly-swept
MFRAGS=2 **single-buffer B-`ldmatrix`** tile wins at 1024³ (25.2 TF), where its
30 KB tile fits ~5 blocks/SM and the B-`ldmatrix` cuts the B-feed the double-buffer
variant could not afford; the MFRAGS=4 double-buffered scalar-B tile wins at 2048³
(30.3 TF); and the Orin flagship `mmm-wide-b-m4-s1` (MFRAGS=4 B-`ldmatrix`,
single-buffer) takes the crown at 4096³ (28.0 TF), where its 2-blocks/SM occupancy
pays off. No single committed config is best everywhere, which is exactly what the
shape-keyed autotuner (`habu-feed-mma-config`) is for. BK=64 on the wide tiles was
also swept this session and **loses at every shape** (128×64 BK=64 s1 dyn:
10.1/17.2/19.4/11.6 TF — the doubled staged smem cuts occupancy more than the halved
`bar.sync` count saves), so it is not tabled.

### Occupancy is NOT the 512³ lever — the referee refutes it (measured)

The campaign opened on the hypothesis that the loud 512³ gap (12.9 vs 21.7) is raw
under-occupancy: too few blocks for 48 SMs. The Triton referee's **own winning
config refutes it**. Triton's tf32 autotune winners (from the referee's printed
`best[…]`, this session on the GB10):

| shape | Triton winner        | grid (blocks) | num_warps | num_stages |
|-------|----------------------|--------------:|----------:|-----------:|
| 512³  | BM128 BN64 BK32      | 4×8 = **32**  |     **4** |      **5** |
| 1024³ | BM128 BN64 BK32      | 8×16 = 128    |     **4** |      **4** |
| 2048³ | BM64  BN128 BK32     | 32×16 = 512   |     **4** |      **3** |
| 4096³ | BM128 BN256 BK32     | 32×16 = 512   |       8   |      **3** |

At 512³ Triton's winner is a **128×64 tile launching 32 blocks — the identical
block count as Habu's winning MFRAGS=2 128×64 tile** (512/128 · 512/64 = 4·8 = 32).
Both leave 16 of 48 SMs idle; both are equally "under-occupied". Yet Triton reaches
21.7 and Habu 13.0. The 512³ gap is therefore **not** block count / occupancy — it is
**per-block tensor throughput**, and the referee names the two levers Habu's tile
family structurally lacks:

1. **num_warps = 4, not 8.** Triton's tf32 winner runs the 128×64 tile with **4
   warps** (128 threads) at 512³/1024³/2048³; Habu's `cg-mma.f` is hardwired to **8
   warps** (256 threads, the 4×2 warp grid, `warp_row = warpid>>1`, `warp_col =
   warpid&1`, `BN` fixed at 64). Halving the warps doubles the per-thread register
   tile and MMA-issue density and lets **2 blocks co-reside per SM** at the same tile,
   which is where Triton's per-block edge comes from at the small shapes. This is a
   *different kernel*, not a knob: the fragment→lane map, the 16·MFRAGS accumulator
   layout, and the D-fragment store map all assume the 8-warp partition. It is the
   `habu-mma-warp-shape` lever (a 4-warp / narrower-`BN` tile variant), deliberately
   **not** attempted this session because it is a from-scratch second tile whose
   element-exactness must be re-proven from the lane map up (the course's #1
   "correct-in-NumPy, garbage-on-device" trap), and the schedule-knob sweep — the
   implementable lever — was exhausted first per the campaign's time-box.
2. **num_stages = 3–5, not 2.** Triton pipelines the K-loop **3–5 deep**; Habu's
   `MMA-PIPE-KLOOP-WITH` maxes at **2** (double-buffer). Deeper staging hides the
   `cp.async` latency Triton's small tiles ride on. On the 99 KB-capped GB10, Habu's
   *wide* tiles (57–100 KB at stages=2) cannot afford 3+ full smem buffers without
   dropping to 1 block/SM — so num_stages≥3 is only reachable **after** the narrower
   4-warp tile of lever 1 shrinks the per-stage footprint. The two levers are coupled;
   lever 1 is the prerequisite.

**Why we (still) lose, stated plainly:** the schedule-knob space the current tile
family exposes (MFRAGS, BK, pad, stages∈{1,2}, static/dynamic smem, A-/B-`ldmatrix`,
bpad) has now been swept to exhaustion on the GB10; the best honest tf32 numbers are
**0.59× / 0.75× / 0.80× / 0.62×** of Triton 3.8 across 512³…4096³. The residual gap is
not a codegen defect (the SASS is native `HMMA`/`LDGSTS`, 0 spills, `docs/codegen-
verdict.md`) and not occupancy (refuted above) — it is the **8-warp / 2-stage
structural ceiling** of the tile. Closing it requires the `habu-mma-warp-shape`
4-warp tile (then deeper `num_stages`), which is the recorded next lever, not another
knob turn.

### fp16 — the Triton reference (Habu's fp16 tile is built in Round 5 below)

When this section was first written Habu's `cg-mma.f` emitted **only**
`mma.sync.aligned.m16n8k8…f32.tf32.tf32.f32` and fp16 was a Triton-only reference.
**Round 5 (below) builds the `m16n8k16.f16.f16.f32` tile and makes this a
head-to-head**; the Triton fp16 column here is the referee it is scored against:

| TFLOP/s |  512³ | 1024³ | 2048³ | 4096³ |
|---------|------:|------:|------:|------:|
| Triton 3.8 fp16 `tl.dot` |  27.4 |  73.8 |  85.8 |  89.1 |

Triton's fp16 peaks at ~89 TF (4096³) — ~2× its own tf32, ~89 % of the ~100 TF
fp16 roof (the tf32:fp16 = 1:2 Blackwell ladder, `docs/codegen-verdict.md`). Fewer
of its fp16 configs bust the smem cap (13 of 45 fail) than the wider tf32 tiles (31
of 45), since fp16 halves the per-tile smem — the same halving that lets Habu's
Round-5 fp16 MFRAGS=4 8-warp tile fit the static cap.

### What the data earns (and what it does not)

- **Earned, honest, unflattering:** on the GB10 the checked Habu TF32 tile is
  device-correct (element-exact, native `HMMA` SASS per `docs/codegen-verdict.md`)
  and on the tensor-core path, but its **throughput is 0.59–0.80× Triton 3.8**,
  reversing the Orin's 1.60×. The "notoriety number" goes to Triton on this box.
  This session's tuning lifted 1024³ from 0.69× to **0.75×** (the MFRAGS=2
  single-buffer B-`ldmatrix` tile); 512³/2048³/4096³ were unmoved and the peak is
  still 0.80× at 2048³.
- **Why it reverses (the roofline story, not a codegen regression):** Triton's
  `tl.dot` lowers to the same `HMMA` and its autotuner — even with 31/45 configs
  pruned by the smem cap — finds a **4-warp, 3–5-stage** pipelined small tile that
  nearly saturates the tf32 roof (91 % at 4096³). Habu's wide tile, tuned to
  amortize the B-feed on the 8-SM Orin, plateaus at 56–61 % of roof on the bigger
  part. The emitter is clean (`docs/codegen-verdict.md`: 0 spills, native
  `LDGSTS`/`HMMA`, `LDS.128`); the gap is **not** occupancy (the referee refutes it,
  above — Triton's 512³ winner launches the same 32 blocks) but the **8-warp /
  2-stage structural ceiling** of the tile, the `docs/compute-campaign.md` work
  reproduced on a higher roof, not a Blackwell codegen defect.
- **Not earned:** any "Habu beats Triton" claim on the GB10 (it does not on
  tf32), and any fp16 head-to-head (Habu has no fp16 tile). The 0.80× peak is the
  honest floor-vs-ceiling gap for the current tile family after the schedule-knob
  space was swept to exhaustion this session; closing it needs the
  `habu-mma-warp-shape` 4-warp tile then deeper `num_stages` (the recorded next
  lever), not another knob turn — see "Occupancy is NOT the 512³ lever" above.
  **Round 2 below builds that 4-warp tile (peak 0.80→0.83×) and falsifies the
  "deeper `num_stages`" half of the prediction.**

## Round 2 — the 4-warp tile (dot habu-4-warp-mma): the structural lever, built and measured (2026-07-19)

Round 1 named the `habu-mma-warp-shape` 4-warp tile as the prerequisite next
lever and deferred it as "a from-scratch second tile whose element-exactness must
be re-proven from the lane map up." This round builds it. `lib/ptx/cg-mma.f` now
parameterizes the warp grid with `MMA-WARPS`: the legacy tile is the 4×2 grid
(WROWS=4, 256 threads) and the new tile is the 2×2 grid (WROWS=2, 128 threads),
with WCOLS fixed at 2 (`warp_col = warpid&1` selects one of the two 32-col halves
of BN=64; `warp_row = warpid>>1` selects one of WROWS row-blocks). The key finding
that made this a surgical parameterization rather than a rewrite: **the per-warp
fragment→lane map, the 16·MFRAGS accumulator layout, and the D-fragment store map
are independent of the warp count** — only the number of warp-rows and the
`cp.async` thread-count partition (`MMA-NTHREADS = WARPS·32 = 128`) change. So the
4-warp `BM128×BN64` tile is exactly MFRAGS=4 on the 2×2 grid: each of 128 threads
owns the same **64 accumulators/lane** as the 8-warp MFRAGS=4 tile, over **half**
the block rows and **half** the per-block smem — which is the whole point (more
blocks per SM at the same per-thread tile; Triton's per-shape tf32 winners run
this exact 4-warp / `BM128×BN64` blocking, see the referee table above).

**Correctness first.** Every 8-warp config stays **byte-identical** (proven by an
emit diff of default + SWZ + wide + wide-B kernels: empty). The 4-warp tiles are
proven **element-exact** before any timing (`tools/ptx/mma-gemm-check.f`: 6 new
`MGC-CFG-W4*` rows at 128³/256³ and 6 deep-stage rows at 256³/512³, 0 mismatches
on the GB10), and a new `E-MMA-WARPS` emit guard fails closed on an illegal warp
grid (WARPS∉{4,8}, or WARPS=4 without the wide MFRAGS>1 staging) — a new negative
regression pins it.

### Result — the 4-warp geometry wins the small/mid shapes (best-of-3, 2411 MHz, ldmatrix-A)

| TFLOP/s (tf32, C=A·B)          |  512³ | 1024³ | 2048³ | 4096³ |
|--------------------------------|------:|------:|------:|------:|
| round-1 8-warp best            |  12.9 |  25.2 |  30.3 |  28.0 |
| **round-2 best**               |**14.5**|**27.7**|**31.5**| 28.2 |
| winning tile                   | 4w M4 s2 | 4w M4 s1 | 4w M4 s1 | 8w M4-Bldm s1 |
| Triton 3.8 `tl.dot`            |  21.7 |  33.5 |  37.8 |  45.3 |
| **Habu / Triton**              |**0.67×**|**0.83×**|**0.83×**| 0.62× |

The 4-warp `BM128×BN64` tile takes 512³ (14.5, **+12 %**, 0.59→0.67×), 1024³
(27.7, **+10 %**, 0.75→0.83×), and 2048³ (31.5, **+4 %**, 0.80→0.83×). At 4096³
the 8-warp 256-row B-`ldmatrix` tile still wins (28.2 vs the best 4-warp 25.5):
with 512 blocks occupancy no longer binds, so the wider tile's larger per-block
B-reuse is more efficient. The **peak head-to-head ratio rises 0.80→0.83×**;
Triton still wins every shape. Two 4-warp tiles split the wins by occupancy regime
(GB10 caps from `cudaDeviceGetAttribute`: **100 KB smem/SM**, 65536 reg/SM, 48 SM;
ptxas, 0 spills):

- **stages=1 static** — 128 reg/thread, 28672 B → **3 blocks/SM** (12 warps) —
  wins 1024³/2048³, where enough blocks launch (128, 512) to fill the SMs and
  occupancy is the constraint;
- **stages=2 dynamic** — 96 reg/thread, 57344 B → **1 block/SM** — wins 512³,
  where only 32 blocks launch (< 48 SMs) so occupancy never binds and the
  double-buffer's cp.async/compute overlap is the lever (the same 32-block regime
  the round-1 referee analysis flagged).

### Deeper staging (3–5) does NOT help — a measured negative

Round 1 hypothesized that the 4-warp tile's smaller footprint would let
`num_stages≥3` fit "while keeping ≥2 blocks/SM," mirroring Triton's 3–5-stage
winners. That was **built** — `MMA-PIPE-KLOOP-MULTI`, a general N-stage `cp.async`
ring pipeline (prologue issues N-1 stages; steady `wait_group(N-1)`; a draining
`wait_group(N-2…0)` epilogue for the last N-1 tiles), proven element-exact at
stages 3/4/5 — and **measured uniformly slower** than stages 1/2 (single run):

| TFLOP/s (tf32) config      |  512³ | 1024³ | 2048³ | 4096³ | smem/block | blocks/SM |
|----------------------------|------:|------:|------:|------:|-----------:|:---------:|
| 4w M4 s3 ldmA (128×64)     |  13.1 |  21.4 |  25.6 |  13.6 |   86016 B  |     1     |
| 4w M2 s3 ldmA (64×64)      |  10.5 |  18.5 |  21.6 |   7.2 |   55296 B  |     1     |
| 4w M2 s4 ldmA (64×64)      |  10.9 |  18.3 |  22.1 |   7.4 |   73728 B  |     1     |
| 4w M2 s5 ldmA (64×64)      |  10.9 |  18.3 |  20.8 |   7.1 |   92160 B  |     1     |

The mechanism is the GB10's **100 KB smem/SM**: the 99 KB per-block cap lets one
big buffer fit, but 3+ full buffers (≥55 KB even on the 64-row MFRAGS=2 tile)
exceed half the SM and force **1 block/SM** — so deeper staging *loses* exactly the
occupancy the 4-warp tile was supposed to buy. Only stages≤2 keep ≥2 blocks/SM on
these tiles. This **falsifies the "deeper `num_stages` after the 4-warp shrink"**
half of the round-1 prediction on this device: the 4-warp *geometry* is the lever;
pipeline *depth* is not. It is consistent with the Orin step-3c result that this
tile is **mma-issue-bound, not `cp.async`-feed-bound** — extra pipeline depth hides
a latency that was never the bottleneck, at an occupancy cost that is. (The
pipeline is kept, proven, and selectable; it is simply not a win on this device.)

### Next lever (the honest why we still trail 0.62–0.83×)

The residual gap is now neither warp count nor stage depth (both swept to
exhaustion) nor occupancy (the static 4-warp tile matches Triton's block count AND
gets 3 blocks/SM). Triton's tf32 `tl.dot` still out-throughputs Habu per block.
The remaining structural differences in `cg-mma.f`, in likely-payoff order and each
a kernel-engineering change of the same class as this round's warp-grid rework
(to be proven element-exact and measured before any number is claimed):

1. **A shared-memory epilogue.** Habu stores each lane's D fragments straight to
   global with 4 scattered `st.global.f32` per n-tile (uncoalesced 4-byte writes);
   Triton stages the accumulator tile back through smem and writes C coalesced.
   The store is a measurable fraction of a compute-light small-shape launch.
2. **A wider tensor op / higher mma-issue density.** Habu issues
   `mma.sync.m16n8k8`; the Blackwell `tl.dot` path uses a wider/denser HMMA
   schedule, so more FLOPs retire per issue slot — the mma-issue bound the
   ablation points at.
3. **Register-tile scheduling headroom.** The static 4-warp tile is at 128
   reg/thread with 0 spills — near the point where a larger tile would spill, so
   further M/N widening needs a scheduling change, not just a knob.

### Reproduction (exact)

```
# Habu column — element-exact correctness, then throughput (arch auto-probed sm_121a):
bin/hb --load tools/ptx/mma-gemm-check.f      # MGC-ALL: PASS element-exact 64³…512³ (incl. 4-warp + N-stage rows)
bin/hb --load tools/ptx/gemm-bench.f          # GB-GB10: FP32 roof + 8-warp sweep + 4-warp winners, 512…4096
# GB-W4-SWEEP (in the same file) reproduces the full 4-warp exploration incl. the deep-stage negative.
# Triton referee (source-built 3.8 in the ml venv):
~/Work/ml/.venv/bin/python /tmp/gemm-triton-gb10.py
```

### Triton referee script (`/tmp/gemm-triton-gb10.py`, verbatim)

Out-of-tree per this doc's convention (the repo's `tools/host-lint.f` rejects a
committed `.py`), the same way the Orin step-1 `/tmp/triton_matmul.py` is quoted
verbatim above.

```python
import torch, triton, triton.language as tl
DEV = 'cuda'

# broad autotune grid (max-autotune scope): (BM, BN, BK, warps) x stages {3,4,5}
def configs():
    base = [
        (128, 256, 64, 8), (256, 128, 64, 8), (128, 128, 64, 8), (128, 128, 32, 8),
        (128, 64, 64, 4), (64, 128, 64, 4), (128, 256, 32, 8), (256, 128, 32, 8),
        (128, 64, 32, 4), (64, 128, 32, 4), (128, 128, 128, 8), (64, 64, 64, 4),
        (256, 64, 64, 4), (64, 256, 64, 4), (128, 128, 64, 4),
    ]
    return [dict(BM=bm, BN=bn, BK=bk, GROUP=8, stages=st, warps=w)
            for bm, bn, bk, w in base for st in (3, 4, 5)]

@triton.jit
def matmul_kernel(a_ptr, b_ptr, c_ptr, M, N, K,
                  stride_am, stride_ak, stride_bk, stride_bn, stride_cm, stride_cn,
                  BM: tl.constexpr, BN: tl.constexpr, BK: tl.constexpr, GROUP: tl.constexpr,
                  TF32: tl.constexpr):
    pid = tl.program_id(0)
    num_pid_m = tl.cdiv(M, BM); num_pid_n = tl.cdiv(N, BN)
    num_pid_in_group = GROUP * num_pid_n
    group_id = pid // num_pid_in_group
    first_pid_m = group_id * GROUP
    group_size_m = min(num_pid_m - first_pid_m, GROUP)
    pid_m = first_pid_m + ((pid % num_pid_in_group) % group_size_m)
    pid_n = (pid % num_pid_in_group) // group_size_m
    offs_am = (pid_m * BM + tl.arange(0, BM)) % M
    offs_bn = (pid_n * BN + tl.arange(0, BN)) % N
    offs_k = tl.arange(0, BK)
    a_ptrs = a_ptr + (offs_am[:, None] * stride_am + offs_k[None, :] * stride_ak)
    b_ptrs = b_ptr + (offs_k[:, None] * stride_bk + offs_bn[None, :] * stride_bn)
    acc = tl.zeros((BM, BN), dtype=tl.float32)
    for k in range(0, tl.cdiv(K, BK)):
        a = tl.load(a_ptrs, mask=offs_k[None, :] < K - k * BK, other=0.0)
        b = tl.load(b_ptrs, mask=offs_k[:, None] < K - k * BK, other=0.0)
        acc = tl.dot(a, b, acc, input_precision=('tf32' if TF32 else 'ieee'))
        a_ptrs += BK * stride_ak
        b_ptrs += BK * stride_bk
    offs_cm = pid_m * BM + tl.arange(0, BM)
    offs_cn = pid_n * BN + tl.arange(0, BN)
    c_ptrs = c_ptr + stride_cm * offs_cm[:, None] + stride_cn * offs_cn[None, :]
    c_mask = (offs_cm[:, None] < M) & (offs_cn[None, :] < N)
    tl.store(c_ptrs, acc.to(c_ptr.dtype.element_ty), mask=c_mask)

def run_cfg(a, b, c, cfg, tf32):
    M, K = a.shape; _, N = b.shape
    grid = (triton.cdiv(M, cfg['BM']) * triton.cdiv(N, cfg['BN']),)
    matmul_kernel[grid](a, b, c, M, N, K, a.stride(0), a.stride(1), b.stride(0),
                        b.stride(1), c.stride(0), c.stride(1),
                        BM=cfg['BM'], BN=cfg['BN'], BK=cfg['BK'], GROUP=cfg['GROUP'],
                        TF32=tf32, num_stages=cfg['stages'], num_warps=cfg['warps'])

def event_time(fn, warmup=25, reps=100):
    for _ in range(warmup): fn()
    torch.cuda.synchronize()
    s = torch.cuda.Event(enable_timing=True); e = torch.cuda.Event(enable_timing=True)
    s.record()
    for _ in range(reps): fn()
    e.record(); torch.cuda.synchronize()
    return s.elapsed_time(e) / reps

def bench(S, dtype, tf32, iters):
    torch.manual_seed(0)
    a = torch.randn(S, S, device=DEV, dtype=dtype)
    b = torch.randn(S, S, device=DEV, dtype=dtype)
    cdt = torch.float16 if dtype == torch.float16 else torch.float32
    c = torch.empty(S, S, device=DEV, dtype=cdt)
    ref = torch.matmul(a, b)                 # same-dtype torch reference
    best_ms, best_cfg, ok, fail = 1e30, None, 0, 0
    for cfg in configs():
        try:                                  # a config that busts the smem cap is caught + counted
            run_cfg(a, b, c, cfg, tf32); torch.cuda.synchronize()
            ms = event_time(lambda: run_cfg(a, b, c, cfg, tf32), warmup=10, reps=max(20, iters))
            ok += 1
            if ms < best_ms: best_ms, best_cfg = ms, cfg
        except Exception:
            fail += 1
    run_cfg(a, b, c, best_cfg, tf32); torch.cuda.synchronize()
    rel = (c.float() - ref.float()).abs().max().item() / ref.float().abs().max().item()
    ms = event_time(lambda: run_cfg(a, b, c, best_cfg, tf32), warmup=25, reps=iters)
    return (2 * S ** 3) / (ms * 1e-3) / 1e9, best_cfg, rel, ok, fail

if __name__ == '__main__':
    print(f"torch {torch.__version__} | triton {triton.__version__} | "
          f"dev {torch.cuda.get_device_capability()} {torch.cuda.get_device_name()}")
    torch.backends.cuda.matmul.allow_tf32 = True
    shapes = [(512, 200), (1024, 120), (2048, 60), (4096, 40)]
    for label, dtype, tf32 in [("tf32", torch.float32, True), ("fp16", torch.float16, False)]:
        print(f"\n== Triton 3.8 {label} (manual max-autotune, CUDA-event warm timing) ==")
        for S, iters in shapes:
            g, cfg, rel, ok, fail = bench(S, dtype, tf32, iters)
            print(f"Triton {label} GEMM {S}x{S}x{S} GFLOP/s={g:.1f} rel_err={rel:.2e} "
                  f"best[BM{cfg['BM']}xBN{cfg['BN']}xBK{cfg['BK']} st{cfg['stages']} "
                  f"w{cfg['warps']}] cfgs_ok={ok} cfgs_failed={fail}")
```

## Round 3 — the shared-memory C epilogue (dot habu-shared-mem-epilogue): built and measured (2026-07-19)

Round 2 named a **shared-memory C epilogue** as the highest-payoff remaining lever
(lever 1 of its "Next lever" list): Habu stored each lane's D fragments straight to
global with four scattered `st.global.f32` per n-tile (uncoalesced 4-byte writes),
while Triton stages the accumulator tile back through smem and writes C coalesced.
This round builds it. `lib/ptx/cg-mma.f` gains a selectable `MMA-EPILOG` knob (off by
default, like every other tile option). When on, after the K-loop each lane writes its
`16·MFRAGS` D-fragment accumulators into a block-local `[BROWS][BN]` staging tile in
shared memory — the **same `SH` region the `cp.async` pipeline used**, dead after the
last compute — then the whole block re-reads the tile and writes C in coalesced 128-byte
lines (element `e = tid_lin + m·NTHREADS` → row `e/BN`, col `e%BN`, so a warp's 32 lanes
hit 32 contiguous C columns). It trades the store's uncoalesced 4-byte global writes for
one strided smem write + one coalesced global write, paying two block barriers. The
lane→element map is the D-fragment map already proven exact, so **no new mapping** is
introduced — only staging-address arithmetic and a coalesced drain.

**Correctness first.** Every existing config stays **byte-identical** with the epilogue
off (proven by an emit diff of 33 configs spanning default / SWZ / dyn / wide MFRAGS
2&4 / wide-B / 4-warp / deep-stage: empty). The epilogue is proven **element-exact**
before any timing (`tools/ptx/mma-gemm-check.f`: 5 `MGC-CFG-*-EPI` rows at 128³/256³ on
**both** the 8-warp and 4-warp grids, plus a B-`ldmatrix`+epilogue row at 256³/512³ — 0
mismatches on the GB10, scalar+cvt and `ldmatrix` cross-checked). Because the staging
tile is `BROWS·BN·4` bytes and `SH` is sized to the larger of the pipeline and the
staging tile, a tile whose staging busts the `.shared` budget fails closed at emit time
with a named `E-MMA-EPI` throw (new negative regression: the 8-warp MFRAGS=4 **static**
tile stages 64 KB > the 48 KB static cap → throws; the 4-warp MFRAGS=4 static tile stages
32 KB, grows `SH` 28672→32768 B, emits).

### Result — the epilogue wins the compute-light shapes, exactly as predicted

| TFLOP/s (tf32, C=A·B)          |  512³ | 1024³ | 2048³ | 4096³ |
|--------------------------------|------:|------:|------:|------:|
| round-2 best                   |  14.5 |  27.7 |  31.5 |  28.2 |
| **round-3 best (+epilogue)**   |**16.3**|**29.1**|**31.7**| 28.2 |
| winning tile                   | 4w M4 s2 **+epi** | 4w M4 s1 **+epi** | 4w M4 s1 **+epi** | 8w M4-Bldm s1 (no epi) |
| Triton 3.8 `tl.dot`            |  21.7 |  33.5 |  37.8 |  45.3 |
| **Habu / Triton**              |**0.75×**|**0.87×**|**0.84×**| 0.62× |

The epilogue lifts 512³ (14.5 → **16.3**, **+12.5 %**, 0.67→**0.75×**) and 1024³ (27.7 →
**29.1**, **+4.9 %**, 0.83→**0.87×**), is flat at 2048³ (31.6 → 31.7, +0.3 %), and the
**peak head-to-head ratio rises 0.83 → 0.87×** at 1024³. The size ordering of the effect
is the whole story and it matches the round-2 prediction: **the biggest lift is at 512³,
the compute-light launch where the store is the largest fraction of the kernel**; as the
shape grows the store hides behind more compute and the gain shrinks to zero. The
epilogue-off baselines reproduced the round-2 winners within best-of-3 noise this session
(4w M4 s2 512³ 14.5, 4w M4 s1 1024³/2048³ 27.7/31.6, 8w wide-B 4096³ 28.1), so the
same-session A/B isolates the store change.

### The honest negative — the epilogue *hurts* the 256-row 8-warp tile

The 4096³ round-2 winner is the 8-warp MFRAGS=4 256×64 B-`ldmatrix` tile, and turning the
epilogue **on** there is a large **regression** at every shape but 512³:

| TFLOP/s (tf32) 8w M4 B-ldmatrix s1 |  512³ | 1024³ | 2048³ | 4096³ | SH bytes |
|------------------------------------|------:|------:|------:|------:|---------:|
| epilogue **off** (round-2 winner)  |   6.9 |  22.0 |  26.5 |**28.1**|   50176  |
| epilogue **on**                    |   8.2 |  17.4 |  20.5 |  17.4 |   65536  |

The mechanism is **occupancy**: this tile's C staging tile is 256·64·4 = **65536 B**,
larger than its 50176 B pipeline buffer, so `SH` grows to 64 KB and the block's
shared-memory footprint jumps 30 %. On the shapes where enough blocks launch to be
occupancy-bound (1024³ and up) that drop in blocks/SM costs more than the coalesced store
saves — a −20 to −38 % swing. So **4096³ keeps the epilogue-off winner** (28.1 TF, 0.62×
unchanged) and the table's 4096³ column is deliberately not lifted. The epilogue is a win
only where the store fraction is large *and* the staging tile does not evict a block; the
narrow 4-warp 128×64 tiles (staging ≤ 32 KB) satisfy both at 512³/1024³, the 256-row tile
satisfies neither past 512³. This is consistent with the round-2/Orin finding that the
tile is **mma-issue-bound, not store-bound** on the big shapes.

### Next lever

The residual gap (0.75–0.87× on the small/mid shapes, 0.62× at 4096³) is now, in
likely-payoff order: **(1)** a wider/denser HMMA schedule (more FLOPs per mma issue — the
mma-issue bound the ablation points at, still lever 2 from round 2); **(2)** an epilogue
that stages *without* growing `SH` on the 256-row tile (e.g. staging a half-tile at a time
so occupancy is preserved), which would let 4096³ take the coalesced store too. Both are
kernel-engineering changes of the same class as this round, to be proven element-exact and
measured before any number is claimed.

### Reproduction (exact)

```
# element-exact correctness first (arch auto-probed sm_121a), then throughput:
bin/hb --load tools/ptx/mma-gemm-check.f   # MGC-ALL: PASS element-exact incl. 5 MGC-CFG-*-EPI + B-ldmatrix epilogue rows; E-MMA-EPI negative
# swap the file's bottom entry to GEMMBENCH:GB-EPI-SWEEP (as with GB-W4-SWEEP), then:
bin/hb --load tools/ptx/gemm-bench.f       # GB-EPI-SWEEP: each round-2 winner, epilogue OFF then ON, all four shapes
```

Protocol identical to the GB10 head-to-head above: `GB-EPI-SWEEP`, per shape 1 warmup +
ITERS (400, 200, 80, 40) CUDA-event-timed launches, **best of 3 full passes**, run **solo**;
the GB10 held **2411 MHz** sustained during the timed kernels (tight-loop `nvidia-smi`
sample: p50 = p95 = max = 2411 MHz, ≤ 47 °C / ≤ 35 W, not throttled). A = B = 1.0, C = 0
(values immaterial to timing). The Triton column is the same `/tmp/gemm-triton-gb10.py`
referee measured above.

## Round 4 — distinct B registers + burst HMMA (dot habu-distinct-b-registers): built, falsified at the compiler (2026-07-19)

Round 3's residual-gap list named **a wider/denser HMMA schedule** as lever 1 (still the
mma-issue bound rounds 2 and 3 pointed at). The SASS forensic (dot `habu-read-triton-s`)
made that concrete: Habu's wide K-substep reuses ONE B-register pair (`%r54,%r55`) for every
n-tile, so each n-tile's B load is WAR-blocked behind the previous n-tile's four mmas
(load-to-use ~2 instructions), and ptxas inserts ~40 stall NOP per 64 HMMA in the steady
K-loop where Triton's SASS has none. This round tests that mechanism: give each n-tile its own
B register pair (eight regs for four n-tiles, placed just past the A groups at
`%r(6·MFRAGS+48+2j)`) and restructure the substep to load **all** A fragments and **all** B
fragments — each into a distinct register — **before** the mma burst, so no load is WAR-blocked
and ptxas can hoist the shared loads.

**The lever was built and proven element-exact, then falsified by the NOP count.** The scout's
caveat was explicit ("ptxas already allocated distinct physical regs yet still stalled, so the
win must be proven by the NOP count then the clock, not assumed"), so the protocol was: emit the
round-3 winner tiles, `ptxas -arch=sm_121a`, `cuobjdump -sass`, count NOP between HMMA in the
steady body — **before** touching the emitter and **after**. The three round-3 winning tiles
(the 512³ tile 4-warp M4 stages=2 dyn +epi; the 1024³ and 2048³ tile 4-warp M4 stages=1 static
+epi; the 4096³ tile 8-warp M4 B-ldmatrix stages=1 dyn) were measured both ways:

| round-3 winning tile (shape)          | regs | NOP/HMMA steady, before | NOP/HMMA steady, after |
|---------------------------------------|-----:|:-----------------------:|:----------------------:|
| 4-warp M4 s1 static +epi (1024³,2048³)|  128 |       40 / 64           |       40 / 64          |
| 4-warp M4 s2 dyn +epi (512³)          |   96 |       41 / 64           |       41 / 64          |
| 8-warp M4 B-ldmatrix s1 dyn (4096³)   |   96 |       40 / 64           |       40 / 64          |

**The NOP count did not move at all**, and the reason is airtight: with SASS register numbers
and addresses normalized, the emitted opcode streams are **byte-identical** before and after
(diff = 0 lines, over 898, 1017 and 519 opcodes respectively). ptxas already renames the single
virtual `%r54,%r55` B pair into distinct **physical** registers per n-tile (the pre-change SASS
shows the four n-tile B operands as four different physical registers within a substep) and
schedules identically whether the PTX hands it one shared virtual pair or four distinct pairs
with the loads hoisted. Handing ptxas distinct virtual registers changes only the register
*names* it assigns, not the machine code. Register pressure was unchanged (128 and 96 reg, zero
spills, same occupancy). The restructured emitter was cross-checked element-exact on the GB10
(`tools/ptx/mma-gemm-check.f`: every affected wide config — both warp grids, MFRAGS 2 and 4,
ldmatrix and scalar modes, with and without the epilogue — 0 mismatches, and all five emit
legality guards still fail-closed), so it is a *correct* transform; it is simply a no-op at the
machine level.

**Conclusion: the ~40-NOP steady body is the `HMMA.1688.F32.TF32` tensor-core issue latency, not
a B-register WAR hazard.** The tensor core accepts a new HMMA roughly every other cycle; ptxas
fills the mandatory gap with a fragment load where one is available and a NOP where none is, and
the number of NOP is invariant to how the loads are named or ordered. This falsifies the
distinct-B-register lever at the compiler level — no device timing is warranted (the protocol
stops before the clock when the NOP count does not move), and there is no regime where the change
could help or hurt, because it is the same emitted schedule. It is fully consistent with the
rounds 2 and 3 finding that this tile is **mma-issue-bound, not fragment-feed-bound**: the real
lever 1 remains a genuinely *wider or denser* tensor op (more FLOPs retired per issue slot), which
is a different HMMA shape, not a register-allocation or load-scheduling change. The inert emitter
restructure was therefore **not landed** (it would add substep complexity and grow the wide `.reg
.b32` header for zero machine-level effect); this section is the record of the falsified lever.

### Reproduction (exact)

```
# Emit each round-3 winner tile's MMM PTX at its knobs (BK=32 pad=8; the tiles above), capture via
# PTX-CAPTURE-ON EMIT-MATMUL-MMA (the tools/ptx/gemm-bench.f GB-MMM-CFGW4-EPI / GB-MMM-CFGW-B config
# setters select these exact tiles), then on the GB10:
ptxas -arch=sm_121a -o k.cubin k.ptx
cuobjdump -sass k.cubin        # count NOP between the 64 HMMA.1688.F32.TF32 of the steady K-loop body
# Element-exact device cross-check of the restructured emitter (all wide configs):
bin/hb --load tools/ptx/mma-gemm-check.f   # MGC-ALL: 0 mismatches; legality guards fail-closed
```

## Round 5 — the fp16 `m16n8k16` tile (dot `habu-fp16-mma-tile`): built and measured (2026-07-19)

Round 4 falsified the register-scheduling levers and named the *real* remaining lever
precisely: **"a genuinely wider or denser tensor op — more FLOPs retired per issue
slot — which is a different HMMA shape."** This round builds exactly that. `cg-mma.f`
gains a selectable `MMA-DTYPE` knob (off by default, like every other tile option):
`0` = the existing TF32 `mma.sync.aligned.m16n8k8…f32.tf32.tf32.f32`; `1` = a **new
fp16 `mma.sync.aligned.m16n8k16.row.col.f32.f16.f16.f32`** tile — A/B stored as `f16`
halves in both global and shared (the host packs `f32→f16` on the fill path,
`lib/ptx/cg.f` `F64>F16`/`F16-PACK`), accumulate stays `f32`, C stays `f32`. The
`m16n8k16` shape retires **twice the K per HMMA** (8 A-halves + 4 B-halves per lane in
the *same* 4+2 `.b32` register budget as tf32's 4+2), so it issues **half** the mmas per
K-tile — the denser tensor op Round 4 pointed at.

**Correctness first, and the tf32 path is untouched.** With `MMA-DTYPE=0` every existing
config stays **byte-identical** (an emit diff of 35 configs spanning default / SWZ / dyn
/ wide MFRAGS 2&4 / wide-B / 4-warp / deep-stage / epilogue: empty). The fp16 tile is
proven **element-exact** before any timing (`tools/ptx/mma-gemm-check.f` `MGC-CFG-F16`:
eight configs at the block-M-aware edges — **both** warp grids at 128³/256³, the non-wide
64³/128³, a 256³/512³ MFRAGS=4 tile, and two epilogue combos — device-verified on the
GB10, zero mismatches). The tolerance is a **justified zero**: the fill's small integers
(1..13, 1..11) are exact in f16's 11-bit significand, each product is ≤ 143, and the f32
K-accumulation never exceeds 512·143 = 73 216 < 2²⁴, so no add rounds and the device `f32`
C equals the `f64` reference exactly (the compare requires `err = 0.0`, no epsilon; the
argument is in the check-file header). The C/D fragment map is identical to the tf32 tile,
so the scattered store *and* the smem C epilogue are reused verbatim; only the A/B fragment
loads, the mma opcode, and the `f16`-sized staging are new. A fail-closed guard
(`E-MMA-DTYPE`, negative-tested) rejects fp16 combined with a feed knob wired only for the
tf32 fragment format (A-`ldmatrix`, transposed-Bs B-`ldmatrix`, wide ablation).

### Result — fp16 is ~1.5× Habu's own tf32 on the compute-bound shapes, but still trails Triton

Best-of-3, run solo; the same-session FP32 CUDA-core reference (`MM`) reproduced the
committed head-to-head values within ~1 % (8.2 / 13.1 / 15.1 / 12.9 vs 8.2 / 13.2 / 14.9 /
13.0 TFLOP/s), so the session clock matches the tf32 head-to-head baseline (tight-loop
`nvidia-smi` p50 read 2340 MHz, ≤ 43 °C / ≤ 43 W, not throttled). **Bold = fp16 per-shape
winner** (the 4-warp MFRAGS=4 `BM128×BN64` tile sweeps every shape — stages=2 dyn at
512³–2048³, stages=1 static at 4096³):

| TFLOP/s (fp16, C=A·B)          |  512³ | 1024³ | 2048³ | 4096³ |
|--------------------------------|------:|------:|------:|------:|
| **Habu fp16 tile (best)**      |**16.3**|**36.1**|**46.1**|**44.6**|
| Triton 3.8 fp16 `tl.dot`       |  27.4 |  73.8 |  85.8 |  89.1 |
| **Habu / Triton (fp16)**       | 0.59× | 0.49× | 0.54× | 0.50× |
| Habu %-of-fp16-roof (~100 TF)  |   16% |   36% |   46% |   45% |
| (ref) Habu tf32 tile (round 3) |  16.3 |  29.1 |  31.7 |  28.2 |
| **fp16 / own tf32**            | 1.00× | 1.24× |**1.45×**|**1.58×**|

The **fp16-over-own-tf32 multiplier is the finding, and it confirms Round 4's thesis**: the
denser HMMA does nothing at 512³ (1.00×, the occupancy/launch-bound small shape where the
tile is not mma-issue-bound) and grows monotonically to **1.58× at 4096³**, the most
compute-bound shape — exactly where "more FLOPs per issue slot" is the binding lever. So the
tile family *is* mma-issue-bound on the big shapes, as rounds 2–4 argued, and doubling the K
per issue buys a real ~1.5×. It does **not** reach the naive 2× because the fp16 B fragment
is un-transposed: its two K-adjacent halves are one `BN`-row apart in the k-major `Bs`, so
each B register is built from two `ld.shared.u16` + a shift/or rather than one `b32` load —
the per-K B-feed instruction count is unchanged from tf32 while the mma count halved, so the
feed re-weights toward the residual.

**Honest, unflattering headline: on the GB10 the checked Habu fp16 tile reaches 0.49–0.59×
of Triton 3.8's fp16 `tl.dot`** — the same shape of result as the tf32 head-to-head (Triton
wins), and for the same reason (Triton's autotuner finds a 4-warp, 3–5-stage pipelined small
tile that nearly saturates the roof; Habu's 2-stage tile does not). fp16 halving the per-tile
smem does lift occupancy — the MFRAGS=4 **8-warp** 256×64 tile now fits the 48 KB static cap
(20 480 B, vs the tf32 tile's 98 304 B that forced dynamic smem) — but the narrower 4-warp
128×64 tile still wins every shape, and the epilogue helps only 512³ (it ties the winner
there and loses elsewhere), both consistent with the tf32 rounds. The next fp16 lever is the
B feed: a transposed-`Bs` staging (one `b32` load per B register, as the tf32 wave-3 path
does for its B-`ldmatrix`) or an fp16 `ldmatrix.x4`/`ldmatrix.x2` fragment load, either of
which would cut the feed toward the 2× ceiling — a kernel-engineering change of the same
class as the tf32 wave-3 work, to be proven element-exact and measured before any number is
claimed.

### Reproduction (exact)

```
# Element-exact correctness first (arch auto-probed sm_121a), then throughput:
bin/hb --load tools/ptx/mma-gemm-check.f   # MGC-ALL: 8 MGC-CFG-F16 rows PASS element-exact (both warp grids
                                           # 128^3/256^3 + epilogue); E-MMA-DTYPE negative fail-closed; tf32 rows unchanged
# swap the file's bottom entry to GEMMBENCH:GB-F16-SWEEP (as with GB-W4-SWEEP), then:
bin/hb --load tools/ptx/gemm-bench.f       # GB-F16-SWEEP: FP32 roof reference + the fp16 tile across warp grids /
                                           # MFRAGS / stages / epilogue, all four shapes; best of 3 full passes, solo
# tf32 byte-identity (MMA-DTYPE=0 must not move any config):
bin/hb --load tools/ptx/mma-emit-diff.f    # 35-config MMM emit stream; diff base vs branch = empty
```

Protocol identical to the head-to-head above: per shape 1 warmup + ITERS (400, 200, 80, 40)
CUDA-event-timed launches, **best of 3 full passes**, run **solo**. A = B = 1.0, C = 0
(values immaterial to timing). The Triton fp16 column is the same `/tmp/gemm-triton-gb10.py`
referee measured in the fp16 table above.

## Round 6 — the fp16 transposed-`Bs` B feed (dot `habu-fp16-transposed-bs`): a compute-bound-only win (2026-07-19)

Round 5 named the next fp16 lever precisely: the un-transposed B fragment builds each of its two `b32`
registers from two `ld.shared.u16` + a shift/or, because the register's two K-adjacent halves are one
`BN`-row apart in the k-major `Bs`. This round stores `Bs` **transposed** (n-major `BT[n][k]`, K
contiguous) so each register's K-adjacent pair is contiguous and loads as **one `ld.shared.b32`**,
dropping the shift/or — the fp16 analogue of the tf32 wave-3 B-`ldmatrix` transposed-`Bs` feed. `cg-mma.f`
gains a selectable `MMA-BTF16` knob (off by default, like every other tile option). `cp.async` cannot
gather the transpose (a contiguous chunk would scatter across BT rows), so the BT tile is staged by a
scalar transposed `u16` copy (coalesced global read `B[k][n]`, strided shared write `BT[n][k]`) while As
stays a `cp.async` copy — the same split staging the tf32 BLDM path uses. The n-major BT row stride is
`BK+BPAD` halves; `BPAD=8` (stride 40 halves = b32-load start-bank stride 20) is **conflict-free** and is
the load-bearing knob: at `BPAD=0` the 8 `gid` tiles alias a 4-bank window and the tile runs
bank-conflict-bound (the `BPAD=0` row below).

**Correctness first, tf32 and the fp16 default untouched.** With `MMA-BTF16=0` every tf32 config *and*
the fp16 default B feed stay **byte-identical** (`tools/ptx/mma-emit-diff.f`, 35 tf32 + 6 fp16 configs:
empty diff base vs branch). The transposed feed is proven **element-exact** before any timing
(`tools/ptx/mma-gemm-check.f` `MGC-CFG-F16-T`: seven configs on **both** warp grids at the block-M-aware
edges (64³/128³/256³), with and without the epilogue, `BPAD` ∈ {0,8}, device-verified, zero mismatches;
the transpose is a pure permutation of the same integer values, so the justified-zero-tolerance argument
is unchanged). A fail-closed guard (`E-MMA-BTF16`, negative-tested) rejects the transposed feed on a tf32
tile or with a non-4-byte BT row.

### Result — the transposed feed wins only the most compute-bound shape, and loses the small ones

Best-of-3, run solo, sustained 2411 MHz (the same-session FP32 `MM` CUDA-core roof reproduced the tf32
baseline clock). **Bold = per-shape fp16 winner across both B feeds:**

| TFLOP/s (fp16, C=A·B)              |  512³ | 1024³ | 2048³ | 4096³ |
|------------------------------------|------:|------:|------:|------:|
| Habu fp16 k-major B (Round 5)      |**16.3**|**36.1**| 46.1 | 44.6 |
| Habu fp16 transposed-`Bs` (BPAD=8) |  13.1 |  32.3 |**45.2**|**47.2**|
| — same config at BPAD=0            |  10.9 |  22.9 |  27.5 |  29.1 |
| Triton 3.8 fp16 `tl.dot`           |  27.4 |  73.8 |  85.8 |  89.1 |
| **Habu / Triton (fp16, best)**     | 0.59× | 0.49× | 0.53× |**0.53×**|
| **fp16 / own tf32**                | 1.00× | 1.24× | 1.45× |**1.67×**|

The transposed feed is a **regime-split** result, not a strict win, so it ships as a knob (default off),
**not** the fp16 default. It **wins the 4096³ shape** — the most compute-bound, where Round 5 measured
the largest fp16-over-tf32 multiplier — lifting it 44.6 → **47.2 (+5.8 %)**, 0.50× → **0.53×** Triton and
the own-tf32 multiplier 1.58× → **1.67×**; the feed savings (one `b32` load vs two `u16` + shift/or per
register, per K-substep) dominate there and the extra scalar-transpose staging is hidden behind compute.
It is **flat at 2048³** (45.2 vs 46.1) and **regresses the launch/occupancy-bound small shapes** — 1024³
36.1 → 32.3 (−11 %) and 512³ 16.3 → 13.1 (−20 %) — where the added staging (8–16 scalar `u16` transposed
copies/thread per K-tile, vs one `cp.async` 8-half chunk on the k-major B) is exposed rather than hidden.
The `BPAD=0` row is the honest floor: without the pad the b32 loads are 4-way bank-conflicted and the
whole tile runs ~27–29 TFLOP/s, below even the k-major baseline — the conflict-free `BPAD=8` stride is
what makes the feed change pay at all.

So the lever does **not** move the small–mid shapes toward Triton's numbers as Round 5 predicted — that
prediction was wrong for 512³–2048³: the transpose is a staging-vs-feed trade, and only 4096³ is
feed-bound enough to bank it. The honest headline is a **~6 % gain on one shape**, still 0.53× Triton
(the same Triton-wins shape as every prior round), by cutting the fp16 B feed to one `b32` load per
register on the tile where the feed is the binding cost.

### Reproduction (exact)

```
bin/hb --load tools/ptx/mma-gemm-check.f   # MGC-CFG-F16-T rows PASS element-exact (both warp grids, ±epilogue,
                                           # BPAD 0/8); E-MMA-BTF16 negative fail-closed; tf32 + fp16-default rows unchanged
# swap the file's bottom entry to GEMMBENCH:GB-F16-SWEEP (as with GB-F16), then:
bin/hb --load tools/ptx/gemm-bench.f       # GB-F16-SWEEP: k-major + transposed-Bs fp16 configs, all four shapes; best of 3, solo
bin/hb --load tools/ptx/mma-emit-diff.f    # 35 tf32 + 6 fp16-default configs; diff base vs branch = empty
```

## Round 7 — the bf16 `m16n8k16` tile (dot `habu-bf16-m16n8k16-tile`): fp16-class throughput with f32 range (2026-07-19)

Rounds 5–6 built the fp16 `m16n8k16` tile. This round adds **bf16** — the conventional
mixed-precision training dtype (8-bit exponent = full f32 range, so no loss-scaling
gymnastics), wanted for the nanoGPT training path and unblocked by the ratified numerics
policy (reduced precision where the accuracy budget allows). `cg-mma.f`'s `MMA-DTYPE` knob
gains value `2` = a **`mma.sync.aligned.m16n8k16.row.col.f32.bf16.bf16.f32`** tile. bf16
shares the fp16 tile **verbatim**: a bf16 half is 2 bytes exactly like an f16 half, so every
fragment load, cp.async stage, transposed-`Bs` copy, epilogue and store is a pure bit-move —
the *only* differences are the mma operand dtype token (a new `MMA-ABT` word emits `bf16` vs
`f16`) and the host pack. A `bf16`-vs-`f16` emit diff of the same tile config is exactly the 8
mma lines, nothing else.

**Host pack (the one real numeric addition): `F64>BF16`, round-to-nearest-even** (`lib/ptx/cg.f`),
**not truncation.** bf16's exponent field is identical to f32's (8 bits, bias 127), so bf16 is an
f32 with the low 16 mantissa bits removed; `F64>BF16` therefore mirrors `F64>F32` (target exponent
`e−896`, f32's overflow/subnormal bounds) — **not** `F64>F16` (whose `e−1008` bias and tiny range
would be wrong for bf16). The rounding is done in **one** step directly on the 52-bit f64 mantissa
(keep 7 bits, RNE the 45 dropped), the correctly-rounded nearest bf16; it is deliberately **not**
`f64→f32→bf16`, whose double rounding can mis-round a value sitting on an f32 boundary.

**Correctness first, tf32 and the fp16 tile untouched.** With `MMA-DTYPE≠2` every tf32 config
*and* every fp16 config stay **byte-identical** (`tools/ptx/mma-emit-diff.f`, 35 tf32 + 6 fp16
configs: empty diff base vs branch; the 6 bf16 rows are appended after). The bf16 tile is proven
**element-exact** before any timing (`tools/ptx/mma-gemm-check.f`: 8 `MGC-CFG-BF16` + 7
`MGC-CFG-BF16-T` rows — **both** warp grids at the block-M-aware edges (64³/128³/256³/512³), with
and without the epilogue, **both** B feeds (k-major and the `MMA-BTF16` transposed n-major BT),
`BPAD` ∈ {0,8} — device-verified on the GB10, **zero mismatches**). The tolerance is a **justified
zero**, argument adapted to bf16's narrower significand: bf16's significand is 8 bits (7 + implicit),
so the fill's integers (1..13 A, 1..11 B, all ≤ 256) are exact and `BF16-PACK` narrows them with no
error; each product is an integer ≤ 143 and the K-accumulation runs in **f32** (never bf16), whose
every partial ≤ 512·143 = 73 216 < 2²⁴, so no add rounds and the device `f32` C equals the `f64`
reference exactly. Fail-closed guards (`E-MMA-DTYPE`, `E-MMA-BTF16`) reject bf16 combined with a
tf32-only feed knob and gate the transposed-`Bs` BT-row alignment — **extended to bf16 and
negative-tested** (`MGC-BF16-NEG`), not assumed.

### Result — bf16 matches Habu's own fp16, so it is the training dtype at no throughput cost

Best-of-3, run solo, sustained ~2405 MHz (the same-session FP32 `MM` CUDA-core roof reproduced the
tf32/fp16 baseline clock — 8.2 / 13.2 / 15.0 / 13.0 TFLOP/s). **Bold = per-shape bf16 winner across
both B feeds** (the 4-warp MFRAGS=4 `BM128×BN64` tile sweeps every shape — stages=2 dyn k-major at
512³/1024³, transposed-`Bs` `BPAD=8` stages=1 static at 2048³/4096³):

| TFLOP/s (bf16, C=A·B)          |  512³ | 1024³ | 2048³ | 4096³ |
|--------------------------------|------:|------:|------:|------:|
| **Habu bf16 tile (best)**      |**16.4**|**36.1**|**46.3**|**46.9**|
| Triton 3.8 bf16 `tl.dot`       |  27.4 |  67.3 |  77.6 |  80.8 |
| **Habu / Triton (bf16)**       | 0.60× | 0.54× | 0.60× | 0.58× |
| (ref) Habu fp16 tile (Round 6) |  16.3 |  36.1 |  46.1 |  47.2 |
| (ref) Habu tf32 tile (Round 3) |  16.3 |  29.1 |  31.7 |  28.2 |
| **bf16 / own tf32**            | 1.00× | 1.24× | 1.46× | 1.66× |

The **finding is that bf16 tracks Habu's own fp16 within best-of-3 noise** (16.4/36.1/46.3/46.9 vs
16.3/36.1/46.1/47.2) — expected, because the two tiles are bit-identical but for the mma dtype token
and the GB10's bf16 and fp16 HMMA sit on the same throughput ladder. So bf16 buys the **same
1.0×→1.66× over-own-tf32 curve** as fp16 (flat at the launch-bound 512³, rising monotonically to the
compute-bound 4096³ where the denser HMMA binds), and the transposed-`Bs` feed wins the same
compute-bound 2048³/4096³ shapes for the same reason (one `b32` B load vs two `u16` + shift/or). The
**practical** value is not speed over fp16 but that this throughput now comes with **f32 dynamic
range**: bf16 is the dtype nanoGPT trains in without loss-scaling, and Habu now has a checked,
element-exact bf16 GEMM at fp16-class speed.

**Honest, unflattering headline: on the GB10 the checked Habu bf16 tile reaches 0.54–0.60× of Triton
3.8's bf16 `tl.dot`** — the same shape of result as every prior round (Triton's autotuner finds a
4-warp, 3–5-stage pipelined tile that Habu's 2-stage / single-buffer tiles do not match). Triton's
bf16 referee (27.4 / 67.3 / 77.6 / 80.8) is itself a touch below its fp16 (27.4 / 73.8 / 85.8 / 89.1),
so the bf16 ratio reads slightly higher than fp16's at 1024³ despite the near-identical Habu numbers.

### Triton bf16 referee

The same `/tmp/gemm-triton-gb10.py` referee, run with a `bf16` row added to its label loop
(`("bf16", torch.bfloat16, False)`; C dtype stays f32, matching Habu); manual max-autotune, CUDA-event
warm timing, best of 3. Measured on the GB10 (torch 2.9.1 / triton 3.8, `rel_err ~3e-3` vs the
`torch.matmul` bf16 reference, relative policy): **27.4 / 67.3 / 77.6 / 80.8 TFLOP/s**
(512³/1024³/2048³/4096³), 32 of 45 configs fit the smem cap.

### Reproduction (exact)

```
bin/hb --load tools/ptx/mma-gemm-check.f   # MGC-CFG-BF16 + MGC-CFG-BF16-T rows PASS element-exact (both warp grids,
                                           # ±epilogue, both B feeds, BPAD 0/8); MGC-BF16-NEG fail-closed; tf32+fp16 rows unchanged
# swap the file's bottom entry to GEMMBENCH:GB-BF16-SWEEP (as with GB-F16-SWEEP), then:
bin/hb --load tools/ptx/gemm-bench.f       # GB-BF16-SWEEP: k-major + transposed-Bs bf16 configs, all four shapes; best of 3, solo
bin/hb --load tools/ptx/mma-emit-diff.f    # 35 tf32 + 6 fp16 configs; diff base vs branch = empty (6 bf16 rows appended)
# Triton bf16 referee (source-built 3.8 in the ml venv), bf16 row added to the label loop:
~/Work/ml/.venv/bin/python /tmp/gemm-triton-gb10.py
```

## Corrected verdict — the assembler was the discriminator (dot `habu-pin-blackwell-grade-8ec5ee0a`): rounds 4–7 mechanism stands, remedy was ptxas 13.3 (2026-07-19)

Every GB10 number above was assembled by **system CUDA 13.0.88 `ptxas`**. That
build ships an **immature sm_121 scheduler**: it issues each
`HMMA.1688.F32.TF32` at a fixed yield-set interval and pads the steady K-loop
with the **~40 NOP per 64 HMMA** Round 4 found (its stall-field-overflow
encoding). **`ptxas` 13.3.33** — the `cuda_nvcc-linux-sbsa-13.3.33` archive
Triton's build cache fetched, now pinned into Habu's own tool store
(`~/.habu/toolchain/ptxas-13.3.33`, sha256
`f9a0a7f1…4326e`; provisioning recipe in `docs/codegen-verdict.md` "Pinned ptxas
toolchain") — schedules the **same, unmodified PTX** into the resident-warp
schedule with **zero NOP**, ~28 % fewer steady-window stall cycles, the same 128
registers, zero spills, and all `mma-gemm-check` rows still element-exact. The
discriminator for the whole steady-window story was the **assembler binary**, not
the emitter or the PTX.

**What this amends, and what it does not.** Rounds 4–7's *mechanism* work all
**stands** and every falsification was correct: renaming the B registers is inert
at the machine level (Round 4), the tile is mma-issue-bound not fragment-feed-
bound on the big shapes (Rounds 2–5), and the denser `m16n8k16` HMMA is the real
FLOP-per-issue lever (Rounds 5–7). What is corrected is Round 4's *attribution* of
the 40-NOP body to intrinsic tensor-core issue latency — it was the 13.0
assembler's stall encoding, and 13.3 removes it from identical PTX. The campaign's
"compiler-scheduling-class" ceiling was a **toolchain** class, closable by an
assembler upgrade, not a codegen limit.

**The honest wall-clock, because the two numbers must be reconciled.** The ~28 %
is a **steady-window stall-cycle** reduction; it is **not** a 28 % end-to-end GEMM
speed-up, because at the current tiles/shapes the kernel is occupancy-, feed-, and
launch-bound (the very finding of Rounds 2–3), so the steady-HMMA window is only
one term of the roofline. Re-measuring the committed winners under the pinned 13.3
(`tools/ptx/gemm-bench.f` `GB-GB10` and, via the documented `GB-EPI-SWEEP` swap,
the epilogue winners; best-of-3, run solo, and the same-session FP32 CUDA-core
`MM` roof reproduces the committed 8.2 / 13.2 / 15.0 TFLOP/s within < 1 %, so the
clock matches the 2411 MHz the head-to-head was taken at) gives the tf32 numbers
of record:

| TFLOP/s (tf32, C=A·B), best-of-3 winner tile | 512³ | 1024³ | 2048³ | 4096³ |
|----------------------------------------------|-----:|------:|------:|------:|
| 13.0-era committed winner                    | 16.3 |  29.1 |  31.7 |  28.0 |
| **pinned 13.3 winner**                       | 16.3 |**29.9**|**32.6**| 27.7 |
| Δ wall-clock                                 | flat | +2.9 % | +2.8 % | noise |
| **Habu / Triton 3.8 tf32**                   |0.75× |**0.89×**|**0.86×**|0.61× |

The 1024³ epilogue winner (4-warp MFRAGS=4 stages=1 static + smem C epilogue,
128×64) lifts 29.1 → **29.9 TF** and the head-to-head **peak rises 0.87 → 0.89×**;
2048³ lifts 31.7 → **32.6 TF** (0.84 → 0.86×). 512³ moves within best-of-3 noise
and 4096³ reads a hair lower than its 13.0-era committed best (also within noise),
so both keep their committed rows. **fp16 and bf16 are ~0 %**: those tiles retire
twice the K per HMMA, so the steady window carries even fewer of the 13.0 NOPs and
there is almost nothing for 13.3 to reclaim. **Triton still wins every shape** —
the residual gap is its 4-warp, 3–5-stage pipelined tile, exactly as before; the
pin closes the *toolchain* gap the mechanism rounds had folded into that number,
not the pipeline-depth gap.

`perf-rows.tsv` keeps the 13.0-era `dgx-spark-gb10` rows as history and adds the
two moved winners under `dgx-spark-gb10-ptxas133`. With the store provisioned the
resolver picks 13.3 automatically (no `PTXAS` override) and the `PTXAS-STALE-SM121`
diagnostic stays quiet; on a machine with only the 13.0 assembler it fires once,
loudly, and the kernels stay element-exact.

### Reproduction (exact)

```
# The pinned 13.3 resolves automatically (lib/ptx/toolchain.f: PTXAS env -> Habu
# tool store -> system CUDA); no override needed once ~/.habu/toolchain is provisioned.
bin/hb --load tools/ptx/mma-gemm-check.f   # MGC-ALL: element-exact under 13.3 (unchanged from 13.0)
bin/hb --load tools/ptx/gemm-bench.f       # GB-GB10: tf32 sweep, best-of-3, solo; FP32 MM anchors the clock
# swap the file's bottom entry to GEMMBENCH:GB-EPI-SWEEP, then re-run for the epilogue winners:
bin/hb --load tools/ptx/gemm-bench.f       # GB-EPI-SWEEP: each per-shape winner, epilogue OFF then ON
# a stale toolchain is visible, never silent — force the 13.0 assembler to see the diagnostic:
PTXAS=/usr/local/cuda/bin/ptxas bin/hb --load tools/ptx/gemm-bench.f   # prints hb: PTXAS-STALE-SM121: ...
```

## Round 8 — widening `BN` past 64: the 4096-class tile (dot `habu-widen-bn-past`): built and measured (2026-07-19)

Every round through the pin left the tile's **N span hardwired at 64** (`warp_col`
selects one of two 32-col halves; 4 n-tiles/warp). The GB10 sweep's own referee
named this as structural: Triton's per-shape tf32 winners are **`BM64×BN128` at
2048³ and `BM128×BN256` at 4096³** ("Occupancy is NOT the 512³ lever", §above) —
wider-N tiles Habu's `cg-mma.f` could not emit. This round makes `BN` a knob
(64/128/256) so a warp owns `NTILES = BN/(WCOLS·8)` n-tiles per col-half; the
accumulator count (`MFRAGS·NTILES·4`), the fragment→lane store map, the
`cp.async` Bs chunk partition, and the smem-epilogue staging tile all derive from
`BN`. `WCOLS` stays 2 and the `BN=64` non-wide path is untouched, so **every one
of the 47 committed tf32/fp16/bf16 configs is byte-identical** (`mma-emit-diff.f`,
empty diff base-vs-branch under the same assembler).

**Correctness, exhaustively, before any timing.** `mma-gemm-check.f` proves the
wide-`BN` geometry element-exact on the GB10 with zero tolerance (small-integer
fill, f32 accumulate `< 2^24`): `BN=128` and `BN=256` at both warp grids, `MFRAGS`
1/2/4, with and without the smem epilogue, tf32 (scalar-`cvt` **and** `ldmatrix`-A,
which must agree) plus fp16 and bf16 — checked at the two square edges that are
exact multiples of both the M block and the N block, and every wide-`BN` `C[0][0]`
matches the `BN=64` golden at the same edge (10749 at 256³, 21335 at 512³). The
register budget is a **hard, fail-closed gate**: per-lane accumulators
`= MFRAGS·NTILES·4`, and `MMA-CHECK-REGS` throws `E-MMA-REGS` when they plus the
measured working set bust the 255-register file — so the `BN=256 MFRAGS=4` corner
(256 accumulators, cannot even hold them) is rejected at emit time, negative-tested
alongside the `BN` power-of-two/`≥64` gate (`E-MMA-BN`) and the wide-`BN` epilogue
cap (`E-MMA-EPI`, `BROWS·BN·4 = 131072 > 99 KB`). The transposed-`Bs` feeds
(`BLDM`/`BTF16`, whose staging is `n = c&63`) fail closed above `BN=64`.

### Result — `BN=256` lifts 4096³, but 2048³ stays with the `BN=64` tile

Best-of-3, solo, pinned 13.3 ptxas (same-session FP32 `MM` roof reproduces
8.2/13.3/15.0/13.1 — the 2411 MHz band). **Bold = the wide-`BN` mover vs its
committed `BN=64` anchor:**

| TFLOP/s (tf32, best-of-3)                          |  512³ | 1024³ | 2048³ |  4096³ |
|----------------------------------------------------|------:|------:|------:|-------:|
| `BN=64` M4 stages2 dyn (256×64) — committed 2048 winner |  9.2 |  21.5 | **30.2** |  22.9 |
| `BN=64` M4 B-`ldmatrix` s1 (256×64) — committed 4096 winner |  6.9 |  20.9 | 27.1 |   27.4 |
| `BN=128` M2 4-warp (64×128, Triton 2048 geometry)  |   9.3 |  20.1 | 24.2 |   15.4 |
| `BN=128` M2 8-warp s2 dyn (128×128)                |   6.5 |  14.6 | 20.9 |   17.8 |
| `BN=256` M2 8-warp s2 dyn (128×256, Triton 4096 geometry) |  4.2 |  19.0 | 26.3 | **29.6** |
| Triton 3.8 `tl.dot` referee                        |  21.7 |  33.5 |  37.8 |   45.3 |

**The honest split.** The `BN=256` `128×256` tile — the exact geometry Triton's
autotuner picks at 4096³ — reaches **29.6 TF at 4096³, +5.8 % over the committed
`BN=64` 4096 winner (27.4/28.0 TF) and lifting the head-to-head 0.62× → 0.65×
Triton 3.8.** That is a real, reproducible gain on the most compute-bound column
(the +5.8 % holds across all three passes: 29.6/29.6/29.4). **2048³ does not move:
no wide-`BN` tile in the sweep beats the `BN=64` `MFRAGS=4` `256×64` tile (30.2 TF);
the best wide-N 2048³ is `BN=256` M2 at 26.3.** So the dot's "expect 2048 and 4096
to move" is **half confirmed** — 4096 moves up, 2048 stays. The small shapes
(512³/1024³) are worse on the wide tiles, as expected: fewer, larger blocks
under-fill the 48 SMs and the launch/occupancy floor dominates there.

**Why 4096 and not 2048, structurally.** `BN=256` grows the Bs tile to `BK·256·4`
bytes, so a double-buffered `128×256` tile is already 96 KB — and **stages≥3 busts
the 99 KB cap for every `BN=256` shape** (even the 64-row `MFRAGS=1` variant:
`40960·3 > 99 KB`). The wide-N tile therefore inherits the **same 2-stage smem
ceiling** the pin round named, now bounded by the *B* tile rather than the *A*
tile. Closing the residual 0.65× at 4096³ needs the pipeline-depth lever the
99 KB cap forecloses for `BN=256`, not another `BN` widening — the same wall,
one tile wider. The value earned here is the *tile family* (Habu can now emit the
`BM×BN256` geometry element-exact, the register gate keeps it honest) and a
measured 4096³ gain; the value **not** earned is any 2048³ improvement or a
narrowing of the pipeline-depth gap.

### Reproduction (exact)

```
# element-exact first (arch auto-probed sm_121a), then throughput:
bin/hb --load tools/ptx/mma-gemm-check.f   # MGC-ALL incl. the MGC-CFG-BN / MGC-BN-NEG / MGC-REGS-NEG rows
# byte-identity: BN=64 must not move any committed config
bin/hb --load tools/ptx/mma-emit-diff.f    # base vs branch -> empty diff (47 tf32/fp16/bf16 configs)
# throughput (swap the file's bottom entry to GEMMBENCH:GB-BN-SWEEP), best-of-3, solo, 13.3 pinned:
bin/hb --load tools/ptx/gemm-bench.f       # GB-BN-SWEEP: FP32 roof + BN=64 anchors + the BN=128/256 tiles
```
