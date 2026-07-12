# Eval matrix — checked Habu-PTX vs real Triton (Orin sm_87)

The thesis is comparative: a *checked* kernel target (Habu-PTX) versus the
unchecked, runtime-only target the field actually uses (Triton). This doc records
the **real-Triton** column — Triton compiled and run on the same Orin — and the
side-by-side matrix against the Habu-PTX column produced by our own tools.

Triton is the external system under comparison; it is Python because Triton is
Python. It is **not** repo automation and nothing in the gate runs it — the
reproduction scripts below are the canonical reference (kept out of the tree so
`host-lint` stays green; the Habu-side reducers `tools/ptx/bandwidth.f`,
`maki/eval-compare.f`, `maki/eval-device.f` are the live Habu column).

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
a·x+y = 6.0. The candidates mirror the Habu fixture in `maki/eval-compare.f`.
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

`maki/eval-compare.f` also runs an internal Habu-PTX ablation over the SAXPY
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
  golden/device run. (Our device-golden grader `maki/eval-device.f` is that run.)
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
precision) is CAD-PLAN 8.1; this doc stays the measured record.

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
committed `lib/ptx` + `maki/eval-device*.f` pipeline). Generator population: Claude
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

CAD-PLAN 8.1 step 1. The compute-bound contest starts here: square fp32 GEMMs
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
  the same algorithm the `maki/lower-mm.f` naive fallback tile emits);
  `MM` = register-blocked 64×64 tile, shared As/Bs staging, 4×4
  accumulators/thread (`lib/ptx/cg-matmul.f`, the algorithm the `maki/lower-mm.f`
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
  pipelining yet (the CAD-PLAN 8.1 steps that follow this baseline): bk=16
  staging, scalar `ld.shared`, single stage. The Triton side autotunes 8 configs
  and its `tl.dot` lowers to **TF32 tensor cores** on sm_87 — the measured
  `rel_err ~7.8e-4` against a CPU f32 reference is TF32-level precision (a pure
  f32 FMA kernel measures ~1e-6 rel here, as our device goldens do), so the two
  columns also differ in arithmetic: full-f32 fma.rn (ours, golden-gated at
  rtol 1e-4) vs TF32 dot (theirs, silently licensed).
- These absolute numbers supersede the older 15 W notes in `lib/ptx/cg-matmul.f`
  (~77 naive / ~283 blocked / ~1474 Triton): this session's device power state is
  higher; all three columns above are same-session, same-protocol.
- Next levers, in CAD-PLAN 8.1 order: `cp.async` multi-stage staging (the
  schedule family already carries `stages`), wider bk (family floor 32 vs the
  reused kernel's 16), vectorized `ld.shared.v4`, then the `mma.sync` TF32 family
  itself — with the precision *licensed* by the golden gate rather than assumed.

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

CAD-PLAN 8.1 step 2. Same device, same protocol, same shapes as the step-1
baseline above (fp32 `C = A·B`, CUDA-event timing, one warmup, ITERS 200, 80,
30 at 512, 1024, 2048, A = B = 1.0). Pure f32 throughout — **no precision
change** (the device goldens still gate `fma.rn.f32` at rtol 1e-4; TF32/MMA is
step 3). Each
increment re-emits + ptxas-assembles the same in-tree `MM` kernel
(`lib/ptx/cg-matmul.f`, also the algorithm the `maki/lower-mm.f` blocked path
emits) and re-runs `tools/ptx/gemm-bench.f` on the Orin. Every increment keeps
the `maki/lower-mm-device-test.f` and `maki/lower-model-device-test.f` goldens
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

CAD-PLAN 8.1 step 3. The FP32 CUDA-core roof (~940 GFLOP/s) caps the `fma.rn.f32`
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
`PREC-TF32` requested for the matmul class, `maki/lower-mm.f` emits this kernel
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
in-tree grader (`maki/eval-matrix-main.f`) is the judge, the generation run is a
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
bin/hb --load maki/eval-matrix-main.f -- maki/transcripts/live-habu-ptx-2026-07-12.txt
```

| task | target | n | green | pass@1-x1000 | pass@2-x1000 | pass@3-x1000 | repaired | repair-rounds | tokens-to-green | GB/s-x10 | device | graded |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| saxpy | habu-ptx | 5 | 4 | 800 | 1000 | 1000 | 1 | 1 | 172 | not-run | not-run | checker |
| softmax | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 292 | not-run | not-run | checker |
| fused-relu | habu-ptx | 5 | 5 | 1000 | 1000 | 1000 | 0 | 0 | 150 | not-run | not-run | checker |

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
  above are deterministic replay — `maki/eval-live-test.f` (in the maki suite)
  re-replays the transcript and pins every cell of this table.
