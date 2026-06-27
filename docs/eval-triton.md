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
