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
- **Bandwidth:** Triton 63.0 GB/s vs Habu-PTX 42.5 GB/s at this N — same order of
  magnitude, Triton ~1.5×. The gap is the launch path, not codegen: Habu-PTX still
  uses the deprecated `cuLaunchGrid` + per-param `cuParamSetv` path (an 11-arg
  `cuLaunchKernel` faulted; dotted) and an untuned BLOCK; both are well under the
  Orin's ~200 GB/s LPDDR5 peak, i.e. launch/occupancy bound at 2²⁰.

**Earned claim:** a checked stack-effect target is a viable Triton replacement
that **shifts the stack-discipline error class left to author time** — caught
statically, with a located diagnostic and zero GPU — where Triton finds it only at
runtime, at competitive (same-order) bandwidth. **Not** earned: any "faster than
Triton" claim (it is currently ~1.5× slower on this microbench) or that the
checker catches *semantic* errors (it does not; that is the device-golden gate's
job).
