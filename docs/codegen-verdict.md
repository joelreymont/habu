# Codegen verdict — roofline + SASS audit on the DGX Spark GB10 (sm_121a)

Settles the question *"does the code generator suck?"* with measured numbers on
**this** GB10, not the Orin. Dot `habu-codegen-verdict-roofline-4d6bf436`.
Method per `docs/kernel-principles.md`; the Orin baseline it extends is
`docs/eval-triton.md` / `docs/compute-campaign.md`.

**One-line verdict: the code generator does NOT suck.** It emits spill-free SASS
that lowers `cp.async`→`LDGSTS` and `mma.sync`→`HMMA` natively on Blackwell,
vectorizes shared loads to `LDS.128`, and attains the **same fraction of roof on
GB10 as on Orin** (memory ~88%, fp32 GEMM ~48-51%). The only thing that stops the
committed device leg from running is a one-token arch constant, not the emitter.

---

## Verdict table (per kernel)

| kernel | bound | GB10 measured (2048³ / N≥64M) | roof | %-of-roof | SASS findings | verdict |
|---|---|---|---:|---:|---|---|
| SAXPY-V4 `a·x+y` | memory | **240 GB/s** | 273 GB/s DRAM | **88%** | 18 reg, 0 spill, `LDG/STG.128` | **adequate** — at the DRAM wall; only lever is fusion |
| MMN naive GEMM | (no reuse) | 1.55 TFLOP/s | 29.6 TFLOP/s fp32 | 5.2% | 28 reg, 0 spill | adequate-as-baseline (the "before blocking" tile) |
| **MM fp32 blocked+cp.async** | compute (fp32 CUDA) | **14.9 TFLOP/s** | 29.6 TFLOP/s fp32 | **~50%** | 56 reg, **0 spill**, 512 FFMA, 64 `LDS.128`, 8 `LDGSTS.128`, 2 BAR | **adequate** — matches Orin's 52%; headroom is tiling, not codegen |
| MMM tf32 `mma.sync` (default) | compute (tf32 tensor) | 12.6 TFLOP/s | ~50 TFLOP/s tf32 est. | ~25% | 40 reg, 0 spill, 16 `HMMA.1688.F32.TF32` | adequate — un-tuned default feeding, not saturating; flagship wide tile not re-measured here |

All GEMM outputs verified numerically on-device: with A=B=1.0, `C[0]` read back
as f32 bits `0x44000000`/`0x44800000`/`0x45000000` = 512.0/1024.0/2048.0 exactly
(both MM fp32 and MMM tf32) at 512³/1024³/2048³.

**Routing decision (the dot's question):**
- **`habu-ptx-register-pressure-ed521b40` is NOT on the critical path.** Every
  kernel measured **0 bytes spill stores / 0 bytes spill loads** and **zero
  `LDL`/`STL`** in SASS; the heaviest (MM) uses 56 of the 255-register budget on
  a device with 64K registers/SM. There is no measured register-pressure defect
  on GB10 to fix.
- **`habu-ptx-opt-layer-325b9507` is real headroom but a *tiling/feed* lever, not
  a *codegen-quality* fix.** The fp32 GEMM sits at ~50% of the CUDA-core roof —
  identical to Orin (52%) — so the gap is the documented tiling/tensor-core-feed
  work (`docs/compute-campaign.md`), reproduced on a higher roof, not a Blackwell
  codegen regression. The clean SASS (no waste, native cp.async/HMMA) confirms the
  emitter is not the bottleneck; the algorithm's tile/feed is.

---

## Item 1 — does the device leg run here? (BLOCKING for E1 device dots)

**Finding: the committed device leg does NOT run as-shipped — it throws
`E-CUDA` (-5002). Root cause is a hardcoded assembler arch, NOT the code
generator; a one-token fix makes the identical PTX + driver + launch path load
and compute correctly.**

Two committed constants are wrong for this box:
1. `lib/ptx/toolchain.f:65` assembles every kernel with **`-arch=sm_87`**.
2. `lib/ptx/toolchain.f:59` defaults `PTXAS` to
   **`/usr/local/cuda-12.6/bin/ptxas`**, which does not exist here (the box has
   CUDA 13 at `/usr/local/cuda`).

The pipeline is `emit .target sm_87 PTX → ptxas -arch=sm_87 → cubin →
cuModuleLoad(cubin)`. `cuModuleLoad` loads a **cubin (SASS)**, which the driver
does not JIT — so an sm_87 cubin cannot run on the sm_121 GB10.

**Proof (exact CUresults), reusing the committed `CUDA` driver package:**

```
# emit the committed checked SAXPY, assemble at both arches with the CUDA-13 ptxas:
printf '' | bin/hb --load lib/errors.f lib/string.f lib/float.f lib/fmt.f \
  src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/header.f lib/ptx/tile.f \
  tools/ptx/saxpy-cg.f > saxpy.ptx
/usr/local/cuda/bin/ptxas -arch=sm_87    saxpy.ptx -o saxpy.sm87.cubin
/usr/local/cuda/bin/ptxas -arch=sm_121a  saxpy.ptx -o saxpy.sm121.cubin
# cuModuleLoad each on device 0:
sm87   : cuModuleLoad rc=209      # CUDA_ERROR_NO_BINARY_FOR_GPU
sm121a : cuModuleLoad rc=0        # loads
```

`rc=209` is exactly what the committed `tools/ptx/cuda-launch.f` /
`tools/ptx/gemm-bench.f` hit (`RC0` collapses it to the generic `E-CUDA` throw).

**The fix is one token, and the rest of the path is already Blackwell-clean:**
- `ptxas -arch=sm_121a` on the **unchanged** `.target sm_87 / .version 8.3` PTX
  succeeds (rc 0) — ptxas re-targets the virtual PTX; the PTX text does *not*
  need editing. (Retargeting the text to `.target sm_121a` would additionally
  require bumping `.version 8.3`, which ptxas rejects for sm_121a — so the
  minimal, correct change is the assembler `-arch` only.)
- The resulting sm_121a cubin **loads, launches through the committed deprecated
  `cuFuncSetBlockShape`/`cuParamSetv`/`cuLaunchGrid` path, and returns the
  correct golden**: SAXPY `y = a·x + y` with x=2, a=3 → f32 bits `0x40C00000`
  = 6.0, verified on device 0 (driver 580.159.03).

So the device leg is blocked for the **committed gate** (I am read-only on
`toolchain.f`), but the blocker is a build target, not the emitter. With
`-arch=sm_121a` + `PTXAS=/usr/local/cuda/bin/ptxas`, the committed
`tools/ptx/gemm-bench.f` and `tools/ptx/bandwidth-v4.f` run unmodified and
produce the numbers below. **Long-term fix (matches CLAUDE.md's Triton target):**
make the emitter/toolchain arch a device-selected parameter (sm_121a on this box)
instead of the sm_87 literal, so no env override is needed.

**Recommendation:** mint/route a dot to parameterize the PTX/ptxas target arch
(default sm_121a on GB10). It is the single gate that unblocks all E1 device
dots on this machine.

---

## Item 2 — the GB10 rooflines

### Device facts (measured, `cuDeviceGetAttribute` on device 0)

| attribute | value | source |
|---|---|---|
| compute capability | **12.1** (sm_121) | attr 75/76 |
| multiprocessors (SM) | **48** | attr 16 (⇒ 48×128 = 6144 CUDA cores) |
| SM clock — idle / **application** / max boost | 208 / **2418** / 3003 MHz | attr 13 = 2418; `nvidia-smi -q -d CLOCK` |
| SM clock — **measured sustained under GEMM** | **2405 MHz** | 171/189 under-load samples, `nvidia-smi` |
| memory data rate × bus | **8533 MT/s × 256-bit** | attr 36 = 8533000 kHz, attr 37 = 256 |
| threads/SM · regs/SM · smem/SM | 1536 · 65536 · 102400 B | attr 39/82/81 |

The GPU **does not boost to 3003 MHz under sustained compute** — it holds the
2418 MHz application clock (measured 2405 MHz over a 4.9 s GEMM, at 32-47 W and
39 °C, i.e. neither power- nor thermally-throttled). The honest fp32 roof uses
the *measured sustained* clock; the max-boost figure is the spec ceiling the
part does not reach here.

### DRAM bandwidth ceiling — measured

STREAM-like read/read/write triad (SAXPY-V4, bytes = N·iters·12), swept over N to
escape the L2 cache:

| N (f32 elems) | working set | GB/s | note |
|---:|---:|---:|---|
| 1 M | 8 MB | 1516 | **L2-resident** (not DRAM) |
| 4 M | 32 MB | 427 | spilling L2 |
| 16 M | 128 MB | 253 | ~DRAM |
| 64 M | 512 MB | **240** | DRAM |
| 128 M | 1 GB | **240** | DRAM (stable) |

- **Theoretical DRAM ceiling: 8533 MT/s × 256 bit / 8 = 273 GB/s** (matches the
  published GB10 273 GB/s LPDDR5X spec).
- **Achievable: 240 GB/s = 88% of theoretical** — a healthy STREAM efficiency
  for LPDDR5X. Scalar SAXPY (ept=1) measures ~250 GB/s at 64 M — i.e. at the
  memory wall the v4 vectorization gains nothing, exactly as the roofline
  predicts (you cannot beat the memory system on a memory-bound triad).

### fp32 CUDA-core peak — arithmetic

```
P_peak(fp32) = SMs × (fp32 FMA lanes/SM) × 2 FLOP/FMA × clock
```
| clock basis | value | fp32 peak |
|---|---|---:|
| measured sustained (GEMM) | 2405 MHz | 48 × 128 × 2 × 2.405e9 = **29,553 GFLOP/s** |
| application clock | 2418 MHz | 29,710 GFLOP/s |
| max-boost spec ceiling (not reached) | 3003 MHz | 36,900 GFLOP/s |

Cross-check of the lane/FMA model: the same formula gave the Orin's published
1.9 TFLOP/s roof (`docs/kernel-principles.md`); here 48 SM × 128 lanes = 6144
CUDA cores matches the GB10 spec.

### tf32 tensor-core peak — estimate (for the MMM row)

Derived from the published GB10 "1 PFLOP FP4-sparse" tensor figure and the
Blackwell 5th-gen precision ladder (dense TF32 : FP16 : FP8 : FP4 = 1:2:4:8,
sparse = 2× dense): TF32 dense ≈ 1000/16 = **62.5 TFLOP/s at max boost**, ≈ **50
TFLOP/s at the sustained 2405 MHz**. Flagged as marketing-derived; the MMM
verdict below (feeding, not saturating) is invariant to the exact value, as on
Orin.

---

## Item 3 — the kernel set, normalized to %-of-roof

All via CUDA-event GPU timing (the committed `PTXBENCH:BENCH-GPU-NS`), one
warmup, iters = 200 at 512³, 80 at 1024³, 30 at 2048³, cubins assembled
`-arch=sm_121a`.
GPU was never contended during measurement (0% util between launches).

| kernel | 512³ | 1024³ | 2048³ | bound | %-of-roof (2048³) |
|---|---:|---:|---:|---|---:|
| MMN naive (fp32) | 1512 | 1599 | 1547 | fp32 CUDA | 5.2% |
| **MM blocked+cp.async (fp32)** | **8194** | **13275** | **14946** | fp32 CUDA | **~50%** (29.6 TF roof) |
| MMM `mma.sync` tf32 (default) | 7706 | 11648 | 12606 | tf32 tensor | ~25% (≈50 TF roof) |

(GFLOP/s; MM 2048³ measured 14005-14946 across runs — the sustained 4000-iter
run that pinned the 2405 MHz clock gave 14005 = 47.4% of the 29,553 roof; the
30-iter run gave 14946 = 50.6%.)

**Reading:** the fp32 blocked tile lands at ~48-51% of the fp32 CUDA roof — the
**same roof-fraction as Orin's 52%** (`docs/kernel-principles.md`). The naive
tile at 5.2% mirrors Orin's ~5.9%. The register-blocking lever reproduces
identically on Blackwell (MM is ~9.6× the naive tile here). MMM (tf32, un-tuned
default BK=32/MFRAGS=1/scalar+cvt) sits below MM — the same "feeding not
saturating the tensor cores" result the Orin doc records for the default MMM
tile; the tuned flagship wide tile (`MMA-MFRAGS=4` B-ldmatrix, 3026 GFLOP/s =
1.6× Triton on Orin) is the emitter's real tf32 ceiling and is **not** the
default emitted here, so 12.6 TF understates the tf32 codegen (bounded residual,
see below).

---

## Item 4 — `cuobjdump -sass` audit of the GEMM cubin (sm_121a)

`ptxas -v` (both kernels, `-arch=sm_121a`): **0 bytes stack frame, 0 bytes spill
stores, 0 bytes spill loads**, 32768 bytes smem.

### MM (fp32 blocked+cp.async) — 56 registers, 752 SASS instructions

| opcode | count | role |
|---|---:|---|
| `FFMA` | 512 | the 4×4 micro-tile × bk=32 accumulate (68% of all instr) |
| `LDS.128` | 64 | shared As/Bs fragment feed — **fully 128-bit vectorized** (8 f32/k-step in 2 loads) |
| `LDGSTS.E.BYPASS.128` | 8 | **`cp.async` native on Blackwell**, 128-bit, L1-bypass → smem |
| `STG.E` | 16 | C store (16 elems/thread; scalar, strided micro-tile — expected) |
| `IMAD`/`LEA`/`IADD3` | 36/19/17 | index/pointer arithmetic |
| `BAR` | 2 | `bar.sync` — not excessive |
| `LDGDEPBAR`/`DEPBAR` | 2/2 | `cp.async` commit/wait_group dependency barriers |
| `LDL`/`STL` | **0** | **no spills** |
| `MOV`/`UMOV` | 1/4 | negligible — **no redundant-MOV bloat** |

- **8:1 FFMA:LDS ratio** and 128-bit shared loads = high compute density, no
  narrow-load bank-conflict pattern at the SASS level.
- **56 registers exactly matches the emitter's documented +B cp.async blocking
  intent** (48 reg pre-cp.async / 56 with it, `docs/eval-triton.md` step 2) — and
  is identical sm_87→sm_121a, so no Blackwell register regression.
- No dead barriers, no spill traffic, no obvious waste.

### MMM (tf32 `mma.sync`, default) — 40 registers, 0 spill

- **16 `HMMA.1688.F32.TF32`** — the `mma.sync.m16n8k8` tf32 tiles lower to native
  Blackwell tensor-core HMMA (F32 accumulate, TF32 inputs). Correct.
- 48 `LDS` + 48 `FSETP` — the scalar fragment feed + the 48 `cvt.rna.tf32.f32`
  per staged tile (the ALU overhead `docs/eval-triton.md` step 3 names; on the
  default tile it is not the dominant cost).
- 8 `LDGSTS` cp.async, **0 `LDL`/`STL`**. 40 reg (vs 38 on sm_87) — not
  register-bound.

**SASS conclusion:** the emitter's output is clean on Blackwell — spill-free,
natively lowering the two hard features (cp.async → LDGSTS, mma.sync → HMMA),
vectorizing shared loads, with register counts on their documented budget. There
is no "the codegen sucks" evidence in the instruction stream.

---

## Pinned ptxas toolchain (sm_121 scheduler) — dot `habu-pin-blackwell-grade-8ec5ee0a`

The SASS above is *correct* on either assembler, but not equally *fast*. System
CUDA 13.0's `ptxas` (V13.0.88) ships an immature sm_121 scheduler: it issues
every `HMMA.1688.F32.TF32` at a fixed 16-cycle yield-set interval and pads the
steady K-loop with ~40 stall NOP per 64 HMMA (the 40-NOP body `docs/eval-triton.md`
Round 4 found and mis-attributed to a B-register hazard). `ptxas` **13.3.33**
schedules the *same, unmodified* PTX into the resident-warp schedule with **zero
NOP** and ~28% fewer steady-window stall cycles — same 128 registers, zero spills,
all `mma-gemm-check` rows element-exact. The discriminator for the Round-4 40-NOP
steady-window schedule was the assembler binary, not the emitter or the PTX (the
end-to-end wall-clock gain at the current tiles is small — `docs/eval-triton.md`
corrected-verdict round).

So Habu pins 13.3 into **its own** user-local tool store (not another project's
cache dir, which can vanish), sha256-verified, and `lib/ptx/toolchain.f` resolves
it ahead of system CUDA. Provisioning a fresh GB10:

```sh
# canonical source: NVIDIA's cuda_nvcc-linux-sbsa-13.3.33 archive — the same one
# Triton's build cache fetches to ~/.triton/nvidia/nvcc-blackwell/. ptxas is the
# only binary Habu needs from it.
mkdir -p ~/.habu/toolchain
cp cuda_nvcc-linux-sbsa-13.3.33-archive/bin/ptxas ~/.habu/toolchain/ptxas-13.3.33
chmod 755 ~/.habu/toolchain/ptxas-13.3.33
sha256sum ~/.habu/toolchain/ptxas-13.3.33
# f9a0a7f1f7f03b402ca222168a8ae4870fdb312354356b444941fbba7754326e
~/.habu/toolchain/ptxas-13.3.33 --version   # → release 13.3, V13.3.33
```

`toolchain.f` probes in order **`PTXAS` env override → `~/.habu/toolchain/
ptxas-13.3.33` → system CUDA (`/usr/local/cuda`) → legacy 12.6**, reads the
resolved binary's `--version`, and for an sm_121 target with only a pre-13.3
assembler prints a loud named `PTXAS-STALE-SM121` diagnostic (naming this dot and
the ~27% penalty) — it does **not** die: the 13.0 assembler still produces
element-exact kernels, so the degradation must be visible, never silent. With the
store provisioned, no `PTXAS` override is needed and the diagnostic stays quiet.

---

## Reproduction (exact)

Toolchain: `PTXAS=/usr/local/cuda/bin/ptxas` (CUDA 13.0.88, knows sm_121a
natively); driver 580.159.03; `cuobjdump` `/usr/local/cuda/bin/cuobjdump`.

1. **Device-leg finding & fix.** Emit + assemble + load exactly as the Item 1
   block; `cuModuleLoad` rc 209 (sm_87) vs 0 (sm_121a) is the whole finding.

2. **SASS/register audit.** Emit each kernel's PTX from the committed emitters,
   then assemble and dump:
   ```
   # MM: --load lib/errors.f lib/string.f lib/float.f lib/fmt.f \
   #       src/arch/ptx/emit.f lib/ptx/cg.f lib/ptx/cg-matmul.f  ; then EMIT-MATMUL
   # MMM: add lib/ptx/cg-mma.f ; then EMIT-MATMUL-MMA
   /usr/local/cuda/bin/ptxas -arch=sm_121a -v mm.ptx  -o mm.cubin      # regs/spill
   /usr/local/cuda/bin/cuobjdump -sass mm.cubin                        # instruction mix
   ```

3. **Rooflines & kernel set.** With the one-line `toolchain.f`
   `-arch=sm_87`→`-arch=sm_121a` fix, the committed harnesses run directly:
   ```
   PTXAS=/usr/local/cuda/bin/ptxas printf '' | bin/hb tools/ptx/bandwidth-v4.f
   PTXAS=/usr/local/cuda/bin/ptxas printf '' | bin/hb tools/ptx/gemm-bench.f
   ```
   The numbers above were taken read-only via a scratch driver that reuses the
   committed `PTXBENCH` launch + CUDA-event timing on the sm_121a cubin (the only
   difference from `gemm-bench.f`/`bandwidth-v4.f` is the assembler arch), sweeping
   N to 128 M for the DRAM ceiling and sampling `nvidia-smi clocks.sm` for the
   sustained-clock roof. `bandwidth-lib.f` now derives its working set from the
   device's L2 cache size (CUDA attribute 38): N = max(64 M elems, 8·L2/4),
   computed from the live device before each probe — no hand-swept constant. On
   this GB10 the floor governs (L2 < 32 MB), so the committed probe self-sizes to
   N=64 M and measures ~246 GB/s DRAM directly, escaping the old N=2²⁰ L2 residency.

## Residual / bounded

- **Flagship tf32 tile not re-measured on GB10.** The default MMM (12.6 TF) is
  below the shipped wide B-ldmatrix tile (Orin 3026 = 1.6× Triton). Measuring the
  wide tile on GB10 needs the `MMA-MFRAGS=4`/dynamic-smem launch config; it does
  not change the fp32/register verdict but would tighten the tf32 row.
- **tf32 tensor roof is estimated** (marketing FP4 + Blackwell ladder), not
  derived from an authoritative per-SM tf32 FMA rate.
- **Sustained clock is `nvidia-smi`-sampled** (no root/DVFS pinning available);
  240 GB/s and ~50% fp32 roof-fraction are stable across repeated runs.
