# Kernel principles — roofline, the 3 bounds, and how we apply them

The methodology every kernel decision in this repo must follow (distilled from the
kernel-compiler course). **First instinct on any op: compute its arithmetic
intensity and place it on the roofline — that decides whether the lever is fusion
(cut bytes) or tiling/tensor-cores (raise FLOP/s).** Do not optimize before
classifying the bound.

## Roofline & the three bounds

- Arithmetic intensity `I = FLOPs / bytes_moved` (FLOP/byte).
- Attainable `P = min(P_peak, B·I)`. **Ridge point `I* = P_peak / B`** — below it you
  are **memory-bound**, above it **compute-bound**. (A third roof, **communication**,
  is set by interconnect BW — not relevant single-device.)
- Memorize **`2·M·N·K`** FLOPs for a GEMM; **tile arithmetic intensity ≈ BM·BN/(BM+BN)**
  (bigger square tiles → higher I → cross from memory- into compute-bound).

| bound | limited by | typical ops | the fix |
|---|---|---|---|
| **Compute** | peak FLOP/s (esp. tensor cores) | large square GEMM, conv | better tiling, **tensor-core (MMA) utilization**, lower precision |
| **Memory** | HBM/LPDDR bandwidth | softmax, norm, bias+act, GEMV, attention I/O | **fuse** to cut round-trips; recompute vs store |
| **Communication** | NVLink/network | all-reduce, all-to-all (MoE) | overlap with compute; fewer bytes |

## THIS device (Orin NX, sm_87, 25W) — measured roofs

**Canonical measurement environment:** nvpmodel **mode 3 (25W, 4 TPCs / 8 SMs,
TPC_PG_MASK=240)**, GPU DVFS **pinned at 918 MHz** — the mode-3 maximum of the
box's supported GPU rates (`/sys/class/devfreq/17000000.gpu/available_frequencies`
runs 306, 408, 510, 612, 714, 816, 918 MHz; 918 is the top rung). Re-measured on-device
2026-07-14; the `orin-nx-25w` rows in `tools/ptx/perf-rows.tsv` are the baseline.

- **8 SMs × 128 FP32 lanes @ 0.918 GHz → P_peak(FP32 CUDA cores) ≈ 1880 GFLOP/s.**
  The register-blocked cp.async FP32 tile (`MM`, `lib/ptx/cg-matmul.f`) measured
  **918.8 / 967.8 / 979.9 GFLOP/s** at 512³/1024³/2048³ — **~52% of the FP32-CUDA
  roof** (tiling/pipeline headroom remains).
- **Tensor cores (TF32) sit on a HIGHER roof** — Triton's matmul measured **1474
  GFLOP/s** (15W baseline) via TF32 tensor cores (`tl.dot`, allow_tf32), so FP32-
  CUDA-core kernels and tensor-core kernels are on **two different compute roofs**;
  mind which one you're on. Our TF32 mma.sync tile (`MMM`, `lib/ptx/cg-mma.f`,
  scalar+cvt baseline) measured **830.1 / 872.9 / 884.9 GFLOP/s** at 25W — device-
  correct but still feeding, not saturating, the tensor-core roof (it does not yet
  beat the FP32 `MM` tile at this rung).
- **Memory B ≈ 93 GB/s achievable** (v4 SAXPY `SAXPY-V4` hit **93.4 GB/s**; spec
  ~102). Ridge (FP32 roof, measured B) **I\* ≈ 20 FLOP/byte**.

**Historical note — 15W (2 TPCs / 4 SMs, early-July `orin-nx-15w` rows):**
4 SMs × 128 lanes @ 0.92 GHz → P_peak(FP32) ≈ **940 GFLOP/s**; memory **B ≈ 63
GB/s** (v4 SAXPY 64.2 GB/s); ridge **I\* ≈ 15 FLOP/byte**. Moving to 25W (8 SMs at
the same 918 MHz GPU clock) roughly **doubled the FP32 roof** (940 → 1880) and
lifted achievable memory bandwidth **~1.5×** (63 → 93 GB/s). The `orin-nx-15w`
registry rows are retained as history — a new device tag is a fresh baseline, so
`PERF:SCAN` never compares 25W against 15W.

## Where each Habu kernel sits (apply, don't assume)

The `I` and `bound` columns are power-mode-independent; the absolute `roof`/`measured`
figures below are the **15W baseline** (63 GB/s memory, 940 GFLOP/s FP32). At the
canonical **25W** environment the roofs scale to **93 GB/s / 1880 GFLOP/s** (see
above), and current absolute numbers live in the `orin-nx-25w` registry rows.

| kernel | I (FLOP/byte) | bound | roof (15W) | measured (15W) | verdict |
|---|---|---|---|---|---|
| SAXPY `a·x+y` | 0.17 | memory | 63 GB/s | **63 GB/s** | **100% of memory roof — done, only lever is fusion** |
| fused `relu(a·x+y)` | 0.25 | memory | 63 GB/s | 63 GB/s | 100% of memory roof; fusion IS the win |
| tiled SGEMM (64² tile) | ~32 | compute | 940 (FP32 CUDA) | 283 GFLOP/s | **30% of the FP32-CUDA roof — tiling headroom** |
| Triton matmul | ~32 | compute | TF32 tensor-core | 1474 GFLOP/s | on the **tensor-core** roof, above FP32 CUDA peak |

**Principle-derived consequences (this is the plan):**
1. **Memory-bound kernels are already optimal** — SAXPY/fused hit the memory roof;
   the roofline *proves* more codegen can't help, only fusion (fewer bytes). ✓ matches
   what we measured (unroll/MLP flat).
2. **Our GEMM has 30%→100% headroom on the CUDA-core roof** via the 5 speed levers
   below (vectorized shared loads, double-buffer/pipeline, bigger square tiles) →
   target ~700–940 GFLOP/s. Dotted `habu-tiled-gemm-codegen`.
3. **To match/BEAT Triton we must emit TENSOR-CORE MMA** (`mma.sync.aligned` TF32/FP16)
   — a *different, higher roof*, not just better tiling. This is the real beat-Triton
   lever for compute kernels (alongside aggressive fusion for memory-bound chains).
   → owed a dot (tensor-core MMA codegen).

## TF32 tensor-core MMA — validated fragment layout (Orin sm_87)

The compute beat-Triton lever. `mma.sync.aligned.m16n8k8.row.col.f32.tf32.tf32.f32`
computes D[16×8] = A[16×8]·B[8×8] + C across one warp. **Device-validated** lane→
element layout (gid = lane>>2, t = lane&3) — getting this wrong is the course's #1
"correct in NumPy, garbage on device":

- **A** (16×8 row-major), 4 tf32/lane: a0=A[gid][t], a1=A[gid+8][t], a2=A[gid][t+4], a3=A[gid+8][t+4].
- **B** (8×8), 2 tf32/lane: b0=B[t][gid], b1=B[t+4][gid]. (cvt.rna.tf32.f32 each operand.)
- **C/D** (16×8), 4 f32/lane: d0=C[gid][2t], d1=C[gid][2t+1], d2=C[gid+8][2t], d3=C[gid+8][2t+1].

Fragment layout is proven **element-exact** in isolation FIRST (integer operands
exact in tf32 → the mma tile reproduces the integer matmul bit-exact):
`tools/ptx/mma-probe.f` (one 16×8×8 tile, 128 cells, 0 mismatches), then
`tools/ptx/mma-gemm-check.f` for the full K-looping kernel at 64³/128³ (0
mismatches). The kernel is `lib/ptx/cg-mma.f` (`MMM`); `maki/lower/mm.f` emits it
when the matmul class is licensed at TF32, and `maki/precision-device-test.f`
LOWER-GOLDEN passes device==host within the tf32 row (the running license).

**Measured ladder (in-tree, 2026-07-05, docs/eval-triton.md GEMM step 3):** the
`mma.sync` tile reuses the FP32 path's 64×64 block + cp.async double-buffered
staging and swaps only the compute inner (8 warps, 16×32 warp tile = 4 MMA
n-tiles, A fragment reused 4×). Landed **375.6 / 393.5 / 398.5 GFLOP/s** at
512³/1024³/2048³ (ptxas: 38 reg, 32 KB smem, 0 spill).
- This is device-correct and on the tensor-core roof, but at THIS rung it does
  **not yet beat** the tuned FP32 cp.async tile (442) and is ~21% of Triton
  (1636–1890): the roofline-predicted "feeding the tensor cores, not saturating
  them" — MMM uses *fewer* registers (38 vs 56), so it is not register-bound.
- **`ldmatrix` rung MEASURED — NEGATIVE (2026-07-05, eval-triton step 3c).** The
  fragment-feed hypothesis (scalar-load bank conflicts + 48 `cvt`/tile starve the
  MMA) was **falsified by a 3-mode ablation** in `lib/ptx/cg-mma.f` (`MMA-LMODE`):
  dropping every `cvt` (raw `ld.shared.b32`, mma truncates f32→tf32) is FLAT, and
  ONE `ldmatrix.x4` for the A fragment is ~1.2% *slower* (43 vs 38 reg, 0 spill;
  370.0/388.9/394.3 vs 376.1/393.5/398.5 GFLOP/s). All modes element-exact
  (`mma-probe` MP-LDM-ALL + `mma-gemm-check` 64³/128³); default stays the exact-RNE
  scalar+cvt feed. The step-3c lesson "this rung is issue/dependency-bound, not
  fragment-feed-bound" was **WRONG, and its error is the instructive part**: it
  measured `ldmatrix` on an *unpadded* As, whose 16 fragment-row addresses (row
  stride 128 B = 32 words) all alias one shared-memory bank — a 16-way conflict
  that serialized the load and hid the win. **Measure an optimization at the layout
  it needs.**
- **CORRECTED (2026-07-15 `habu-mma-larger-bk`, 2026-07-17 `habu-close-mma-gemm` +
  `habu-mma-amortize-the`):** padding As to a bank-free stride (`MMA-PAD=8`) makes
  `ldmatrix` **+53%** (885.8 → 1358.9 GFLOP/s at 2048³, 918 MHz — past FP32 `MM`
  981.9). At that padded/ldmatrix rung a 918 MHz variant-kernel timing decomposition
  (nsys GPU-metrics is unsupported on this iGPU, so DCE-safe ablated kernels are the
  profiling method) shows the kernel is **FEED-BOUND on un-amortized B-side scalar
  `ld.shared` loads (5.04 ms of 12.61 ms ≈ 40 %, each 8×8 B fragment fed exactly one
  mma), NOT issue/dependency-bound** — the cp.async staging floor (7.48 ms) is hidden
  behind the feed, A-side ldmatrix is ~free (reused 4×), mma issue is ~1 %; the
  quarter-B-loads proxy = **2270 GFLOP/s** ceiling. **THE lever is a wider M register
  tile** (`lib/ptx/cg-mma.f` `MMA-MFRAGS`): each warp owns MFRAGS stacked 16-row
  M-fragments, so each 8×8 B fragment is **reused across MFRAGS M-frags** and a taller
  block (`BM = 64·MFRAGS`) also halves *global* B staging. `MFRAGS=2` (128×64 block,
  32 f32 accumulators/lane, double-buffered dynamic .shared 57344 B) measured
  **2133.9 GFLOP/s at 2048³ = 1.13× Triton (1890.5) — the first Habu GEMM past parity**,
  +55.7 % over the shipped swizzled `MMM-SWZ-BK64` and 94 % of the 2270 ceiling
  (element-exact `mma-gemm-check` 128³/256³, two runs ±0.04 %). Note stages 1-vs-2 was
  flat at the narrow tile but stages=2 is +2.4 % at the wide tile — the amortized feed
  re-exposes the cp.async floor, so overlap matters again. **Residual** (productized
  DCE-safe ablation `tools/ptx/mma-ablate.f`, the `MMA-ABLATE` knob, 2048³ 918 MHz):
  the 128×64 kernel is STILL **~36 % B-feed** (full 241.2 ms → quarter-B 153.5 ms, its
  own ceiling **3357 GFLOP/s**) and now **~15 % mma-issue** for the 2nd M-frag
  (full − single-mma 35.2 ms), so parity is reached but the tile is not saturated — the
  next lever is **B-side `ldmatrix` on a transposed/swizzled Bs** (one ldmatrix replaces
  8 scalar B loads; needs a NEW element-exact fragment proof in `mma-probe.f` FIRST) and/or
  MFRAGS=4. The general method: attribute with DCE-safe ablated kernel variants (the iGPU
  has no counter profiling), then attack the phase the decomposition names.
- **SATURATED (2026-07-17 `habu-mma-wave-2`, `lib/ptx/cg-mma.f` `MMA-MFRAGS=4`):** the
  attribution named MFRAGS=4 as a lever, and it pays big — **2707.3 GFLOP/s at 2048³ (918
  MHz), +26.9 % over the MFRAGS=2 parity (2133.9) and 1.43× Triton (1890.5)**, element-exact
  at 256³ AND 512³ (ldmatrix + scalar+cvt cross-check), two runs ±0.09 %, references
  reproduced ±0.07 %. The **256×64 block reuses each 8×8 B fragment across 4 M-frags** (64
  f32 accumulators/lane, `%f<80>`/`%r<72>` register pools that stay byte-identical at
  MFRAGS≤2). The counter-intuitive winner: **single-buffer STATIC (49152 B = the 48 KiB cap)
  BEATS double-buffer dynamic (98304 B) by +11.6 %** (2707 vs 2394) — the exact *opposite* of
  the MFRAGS=2 finding that stages=2 helped. Reason: at MFRAGS=4 the double-buffer tile needs
  98 KiB so only **1 block/SM** (8 warps) resides, while the single-buffer tile fits 2–3
  blocks/SM; and the 4× B-feed amortization has re-weighted the roofline off the cp.async
  floor, so **occupancy now beats overlap**. The lesson: `num_stages` is tile-size- *and*
  occupancy-dependent — re-measure it at every tile, and a bigger register/smem tile can
  flip stages=2 from a win to a loss. **New residual attribution** (`mma-ablate.f`, 2048³):
  B-feed fell to **27 %** (51.4/190.3 ms, down from 36 %) but the **2nd–4th-M-frag mma-issue
  rose to 32 %** (60.4/190.3 ms, up from 15 %) — the roofline **shifted from feed-bound toward
  tensor-core-issue-bound**, own quarter-B ceiling 3711 GFLOP/s (73 % attained). The remaining
  27 % B-feed is now addressable by the **B-side ldmatrix fragment layout, proven element-exact
  in isolation** (`tools/ptx/mma-probe.f` `MP-BLDM-ALL`): a non-trans `ldmatrix.x2` on a
  **transposed** `SHM_BT[n][k] = B[k][n]` returns exactly `b0=B[t][gid], b1=B[t+4][gid]` —
  because the ldmatrix result law is `reg = tile[row=lane>>2][tf32col=lane&3]`, so the tile it
  reads must be Bᵀ. `ldmatrix.trans` is **not** usable for tf32 (it transposes at b16 granularity
  and a tf32 is two b16 halves, so `.trans` splits every tf32) — the transpose must live in the
  **staging**, not the load. That proof feeds `habu-ship-swizzled-mma`; the mma-issue 32 % is a
  harder tensor-core-throughput bound.
- **B-SIDE LDMATRIX WIRED — WIN (2026-07-17 `habu-mma-wave-3`, `lib/ptx/cg-mma.f` `MMA-BLDM`):** the
  proven transposed-`Bs` law was wired into the wide tile — a new scalar **transposed staging**
  (`SHM_BT[n][k]=B[k][n]`, coalesced global read / strided shared write; cp.async cannot scatter a
  contiguous chunk so it is a scalar copy) plus **one `ldmatrix.sync.aligned.m8n8.x2` per 8×8 B
  fragment** replacing the per-n-tile 2 `ld.shared`+2 `cvt` scalar feed. **MFRAGS=4 BK=32 pad=8
  bpad=4 stages=1 single-buffer dynamic (50176 B) measured 3026.6 GFLOP/s at 2048³ (918 MHz) =
  +11.9 % over the scalar-B winner `MMM-WIDE-M4-S1` (2707.3, reproduced same-session 2704.0/2707.2)
  and 1.60× Triton (1890.5)**, element-exact at 256³ AND 512³ (B-ldmatrix + scalar+cvt cross-check),
  two runs ±0.14 %. **DCE-safe ablation re-attribution** (`mma-ablate.f`, 2048³): the residual B-feed
  fell **27 % → 7 %** (51.4 → 12.1 ms) — a 4.2× cut, exactly the predicted "drop toward the mma-issue
  floor" — leaving the kernel at **93 % of its own quarter-B ceiling** (vs 73 % scalar-B); the
  mma-issue (21 %) is now the dominant residual and the harder tensor-core-throughput floor.
  **BANK GEOMETRY IS DECISIVE — the ldmatrix must read a conflict-free stride:** the n-major BT row
  stride `BK+bpad` sets the ldmatrix read start-bank stride. `bpad=4` (stride 36 words ≡ 4 mod 32 →
  the 8 tile rows span 8 distinct 4-bank windows) gives the 3026.6 win; `bpad=0` (stride 32 → all 8
  rows alias **one** 4-bank window, an 8-way conflict) collapses to **1318.5 GFLOP/s — WORSE than the
  scalar-B baseline**, the *same* "measure the optimization at the layout it needs" trap as the
  original unpadded-As `ldmatrix` miss. The 16 B ldmatrix-row alignment (`(BK+bpad)*4` a multiple of
  16) is enforced fail-closed in the emitter (`MMA-CHECK-BLDM` → `E-MMA-BLDM`): a misaligned bpad
  faults the GPU (sm machine-check), so it must never reach a launch. Double-buffer (stages=2, 100 KB,
  1 block/SM) and MFRAGS=2 both lose to the MFRAGS=4 single-buffer tile (same occupancy-beats-overlap
  lesson as wave-2). Next lever is the mma-issue floor (tensor-core throughput).

## TF32 tensor-core ROOFLINE VERDICT (wave-4) — the mma.sync GEMM program is on the tf32 issue floor

Waves 1-3 took the TF32 `mma.sync` GEMM from 884.9 to **3026.6 GFLOP/s** (2048³, 918 MHz).
Wave-4 is the desk verdict: derive the sm_87 Orin NX TF32 tensor-core theoretical peak from
architecture facts, place the measured ladder against it, and decide whether a kernel lever
remains or the perf program closes. **No device session was run** — see the closing note.

### Deriving the sm_87 Orin NX dense-TF32 tensor peak at 918 MHz

Architecture facts (NVIDIA sources, cited below):
- **Orin NX 16GB GPU:** 1024 CUDA cores, 32 Tensor Cores, Ampere `sm_87`, 918 MHz max GPU
  clock (base = boost). [Orin NX data sheet / NVIDIA-confirmed spec pages.]
- **Per-SM structure (authoritative):** *"128 CUDA cores per SM … and four 3rd Generation
  Tensor cores per SM"* — NVIDIA Jetson AGX Orin Series Technical Brief v1.2, p.5. So Orin NX =
  1024/128 = **8 SMs** = 32/4 = 8 SMs (both counts agree). This is the same 8-SM count the 25W
  mode-3 measurement environment already uses.
- **3rd-gen Ampere dense-TF32 rate = 512 TF32 FMA per SM per clock.** Anchor: A100 (GA100,
  same 4× 3rd-gen Tensor Cores/SM) publishes **156 dense TF32 TFLOPS** at 108 SMs / 1.41 GHz,
  and dense FP16 = 312 TFLOPS with TF32 = ½ of FP16. Back out the per-SM rate:
  `156e12 / (108 × 1.41e9 × 2 FLOP/FMA) = 512 TF32 FMA/SM/clock` (= 128 per Tensor Core, ×4).
  [NVIDIA Ampere Architecture In-Depth blog; A100 datasheet/whitepaper.]
- **Orin runs the Tensor Cores at the FULL (GA100-class) rate, NOT the GeForce/GA102 consumer
  throttle.** Decisive evidence: AGX Orin's **GPU-only** INT8 = *170 sparse TOPS* (Technical
  Brief p.5, GPU compute excluding the 2× NVDLA), i.e.
  `170e12 / (16 SM × 1.3e9) / 2(sparse) / 2(op→MAC) = 2048 dense INT8 MAC/SM/clock` — exactly
  the GA100 INT8 rate and **2× the GA102 consumer rate** (~1024). A part whose INT8 is full
  GA100-rate does not carry the GeForce market-segmentation throttle, so the fixed 3rd-gen ratio
  ladder **INT8 : FP16 : TF32 = 4 : 2 : 1** (in MAC/FMA per SM per clock) holds: TF32 = 512
  FMA/SM/clock.

**Peak arithmetic:**

```
P_peak(TF32, dense) = SMs × (TF32 FMA/SM/clock) × 2 FLOP/FMA × clock
                    = 8 × 512 × 2 × 918e6
                    = 8192 × 918e6
                    = 7.52e12 FLOP/s  ≈  7520 GFLOP/s
```

**Cross-checks (the derivation is trustworthy where it reproduces measured/published roofs):**
- Same method on the FP32 CUDA roof: `8 × 128 × 2 × 918e6 = 1880 GFLOP/s` — the repo's measured
  FP32 roof, and NVIDIA lists Orin NX FP32 = **1.9 TFLOPS**. The AGX Orin brief's own Figure-1
  FP32 = **5.3 TFLOPS** is exactly `16 × 128 × 2 × 1.3e9`, confirming the lane/clock/FMA=2 model
  end-to-end on an NVIDIA-published number.
- **Our measured best 3026.6 > 1880** (the FP32 CUDA roof *and* the fully-throttled GA102 TF32
  rate of 128 FMA/SM/clock → 1880 peak). A kernel cannot exceed its hardware roof, so the
  consumer-throttled interpretation is *ruled out by measurement*.
- **Discounted alternative (flagged honestly):** if `sm_87` carried only the GeForce
  *FP32-accumulate* half-throttle (TF32 = 256 FMA/SM/clock → **3760** peak), the best would sit
  at 80% of peak. That is inconsistent with (i) the DCE-safe phase decomposition, which spends
  only ~27% of runtime issuing mma (a kernel at 80% of tensor peak would be mma-saturated), and
  (ii) a hand-written tensor kernel achieving a *higher* fraction of its roof than our FP32 CUDA
  kernel (52% of the FP32 roof) — tensor kernels are harder to feed, so they attain a *lower*
  roof-fraction, not higher. Both the INT8=GA100-class evidence and the measurement favor
  **512/7520**. **The verdict below is identical under either peak.**

### The measured ladder vs the 7520 GFLOP/s dense-TF32 peak (2048³, 918 MHz)

| rung | GFLOP/s | % of 7520 TF32 peak | vs Triton 1890.5 |
|---|---|---|---|
| **theoretical dense-TF32 peak** | **7520** | 100% | 3.98× |
| single-mma proxy ceiling (drops M-frags 1-3 mma; *unphysical*) | ~3810 | 50.7% | 2.02× |
| MFRAGS=4 scalar-B quarter-B ceiling (wave-2 proxy) | 3711 | 49.3% | 1.96× |
| wave-3 winner's own quarter-B ceiling (B-feed removed) | ~3259 | 43.3% | 1.72× |
| **MMM-WIDE-B-M4-S1 — current best (wave-3)** | **3026.6** | **40.2%** | **1.60×** |
| MMM-WIDE-M4-S1 — scalar-B MFRAGS=4 (wave-2) | 2707.3 | 36.0% | 1.43× |
| MMM-WIDE-M2 — MFRAGS=2 parity (amortize) | 2133.9 | 28.4% | 1.13× |
| **Triton `tl.dot` allow_tf32 (baseline)** | **1890.5** | **25.1%** | 1.00× |
| MMM-SWZ-BK64 — A-side ldmatrix (wave-1) | 1369.6 | 18.2% | 0.72× |
| MMM — scalar+cvt TF32 baseline | 884.9 | 11.8% | 0.47× |

*(FP32 reference points sit on the separate 1880 GFLOP/s CUDA-core roof, not this tensor roof:
FP32 `MM` = 979.9 = 52% of 1880.)* The current best is **40.2% of the derived tf32 tensor peak,
93% of its own quarter-B feed-ceiling** (DCE-safe ablation, `tools/ptx/mma-ablate.f`: B-feed
12.1/169.7 ms = 7%, mma-issue 34.9/169.7 ms = 21%), and **1.60× Triton** — which itself only
reaches 25% of the same peak.

### VERDICT: (a) CLOSE the tf32 mma.sync GEMM perf program — the residual is the irreducible tf32 issue floor

The kernel-lever inventory from waves 1-3 is **exhausted**, and the three instruction-level
candidates named for wave-4 are each already spent or a hard constraint, not headroom:
- **Wider M register tile / more M-frags:** done — MFRAGS=4 (256×64 block, 64 f32
  accumulators/lane). MFRAGS=8 needs 128 f32 accumulators/lane and a 512×64 tile: it blows the
  255-register budget (guaranteed spill) and the shared-memory cap. Not viable.
- **Dual-issue scheduling across M-frags:** the 4 M-frags already expose independent mma
  accumulator chains (ILP), and the block runs 8 warps across the 4 SM warp-schedulers. The 21%
  mma-issue is the Tensor Cores consuming *real, fixed* work, not a scheduling stall.
- **Accumulator pressure / `wait_group` placement:** MFRAGS=4 sits at the register sweet spot
  (wider spills); single-buffer static already beat double-buffer (occupancy-beats-overlap once
  the B-feed is amortized), so cp.async wait placement has no remaining overlap to win.

The residual 21% "mma-issue" is the **Tensor Cores doing the GEMM's fixed mma work** — a 2048³
GEMM is `N³/1024 = 8.39M` m16n8k8 mma instructions, a count no schedule can reduce. Cutting
mma-issue time needs a *denser instruction shape*, and **for TF32 the widest shape is already in
use**: `mma.sync.m16n8k8` (the only wider-K tf32 shapes are m16n8k4/k8; there is **no m16n8k16
for tf32** — that shape exists only for fp16/bf16/int8). The tf32 instruction-shape lever is
therefore spent. At **40% of the theoretical tf32 tensor peak, 93% of its own feed-ceiling, and
1.60× a mature autotuned Triton baseline**, the TF32 `mma.sync` GEMM is on its practical issue
floor. **Recommendation: close the GEMM perf program.** No wave-4 lever dot is minted — there is
no quantified tf32 instruction-level lever with measured headroom to hand off.

**Where the residual goes:**
- **Autotuner (`habu-feed-mma-config-d783e33b`):** per-shape BM/BN/BK/MFRAGS/stages search may
  extract single-digit-% gains on specific (non-2048³, tall/skinny, odd-K) shapes. This is
  *config search over the existing shapes*, not a new instruction-level lever.
- **Default-flip (`habu-ship-swizzled-mma-7b78c01b`):** productize the wave-3 winner as the
  emitted default and refresh the competitive `HABU-MMM-TF32` row.
- **fp16/bf16 shapes — USER-GATED numerics-policy question, OUT OF SCOPE, not implemented.** The
  *only* remaining instruction-level lever is a denser accumulate shape (`mma.sync.m16n8k16`
  fp16/bf16), which ~2× the K per instruction and lifts the tensor peak — but it **changes the
  numerics contract** (fp16/bf16 inputs vs the tf32-for-f32 eval policy). This is a precision
  decision for the user, not a kernel-perf decision. If and only if the user relaxes the
  tf32-for-f32 policy does a new perf lever open; recorded here so the closure is auditable.

**Sources (peak derivation):**
- NVIDIA Jetson AGX Orin Series Technical Brief v1.2, July 2022 (`TB_10749-001_v1.2`), p.5:
  "128 CUDA cores per SM … four 3rd Generation Tensor cores per SM"; AGX Orin 64GB = 2048 CUDA /
  64 TC / 170 sparse INT8 GPU-TOPS / 5.3 FP32 TFLOPs; 2× NVDLA v2.0.
- NVIDIA "NVIDIA Ampere Architecture In-Depth" developer blog + A100 datasheet/whitepaper:
  each 3rd-gen Tensor Core = 256 FP16 FMA/clock (1024/SM); A100 dense TF32 = 156 TFLOPS = ½ FP16
  312 TFLOPS; 108 SMs @ 1.41 GHz → 512 TF32 FMA/SM/clock.
- Orin NX 16GB spec (1024 CUDA / 32 TC / 918 MHz / FP32 1.9 TFLOPS / 100 INT8 TOPS incl. DLA;
  2× NVDLA v2.0) — NVIDIA Orin NX data sheet and vendor spec pages.

**Why no device session.** The mission allowed a pure mma-issue-rate micro-benchmark only if the
desk analysis genuinely needed one. It did not: the peak is pinned by authoritative NVIDIA data
(the 8-SM count, the 512 TF32 FMA/SM/clock rate, the GA100-class full-rate evidence), the
existing productized DCE-safe ablation already supplies the on-device phase decomposition, and
the verdict (close) is **invariant** to the one residual ambiguity (512 vs 256 FMA/SM/clock) — a
probe could refine the *practical* issue roof but cannot open a tf32 instruction-level lever that
waves 1-3 did not already close. A pure-mma probe would only convert "40% of a strongly-inferred
7520 peak" into "40% of a measured 7520 peak"; it does not change the recommendation. The Orin
was left untouched (no new perf-rows rows; the ladder above is the committed registry).

## The five things that govern speed (check all five)

1. **Occupancy** — enough resident warps to hide latency. *Means, not goal*: a
   register-heavy GEMM at 50% occupancy beats 100%-occupancy with a bigger accumulator
   tile (more reuse). Optimize sustained throughput, not an occupancy number.
2. **Coalescing** — a warp's 32 lanes touch contiguous global addresses (→ few 128 B
   transactions). Strided access multiplies byte traffic. (Our v4 staging must stay
   coalesced.)
3. **Shared-memory bank conflicts** — 32 banks; pad/swizzle so lanes don't serialize.
4. **Divergence** — a warp-splitting `if` runs both sides masked. Minimize per-lane
   branching. (Our attention's serial thread-0 softmax is a divergence/latency sink —
   the dotted parallel-softmax fix.)
5. **Latency hiding** — the SM tolerates ~500-cycle memory latency via ready warps +
   ILP. "Make it more parallel" usually beats "make each thread faster."

## Fusion (the memory-bound lever)

Elementwise + reduction ops are memory-bound → **fuse so intermediates never touch
HBM**. Unfused `matmul→bias→GELU` pays 3× the HBM traffic; fused does the epilogue in
registers. In a *concatenative checked* DSL fusion is free (= word concatenation) and
proven by the checker — see `docs/eval-triton.md`. **Aggressive epilogue fusion onto a
GEMM is where we can move strictly fewer bytes than hand-fused Triton** (dotted
`habu-automatic-aggressive-fusion`).

## Matmul in practice: shapes, tuning, tests

- **Shape regimes:** square → compute-bound (max tiles/MMA); **tall-skinny** (small K)
  → **split-K / stream-K** for parallelism; **decode M=1** → GEMV, memory-bound →
  weight-only quant + batch. Recognize the regime before tuning.
- **Autotuning:** don't guess BM/BN/BK/num_warps/num_stages — **search per shape key**,
  cache the winner. `num_stages` = software pipelining (prefetch K-iterations; costs
  SMEM, interacts with tile size/occupancy).
- **Tensor-core constraints:** fixed operand/accumulator format pairs; K aligned to the
  MMA-K (and scale block for FP4/FP8); scales have a prescribed interleaved layout.
  **Accumulator stays FP32**; low precision is a roofline move (more FLOP/s, fewer
  bytes) — the accuracy game is *scaling*, not the adder.
- **Numerical checks (BLOCKING for any kernel):** prove correct vs an **FP32 reference**
  with **dtype-matched tolerance** (BF16/TF32 are *not* bit-exact), and lock it with a
  regression test. (We do device-vs-CPU; the committed test harness is dotted
  `habu-committed-device-correctness`.)
- **Benchmark/profile rows (BLOCKING for perf claims):** report the kernel label, launch
  shape, `gpu_elapsed_ns` from CUDA events, bytes, FLOPs, GB/s, GFLOP/s, and roof
  utilization through the checked Habu profile helpers (`tools/ptx/profile.f`) and
  generic launch harness (`tools/ptx/bench.f`). Keep host launch timing separate
  from device timing. A new kernel optimization is not "faster" until the relevant
  profile row explains which roof it moved toward. Current fused-vs-unfused device
  row: v4 SAXPY + v4 RELU as separate launches sums to 66.269 ms / 200 iters, while
  fused v4 RELU is 39.209 ms / 200 iters (`fusion_elapsed_ratio_x1000=1690`).

## Profile-row registry & perf-regression workflow (BLOCKING)

Measured rows are durable, not prose: every kernel-optimization rung carries its
own row in **`tools/ptx/perf-rows.tsv`** — kernel id, launch config
(grid/gridy/block/blocky/iters/work-items), metric kind (`GBS`, `GFLOPS`,
`PCT-ROOF`, all value×1000), value, device, date, note. The checked owner is
`tools/ptx/perf-registry.f` (package `PERF`); rows fail closed under
`PERF:LOAD`/`PERF:LINE-OK?`.

- **Changing kernel codegen requires a row.** `tools/kernel-perf-lint.f` scans a
  `jj diff --git` artifact and fails when the diff touches a kernel emitter
  (`lib/ptx/cg.f`, `lib/ptx/cg-*.f`, `tools/ptx/*-cg.f`, `src/arch/ptx/emit.f`)
  without adding a registry row. Run it alongside `tools/typed-local-diff-lint.f`
  in the pre-commit Forth gate:
  `bin/hb --load tools/kernel-perf-lint.f -- diff.patch`.
- **Off-device sessions add a WAIVER row instead.** When the Orin is
  unavailable, add a `WAIVER` row (value 0) whose note documents the
  device-gated reason and the owed measurement; the lint accepts it and the row
  stays as the visible debt until the next device pass replaces it.
- **Re-measurements are appended, never edited**, so the latest same-key pair is
  comparable. `tools/ptx/perf-compare.f` flags a new value more than
  `PERF:TOL-MILLI` (50 permille = 5%) below its baseline as a regression;
  `bin/hb tools/ptx/perf-regress.f` runs that scan over the committed registry
  and exits nonzero on any regression.
- **Gate wiring:** the `ptx-toolchain` suite (lint-libs slice of
  `test/gate-stdlib.f`) runs the registry/compare/lint tests, validates the
  committed registry, runs the regression scan, and host-loads the bench stack
  (`bench.f`, `bandwidth-lib.f`, `fusion-compare.f`, `gemm-bench.f`) with device
  legs as recorded SKIPs off-device.

## The one-line instinct (say it on every op)

"What's its arithmetic intensity vs the ridge? Elementwise/norm sit far left →
memory-bound → **fuse**. Big GEMMs sit right → compute-bound → **tile + tensor-core
MMA**. Then check whether the compiler already fuses it; if it can't cross that
boundary, write the kernel." Then verify against the roofline numbers above — never
claim a kernel is "slow/fast" without its % -of-the-right-roof.
