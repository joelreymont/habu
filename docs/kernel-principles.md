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

## THIS device (Orin NX, sm_87, 15W) — measured roofs

- **4 SMs × 128 FP32 lanes @ 0.92 GHz → P_peak(FP32 CUDA cores) ≈ 940 GFLOP/s.**
- **Tensor cores (TF32) sit on a HIGHER roof** — Triton's matmul measured **1474
  GFLOP/s > 940**, which is only possible via TF32 tensor cores (`tl.dot`, allow_tf32).
  So FP32-CUDA-core kernels and tensor-core kernels are on **two different compute
  roofs**; mind which one you're on.
- **Memory B ≈ 63 GB/s achievable** (v4 SAXPY hit it; spec ~102). Ridge (FP32 roof,
  measured B) **I\* ≈ 15 FLOP/byte**.

## Where each Habu kernel sits (apply, don't assume)

| kernel | I (FLOP/byte) | bound | roof | measured | verdict |
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

Isolated 16×8×8 tile verified vs CPU to max|err| 3.4e-3 (= TF32 ~10-bit mantissa).
A K-looping GEMM from it is device-correct (max|err| 0.03 at 512³).

**Roofline-guided optimization ladder (measured, the methodology working):**
- naive MMA (1 tile/warp, global fragment loads, no reuse): **105 GFLOP/s** — *slower*
  than FP32 reg-block 283, because it's **global-memory-starved** (tensor cores don't
  help until you feed them).
- + register reuse (warp computes 16×64 = 8 MMA cols, A fragment reused 8×): **336
  GFLOP/s** (512³, 245 at 1024³) — above the FP32 path, on the tensor-core roof.
- + **shared-mem A/B staging** (64×64 block, 4 warps, cooperative stage + bar.sync):
  **371 GFLOP/s** (512³ 350, 1024³ 371 — staging fixed the 1024³ global-thrash). All
  device-correct (max|err| 0.03 = TF32).
- We are at ~25% of Triton (1474) / ~39% of the FP32 roof. The remaining rungs (dotted
  `habu-tensor-core-mma`) are the standard high-perf suite, each a roofline/5-levers
  move: **larger BK** (fewer bar.syncs, more compute/sync), **double-buffering**
  (`num_stages` — hide the global-load latency the bar.sync currently exposes),
  **bank-conflict-free shared** (pad/swizzle), and **`ldmatrix`** for fragment loads.
  Reuse → stage → pipeline is the roofline-predicted order; each rung measured.

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

## The one-line instinct (say it on every op)

"What's its arithmetic intensity vs the ridge? Elementwise/norm sit far left →
memory-bound → **fuse**. Big GEMMs sit right → compute-bound → **tile + tensor-core
MMA**. Then check whether the compiler already fuses it; if it can't cross that
boundary, write the kernel." Then verify against the roofline numbers above — never
claim a kernel is "slow/fast" without its % -of-the-right-roof.
