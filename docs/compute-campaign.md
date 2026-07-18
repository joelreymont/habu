# Compute-bound campaign — the beat-Triton plan and measured record

**Status:** living record. Extracted 2026-07-18 from `docs/archive/cad-plan.md` §8.1 (now
archived at `docs/archive/cad-plan.md`) so the compute-bound strategy,
sequencing, and LANDED measurement log keep a single editable home after the
Model CAD design was superseded by `MODEL-CAD-V2-PLAN.md` (2026-07-11).

**Scope.** This doc owns the compute-bound campaign: the strategy levers, the
dependency-ordered sequencing, the LANDED log with measured GFLOP/s history, and
the Blackwell/TMA second-target extension pointer. The raw measured columns live
in `docs/eval-triton.md` (the measured record); the V2 architecture that
consumes this strategy is `MODEL-CAD-V2-PLAN.md` §22 (compute backend parity,
delivery order, competitive-evidence schema). Measured numbers below are
preserved verbatim from the original §8.1. Bare `§` references in the text
(e.g. §7.4, §9, §13) point at the archived design, `docs/archive/cad-plan.md`,
whose section numbering is unchanged.

## Compute-bound strategy (the beat-Triton plan)

Measured reality first (docs/eval-triton.md, real Triton 3.5.1 on this Orin):
memory-bound kernels are PARITY at the streaming ceiling (~63 GB/s both) —
nobody beats DRAM — and no GEMM comparison has been measured yet. The
compute-bound plan, in dependency order:

1. **Tensor-core MMA emission** (`habu-tensor-core-mma`): checked emitters for
   `mma.sync.aligned` TF32 first (the family is already named gemm-tf32), then
   fp16/bf16 with f32 accumulate. We emit PTX text directly — no LLVM between
   the schedule decision and the instruction. Without this lever there is no
   compute-roof contest at all.
2. **`cp.async` + multi-stage SMEM pipelining**: the schedule family already
   parameterizes `stages`; the emitter honors it with double/triple-buffered
   `cp.async` staging. Pure emitter work on existing machinery.
3. **Persistent autotuning beats JIT autotuning** (`cad-6-tune` + the §13
   store): Triton tunes at JIT time, in-process, per deployment, with generic
   configs on sm_87. We tune once on the real device, key by §7.4, store the
   winner with evidence, and replay with zero warmup — so we can also afford
   larger search spaces, paid offline.
4. **Fusion depth is the real Orin lever**: this target is memory-starved, so
   most "compute-bound" work is composites whose intermediates spill. The
   planner owns the whole IR with exact bytes: GEMM with prologue
   dequant/epilogue bias+activation in one kernel (slice 3), and the
   attention megafusion (QK^T -> softmax -> V, SMEM-resident;
   `habu-re-express-fused`, `habu-ptx-m11-attention`). End-to-end model
   latency is the honest metric, and fewer launches moving fewer bytes wins
   it even at equal per-kernel FLOPs.
5. **Whole-model decisions a kernel DSL cannot make**: weight layout owned at
   PROMOTE time (pre-transpose/pre-swizzle into the artifact —
   `habu-cad-weight-layout`); launch amortization on Jetson-class overheads
   (persistent kernels / a graph-style driver loop —
   `habu-cad-launch-amortize`); precision policy LICENSED by the gates —
   TF32/FP16 applied only where GOLDEN + gradcheck prove it safe
   (`habu-cad-precision-policy`) [LANDED 2026-07-05 (step 3a, the MMA
   prerequisite): per-class precision registry `maki/precision.f` — PREC-F32
   default everywhere, PREC-TF32 licensed for the matmul class only (atol 1e-6,
   rtol 2e-3 from the measured ~7.9e-4 TF32 GEMM error, ~2.5x headroom); golden
   verdicts judged under each class's ACTIVE precision row and named in the
   reason; PROMOTE evidence records `golden=device-pass:<prec>`; the inverse
   guard (a seeded 0.5% fault fails even under tf32) is proven on-device by
   `maki/precision-device-test.f`. The MMA lane runs licensed via
   `PREC-TF32 CLASS-MATMUL PREC!` — the passing golden IS the license].
6. **Roofline-directed search**: PROFILE's classification (§9) spends tuner
   candidates only on regions actually under the compute roof.

## Sequencing and LANDED log

Sequencing: slice-3 GEMM -> register-blocked GEMM tile + the FIRST measured
GEMM-vs-Triton baseline [LANDED 2026-07-04: lower-mm.f blocked 64x64 tile,
device-golden green; fp32 GFLOP/s at 512..2048 square = ours naive ~55 flat,
ours blocked 357 rising to 381 (6.5-7.0x), Triton autotuned TF32-dot 1636
rising to 1891 (4.6-5.0x over our blocked) — docs/eval-triton.md "GEMM: the
FIRST measured compute-bound column"] -> pipeline the blocked GEMM (step 2)
[LANDED 2026-07-05, pure f32, goldens green: 2A = bk=16->32 family floor +
ld.shared.v4 B load (blocked 379, 397, 403 GFLOP/s at 512, 1024, 2048; 48 regs
and 16 KB smem); 2B = cp.async.cg double-buffered staging, stages=2 (416, 437,
442 GFLOP/s; 56 regs and 32 KB smem) = +16-17% over the bk16 baseline, Triton
gap 4.6-5.0x down to 3.9-4.3x; emitter shaped for family stages 3-4] ->
on-device PROFILE/roofline -> MMA family (step 3, the higher compute roof;
step 3a gate-licensed precision LANDED 2026-07-05, see lever 5) ->
cad-6 tune -> attention megafusion ->
end-to-end model latency vs torch.compile on the detector-class workload. Honest finish line: parity on
the pure compute roof (tensor cores are tensor cores), win on everything
around it — fusion depth, zero-warmup replay, layout ownership, launch count —
which is where end-to-end latency lives.

## Blackwell extension (second process target)

Blackwell extension (second process target, does not disturb the sm_87
sequencing): gathered GEMM + TMA movement plans on the DGX Spark GB10
(sm_121a) — design in `docs/tma-gather.md`, motivating silent-corruption case
in `docs/case-tma-stride.md`.
