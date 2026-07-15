---
title: MMA larger BK + swizzled bank-free shared
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"2026-07-05T10:09:39.727995+02:00\\\"\""
closed-at: "2026-07-15T22:11:50.783425+02:00"
close-reason: "Merged (rebased 0ff7e4e3 -> f165eb9b; +2 TRUSTED.md rows for the new driver bindings added at integration after trust-lint caught them unmanifested) on master. RESULT: the swizzle was the lever - padding As shared rows (MMA-PAD=8) makes ldmatrix fragment loads near-bank-free and unlocks +53.5% (611.8 GFLOP/s BK=32 pad=8 static) / +54.8% (616.9, BK=64 pad=8 dynamic-shared) over the 398.5 scalar+cvt baseline at IDENTICAL unboosted 408 MHz clocks - beating the 441.8 FP32 CUDA-core kernel; larger BK alone was worth only ~1%. Emitter parameterized (MMA-BK/PAD/STAGES/DYNSMEM + fragment mode) with fail-closed E-MMA-SMEM legality (executed negative); MMA-owned cp.async double/single-buffer pipeline for non-default configs; cuFuncSetSharedSize + cuFuncSetAttribute driver bindings (TRUSTED rows owned by the open ptx-m1 driver dot) + bench SHARED! for >48KiB dynamic shared. DEFAULT DELIBERATELY UNCHANGED (byte-identical emit for all 3 modes, diff-verified) - flipping it is fenced/coupled and now dotted. Device: mma-gemm-check element-exact for all 10 configs incl. dynamic-smem double-buffer; sweep rows committed (orin-nx-25w, 408 MHz noted). zed as-found verified incl. GPU clock untouched. Gates on the exact merged tree: opt-test/saxpy/regress ok, maki 0 FAIL, trust-lint 722/748/0 + inventory unclassified 0 + strict green, suite-coverage 107/0, host 0, filemap 796/0, kernel-perf rc 0, typed-local-diff exit 0, full run.f RUN_EXIT=0 (perf-verdict marginal-pass 2-of-3 under load). RESIDUALS DOTTED: 918 MHz clock-pinned re-measure (+ competitive-row refresh), ship-swizzle-as-default (fenced lower-mm + opt-test cvt pin), autotuner axis wiring (fenced), optional XOR swizzle."
---

After ldmatrix + bigger warp tile: raise BK past 32 (fewer bar.sync, more compute/sync) and add a swizzled/padded As/Bs shared layout so the fragment loads are bank-conflict-free. Feed the cad-6 shape-keyed autotuner the (BK, warp-tile, stages, swizzle) axes. Keep mma-gemm-check element-exact + tf32 golden green; the target is climbing above the 940 FP32 CUDA-core roof toward Triton.

Claim: agent=mmabk workspace=.jj-ws/fable-mmabk
