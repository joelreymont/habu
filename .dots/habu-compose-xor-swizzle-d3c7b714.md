---
title: Compose XOR-swizzle with the grouped-raster 4096 tile
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T09:52:29.981350+02:00\""
---

Follow-up flagged by the XSWIZ landing (f9d6874b, dot habu-xor-swizzle-mma-cd2d2009): the 4096^3 record (0.75x Triton, round 10 grouped-raster CTA ordering) was NOT in the XSWIZ sweep, and XSWIZ separately lifted the 4-warp family +50% at 4096^3. Compose MMA-XSWIZ with the round-10 grouped-raster winner config and measure whether the 4096 record moves; also re-check whether the composition changes the 2048 best (0.95x, the new XSWIZ record). Proof order per the round idiom: element-exact first (mma-gemm-check rows for the composed configs, 0 mismatches), then best-of-3 solo timing vs the SAME-SESSION re-run of both parents (XSWIZ-only and raster-only), then a doc round section incl. honest negatives. Legality: verify E-MMA-XSWIZ's split-K/ablate fences do not wrongly fence raster (raster+swizzle must be a LEGAL combination or the fence must be justified). GPU TIMING LANE - solo discipline, nvidia-smi idle check before each pass. Files: lib/ptx/cg-mma.f legality only if needed, tools/ptx/mma-gemm-check.f rows, tools/ptx/gemm-bench.f wrappers, docs/eval-triton.md round section.

Claim: agent=xswzraster workspace=.jj-ws/fable-xswzraster machine=spark (THE session GPU timing lane; owns tools/ptx bench/check rows + doc round; cg-mma.f legality only if needed)
