---
title: Optimize LRED layernorm reduction schedule
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T20:12:23.599768+02:00"
---

Found by the layernorm lane 2026-07-15 (evidence in maki/layernorm-fusion-bench-device-test.f + perf-rows LN-FUSE-* rows): the block-per-row LRED layernorm kernel measures ~7.8 GB/s on Orin 25W - ~6.7x off the ~52 GB/s roof the flat-EW kernels hit - so it dominates any chain it appears in and capped the layernorm fusion win at 1.41x (the two eliminated EW round-trips were only ~30 percent of the ablated chain). Fix: optimize the reduction schedule in maki/lower-red.f LRED emission (candidates: multiple rows per block, vectorized loads, warp-shuffle partial sums - see PTX M6 warp-shfl dot for the kernel-level primitive; two BLOCK-SUM passes for mean+var may fuse to one Welford-style pass). Acceptance: layernorm region GB/s materially up (record honest before/after rows), goldens stay green (device==host composed tol), fusion ratio re-measured. FENCED: maki/lower-red.f is in the fenced lower-* set - coordinate/hold until the fence releases. Files: maki/lower-red.f, layernorm bench rows. Verify: lower-red(-device)-tests, layernorm bench, maki/test.f. Ownership: maki reduction lowering perf.
