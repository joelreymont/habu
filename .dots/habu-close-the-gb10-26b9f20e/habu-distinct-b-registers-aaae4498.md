---
title: Distinct B registers and burst HMMA issue
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T10:52:07.571462+02:00"
---

Round-4 lever of the GB10 gap campaign (header habu-close-the-gb10-26b9f20e), from the SASS forensic (dot habu-read-triton-s-f85d20be, wf_babd5cc5-21f journal): Habu's K-loop substep reuses ONE B-register pair (%r54,%r55) for every n-tile - each n-tile's B load is WAR-blocked behind the previous n-tile's 4 mmas, load-to-use distance ~2 instructions, so ptxas inserts ~40 stall NOPs per 64 HMMAs (0.64 NOP/HMMA); Triton's SASS has zero. Restructure the substep emitters in lib/ptx/cg-mma.f (MMA-B-RAW, MMA-B-LDM-WIDE, MMA-NTILE-WIDE, MMA-KSTEP-WIDE): give each n-tile its own B register pair (8 regs for 4 n-tiles) and emit load-ALL-B-fragments-then-issue-the-16-mma-burst, removing the WAR chain so ptxas can hoist the shared loads. Register budget: currently 96-128 reg/thread with 0 spills, headroom to 255, but occupancy must be re-measured (3 blocks/SM on the static 4-warp tile is the win to protect). Proof protocol: element-exact via mma-gemm-check for every affected config FIRST; byte-identity is NOT expected (the substep changes) so instead re-dump SASS and REPORT the new NOP/HMMA count - the scout's honest caveat is that ptxas already allocated distinct physical regs yet still stalled, so the win must be proven by the NOP count then the clock, not assumed; then the doc timing protocol, extend eval-triton.md Round 4 + perf-rows.tsv. Expected compound with the epilogue at 512/1024 where stall fraction is largest.
