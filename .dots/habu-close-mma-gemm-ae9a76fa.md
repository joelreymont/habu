---
title: Close MMA GEMM gap to Triton parity
status: open
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-15T22:54:12.720615+02:00\\\"\""
---

User goal 2026-07-15: the swizzled TF32 mma.sync GEMM measures 1369.6 GFLOP/s at 918 MHz = 72.4 percent of the committed Triton baseline (1890.5, same clock class); hardware therefore permits >=1890 - close the remaining ~28 percent. METHOD, profile-first: (1) PROFILE the 1369 kernel on zed before touching anything - occupancy, achieved SM/mma issue rate, smem + DRAM throughput, stall reasons (build a small reusable profiling harness per the build-the-tool rule if none exists; nsys/ncu availability on the box to be checked, else PTX-level counters/timing decomposition) - and let the profile rank the levers. (2) The lever list, each already parameterized or dotted: deeper cp.async pipeline (MMA-STAGES 3-4; the knob exists, nothing >2 measured), wider warp/register tiles (more FMA per smem read), XOR swizzle (habu-xor-swizzle-mma-cd2d2009 - frees the 4KiB pad so tiles/stages can grow), instruction interleave of mma.sync with cp.async waits, vectorized store epilogue. (3) Sweep honestly on-device (element-exact mma-gemm-check green for EVERY kept config, rows per measurement discipline at the 918MHz tag), commit the best config + the measured curve. ACCEPTANCE: either >=1890 GFLOP/s (parity) with correctness green, or a profile-backed statement of the reached ceiling and exactly which resource saturates (that becomes the next capability dot). Feed the winning axes to the autotuner dot (habu-feed-mma-config-d783e33b) and the default flip (habu-ship-swizzled-mma-7b78c01b). Files: lib/ptx/cg-mma.f, profiling harness under tools/ptx/, perf rows, mma-gemm-check configs. Ownership: ptx MMA performance. Depends: none hard (cg-mma is unfenced); coordinate rows with the 918mhz tag discipline.


PARKED 2026-07-16 (mmaparity lane): the profile-first tooling LANDED (1aec68f2
tools/ptx/mma-profile.f - config-driven single-launch harness, proven on-device;
69632B true-smem correction; ncu-hang lesson) but the measurement leg is
BLOCKED: the first sudo ncu attach wedged zed's GPU/driver and took the box off
the network (no watchdog reboot; needs a PHYSICAL POWER CYCLE - user action).
Resume protocol when zed returns: verify as-found repo states (~/Work/habu @
76fe83a5 clean, ~/Work/odin-habu @ b0a5a63b clean), remove the leftover
/tmp/mmaparity-run root, then profile via nsys sampling or the harness's
variant-kernel timing decomposition - NEVER ncu-first (LESSONS + orchestrator
memory carry the hazard). Clock as-found was the 408 lock; nothing to restore.

PROFILE-FIRST LEG COMPLETE 2026-07-17 (mmaparity lane; claim released - the
implementation lever is the follow-up dot habu-mma-bfeed-amortize). ATTRIBUTION
(variant-kernel timing decomposition at the 918MHz pin; nsys GPU-metrics is
UNSUPPORTED on this Orin iGPU so counter profiling is impossible here,
independent of the ncu ban; ablation via 8 DCE-safe emit modes): per 12.61ms
iteration at 2048^3 - B-side scalar ld.shared.b32 fragment loads 5.04ms (~40%,
THE bottleneck: 8 loads per K-substep, each 8x8 B fragment feeds only ONE mma,
zero reuse); global cp.async staging+bar.sync floor 7.48ms (hidden behind the
feed); A-side ldmatrix ~free (one ldmatrix.x4 reused 4x); mma.sync issue
0.14ms (~1%). This OVERTURNS the recorded mma-issue/dependency-bound
hypothesis (LESSONS ~L2243, kernel-principles step-3c): at the pad8+ldmatrix
rung the kernel is FEED-BOUND on un-amortized B loads. MEASURED CEILING:
quarter-B-loads proxy runs 7.57ms = 2270 GFLOP/s = 1.20x Triton - parity is
reachable. Config sweep re-confirmed element-exact at the pin: the entire +53%
of the swizzle landing is pad=8 unlocking bank-free ldmatrix (no-pad ldmatrix
is SLOWER than baseline); SWZ-BK64 1368.9 stands, all other knob points lose.
LEVERS RERANKED BY EVIDENCE: #2 wider register tiles is THE lever (amortize B
across >=2 M-frags, proven layouts, lower risk; alternative: B-side ldmatrix
on transposed/swizzled Bs - higher payoff but needs a NEW element-exact
fragment proof in mma-probe.f first); #1 cp.async stages 3-4 attribution-DEAD
standalone (stages1==stages2 within noise; floor already hidden) - revisit
only after the tile grows; #3 autotune of existing knobs EXHAUSTED (block-
shape axis folds into #2); #4 XOR swizzle no standalone win - only as a 4KiB
enabler for bigger tiles; #5 epilogue ~worthless (store not the limiter).
Best shipped remains SWZ-BK64 1368.9 = 72.4% of 1890.5. Zed as-found ==
restored (repo clean at tip, devfreq 408MHz pin restored, 25W, ncu never
attached, box healthy).
