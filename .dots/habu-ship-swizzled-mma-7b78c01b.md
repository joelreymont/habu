---
title: Ship swizzled MMA config as production default
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T22:11:50.766761+02:00"
---

Residual from habu-mma-larger-bk-1ae1c6b2 (2026-07-15): the proven best MMA config (BK=32 MMA-PAD=8 stages=2 ldmatrix, +53.5% over the shipped scalar+cvt default at equal clocks, element-exact, fits 36 KiB static shared) is committed as an OPT-IN because flipping the default is coupled three ways: (1) the FENCED maki/lower-mm.f LMM-MMA-BODY shares the emitter's global config (sol's region territory + makipools remainder - coordinate); (2) lib/ptx/opt-test.f pins cvt.rna.tf32.f32 which the ldmatrix path does not emit - the pin must be re-derived honestly for the new default; (3) the shipped 884.9 GFLOP/s competitive golden must be refreshed at the historical 918 MHz clock first (habu-re-measure-mma dot). Fix when the fence releases and the clock re-measure lands: flip the emitter default to the swizzled config, re-derive opt-test pins, run the full lower-mm/maki device goldens, update the competitive row + docs. Acceptance: default emit = swizzled config, all device goldens green, honest new competitive row, no consumer left on the slow path unintentionally. Files: lib/ptx/cg-mma.f default knobs, lib/ptx/opt-test.f, maki/lower-mm.f coordination, perf/competitive rows. Ownership: ptx MMA production config.

UPDATE 2026-07-17 (bfeed landing 7c7e7102): the flip target should be
re-evaluated - the new measured best is MMM-WIDE-M2 (MMA-MFRAGS=2, 128x64
block, DYNAMIC smem 57344B, 2133.9 GFLOP/s = 1.13x Triton), which adds two
consumer-visible couplings beyond the swizzled 64x64 config: dynamic .shared
at launch and block-M-aware grids (gridY = M/128, M multiple of 128; small/
ragged shapes need the MFRAGS=1 path or padding). The stages=1 STATIC 128x64
variant (2084.1, 28672B) avoids the dynamic-smem coupling at -2.4%.

UPDATE 2026-07-17 (wave2 landing cb8fa57e): best is now MMM-WIDE-M4-S1
(MFRAGS=4 stages=1 STATIC 49152B, 2707.3 = 1.43x Triton) which REMOVES the
dynamic-smem coupling flagged in the previous update (static fits the 48KiB
opt-in cap? verify: 49152B = 48KiB exactly - check the static .shared limit
on sm_87 before flipping) but tightens the shape coupling to M multiple of
256. The B-side ldmatrix fragment proof (mma-probe.f MP-BLDM-ALL) also
feeds this flip: transposed Bs staging changes the staging emitters the
default flip must carry.

UPDATE 2026-07-17 (wave3 landing 58faceba): flip target moves again -
MMM-WIDE-B-M4-S1 (3026.6 = 1.60x Triton) is single-buffer but DYNAMIC
50176B (> the 49152B sm_87 static cap by 1KiB), so the dynamic-launch
coupling RETURNS for the best config; the flip decision should weigh
MMM-WIDE-M4-S1 (2707.3, static 49152B, no coupling) vs the extra +11.9%.
B-ldmatrix also adds the transposed-Bs scalar staging emitters to the
surface the flip must carry.

RETRACTION 2026-07-19 (dot habu-retract-or-re-698be8b3): the
MMM-WIDE-B-M4-S1 "3026.6 = 1.60x Triton" flip target (and the 1.13x and
1.43x rungs) cited in the UPDATE notes above are Orin-only measurements.
The Orin has been retired as a measurement platform (Joel, 2026-07-19) and
that Triton head-to-head is unverified and likely mistaken - the
default-flip decision must not rest on it until the config is re-measured
on the sole benchmark platform, the GB10 spark. GB10 conclusions stand on
their own referee runs.
