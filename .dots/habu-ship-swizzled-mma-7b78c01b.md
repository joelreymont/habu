---
title: Ship swizzled MMA config as production default
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T22:11:50.766761+02:00"
---

Residual from habu-mma-larger-bk-1ae1c6b2 (2026-07-15): the proven best MMA config (BK=32 MMA-PAD=8 stages=2 ldmatrix, +53.5% over the shipped scalar+cvt default at equal clocks, element-exact, fits 36 KiB static shared) is committed as an OPT-IN because flipping the default is coupled three ways: (1) the FENCED maki/lower-mm.f LMM-MMA-BODY shares the emitter's global config (sol's region territory + makipools remainder - coordinate); (2) lib/ptx/opt-test.f pins cvt.rna.tf32.f32 which the ldmatrix path does not emit - the pin must be re-derived honestly for the new default; (3) the shipped 884.9 GFLOP/s competitive golden must be refreshed at the historical 918 MHz clock first (habu-re-measure-mma dot). Fix when the fence releases and the clock re-measure lands: flip the emitter default to the swizzled config, re-derive opt-test pins, run the full lower-mm/maki device goldens, update the competitive row + docs. Acceptance: default emit = swizzled config, all device goldens green, honest new competitive row, no consumer left on the slow path unintentionally. Files: lib/ptx/cg-mma.f default knobs, lib/ptx/opt-test.f, maki/lower-mm.f coordination, perf/competitive rows. Ownership: ptx MMA production config.
