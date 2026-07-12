---
title: "maki: typed seam for LLA-STAGE-MM M/N/K triple"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T20:49:40.220583+02:00"
---

Typed-launch worker observation 2026-07-12: LLA-STAGE-MM (maki/lower-launch.f) copies LMM-M@/N@/K@ raw ( -- n ) accessors into ordered LLA-PM/PN/PK param slots - a same-type ordered triple where a transposition compiles silently (runtime-guarded by E-LMM-DIMS at LMM-CHECK-SHAPES only for shape-inconsistent swaps). The seam lives in lower-mm.f (owns M/N/K). Options: (a) M=rows(A) K=cols(A) N=cols(B) typed accessors reusing CAD-KIND rows/cols where roles genuinely map, with a typed LLA-SET-MNK! setter; (b) if M/N/K deserve their own kinds, that is a cad-kinds.f addition - align with the V2 plan's dim-kind roadmap before minting. Small, after habu-maki-typed-storage-2279c4b0 (same files).
