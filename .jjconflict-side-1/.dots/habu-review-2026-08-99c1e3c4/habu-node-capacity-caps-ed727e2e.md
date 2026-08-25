---
title: node capacity caps drifted apart
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.995912+02:00"
---

Problem: maki/model-ir.f:137 MIR-CAP $8000 while maki/lower/ew.f:78, mm.f:104, red.f:96, launch.f:373 MDL-CAP and fusion-plan.f:75 FP-CAP keep 128 with comments 'mirrors MIR-CAP'; a model with a node id >= 128 captures and then fails closed in lowering with E-LEW-REG/E-LMM-REG/E-LRED-REG/E-MDL-CUBIN - the wrong error class for a capacity problem. Acceptance: one shared constant or derivation from NODE-COUNT@; a 129-node model lowers or refuses with a capacity code; the lying comments gone. Files: maki/model-ir.f, maki/lower/*.f, maki/fusion-plan.f. Verify: maki/test.f plus the new case. Depends: none. Ownership: maki lowering. Claim: unassigned.
