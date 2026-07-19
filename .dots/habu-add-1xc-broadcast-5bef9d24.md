---
title: Add 1xC broadcast multiply op for transformer affine
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T20:12:23.604059+02:00\""
---

Found by the layernorm lane 2026-07-15: the maki op set cannot express a per-feature (1xC) affine SCALE - OP-SCALE is scalar 1x1, OP-MUL requires same-shape operands - so the layernorm fixture used scalar gamma + per-feature beta (OP-BIAS is 1xC). A faithful transformer layernorm gamma needs a 1xC-broadcast multiply: new op-registry entry (shape legality: AxC by 1xC -> AxC), executor support, EW lowering (mirror OP-BIAS's broadcast pattern in lower-ew), fusion-class EW so it fuses into reduction epilogues like BIAS does, host+device tests, and the layernorm bench upgraded to vector gamma. Acceptance: AxC*1xC broadcasts correct host+device; wrong-shape rejects; layernorm-with-vector-affine plans one region and stays golden. Files: maki op registry/model-ir surface, executor, lower-ew (FENCED - coordinate), tests. Verify: op/executor tests, lower-ew tests, maki/test.f, device golden. Ownership: maki op set capability.

Claim: agent=bcast1xc workspace=.jj-ws/bcast1xc machine=spark
