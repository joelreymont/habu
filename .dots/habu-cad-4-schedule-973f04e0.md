---
title: "CAD 4: schedule object + cache key"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:35.490948+02:00"
---

docs/model-cad.md Phase 4. Checked schedule object: region, target, shape/dtype/layout keys, block size, vector width, tile M/N/K, warps, stages, smem layout, fragment shape, epilogue, save/recompute policy, measurement history. TILE prints candidates before emission; all candidates recorded; replay by key; winner cached per shape/dtype/layout/target. Key compare hand-written until habu-checker-capability-derive lands. Families v1: elementwise-v1, row-reduction-v1, softmax-row-v1, gemm-tf32-v1. Related: habu-ptx-m9-bench. Depends: cad-0a.

UPDATE (plan-review fold, 2026-07-04): families are FIVE incl. decode-v1 (PBD chains; ops/references arrive with LA dots). Schedule object does NOT carry save/recompute policy (fusion-plan field, CAD-PLAN 12). Epilogue is fixed by the fusion plan, never searched. Store dependency: habu-cad-5-artifact-9a3d5a56.
