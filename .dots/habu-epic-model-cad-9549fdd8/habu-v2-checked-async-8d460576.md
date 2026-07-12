---
title: V2 checked async pipeline
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.059595+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:1397-1417 requires cp.async multistage lowering, but the checker cannot yet prove shared-tile lifetime, barrier phase, or pipeline-depth legality. Fix: implement the first typed two-stage global-to-shared pipeline capability and lower one GEMM tile through it. Acceptance: missing wait, wrong barrier phase, shared lifetime escape, and unsupported target reject statically; emitted cp.async sequence passes ptxas and device golden. Files: src/core/checker.f, lib/ptx/tile.f, lib/ptx/cg-matmul.f, test/type-ptx*.f. Verify: negative checker fixtures, ptx-stdlib gate, Orin golden.
