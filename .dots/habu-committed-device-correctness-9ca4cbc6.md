---
title: Committed device-correctness tests for GEMM/attention/fused kernels
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T16:53:45.385660+02:00"
blocks:
  - habu-make-ptx-device-c0eb12a3
  - habu-add-ptx-planner-30b93e8c
  - habu-automatic-op-fusion-329aac27
  - habu-tiled-gemm-codegen-76075375
  - habu-ptx-m11-attention-fa7b0598
  - habu-add-logits-domain-a1489686
---

File: PLAN.md:639. Gap: GEMM, attention, fusion, loss, and benchmark kernels
need committed Orin device proofs through the generic PTX runtime, not
throwaway `/tmp` runners or Python graders. Fix: add/rewrite checked Habu device
tests that emit -> ptxas -> launch -> compare CPU goldens using private temp
roots, target capability data, and generic profile rows for fused, GEMM/MMA,
attention, and CE/loss paths. Verify: the Orin device gate reruns from the tree,
fails on stale cubin/missing ptxas/wrong golden/device mismatch, and is listed in
the focused PTX gate.
