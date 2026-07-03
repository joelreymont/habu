---
title: "CAD 9: model-op adjoints + model-IR reverse transform"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T01:29:11.726974+02:00"
---

CAD-PLAN section 12 (plan-review round-2 quorum: Phase 9 'no new dot' was wrong - lib/ptx/ad.f is v0 token substitution over kernel primitives, ad-dag.f covers only the softmax-rows primitive set; neither keys by model op nor emits model-IR nodes). Deliver: model-op adjoint registry (the op-registry vjp field points here: gelu/silu/linear/matmul/softmax-row/norms/NLL losses), reverse transform over the cad-1 node table emitting backward regions AS model-IR nodes so they enter the same planners, save-vs-recompute DECISION under the shared bytes/FLOPs cost model (VJP-SAVES today is only a count), gradcheck per adjoint + per fused backward region. Blocks: GRADCHECK gate (milestone 14), habu-maki-from-scratch. Related: habu-epic-maki-autograd chain, lib/ptx/ad-dag.f as kernel-level substrate. Depends: cad-1, habu-maki-tensor-value.
