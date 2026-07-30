---
title: Assemble Qwen runtime modules
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T01:51:15.104775+02:00"
blocks:
  - habu-infer-dense-large-a2437ab1
---

Why: BEGIN-QWEN returns an empty closed module inventory and COMPLETE-QWEN must never infer that the operation leaves ran. Interface: DEVRT:BUILD-QWEN ( DEVRT:qbuild -- DEVRT:qwen-build-result ) first requires WEIGHTS filled, then threads the same qbuild through ADD-QWEN-RMSNORM, ADD-QWEN-ROPE, ADD-QWEN-LINEAR, ADD-QWEN-SWIGLU, ADD-QWEN-PAGED, ADD-QWEN-QKV, ADD-QWEN-ATTN, ADD-QWEN-BLOCK, and ADD-QWEN-LOGITS exactly once in that order. It returns built(qbuild) or refused(qbuild,module-build-error); module-build-error is the exact failed slot. Owner: Qwen module assembly order and sole production caller of the nine ADD transitions only. Dependencies: complete operation chain and loaded qbuild weights. Production red: no production DEVRT composition invokes the nine transitions between Qwen weight staging and session completion. Acceptance: the real build fills all nine slots once; injected failure at each step calls no later transition and returns qbuild for DROP-QWEN; missing weights and prefilled/duplicate slots reject before their first illegal mutation; two builds interleave. Forbidden: module implementation, weight load, completion/publication, generic loop, registry, callback, plugin, retry, alternate order, version, or compatibility path. Smallest owning check: bin/hb --load maki/infer/qwen-runtime-build-test.f on DGX Spark.
