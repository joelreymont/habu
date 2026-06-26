---
title: AD save-vs-recompute policy + cost model
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:00:43.565920+02:00"
blocks:
  - habu-ptx-ir-opt-b90390f0
---

Decomposes ad-reverse. autograd.md Checkpointing: nonlinear adjoints need saved primals/outputs (the tape replacement). Two policies per value: save (stash to global, reload) vs recompute (re-run part of forward in the backward). Pick by an EXPLICIT documented cost model (review found none specified). Plus an equivalence test: save and recompute must produce within-tol-identical gradients for the same kernel.
- Files: src/arch/ptx/ad.f (policy hook) + docs/autograd.md (write the cost model).
- Verify: softmax picks save for small per-row state; a fused path picks recompute; equivalence test passes.
- Dep: the IR/opt layer (habu-ptx-ir-opt-b90390f0) + reverse pass.
