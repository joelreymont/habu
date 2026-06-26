---
title: "Maki: training loop + gradient checkpointing"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:50.989807+02:00"
blocks:
  - habu-write-docs-maki-a4c85dfe
---

D. Implement the training/eval loop + gradient checkpointing per docs/maki/train.md: compose forward, the checked backward, and the optimizer into a step; checkpoint/rematerialization policy.
- Files: maki/train.f.
- Verify: a tiny model trains a few steps with loss decreasing vs a CPU golden; checkpointing reproduces the same gradients.
- Dep: docs/maki/train.md (habu-write-docs-maki-a4c85dfe) + maki tensor types + maki autograd orchestration + maki optimizers.
