---
title: "Maki: training loop + gradient checkpointing"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:50.989807+02:00"
---

D. Implement the training/eval loop + gradient checkpointing per docs/maki/train.md: compose forward, the checked backward, and the optimizer into a step; checkpoint/rematerialization policy.
- Files: maki/train.f.
- Verify: a tiny model trains a few steps with loss decreasing vs a CPU golden; checkpointing reproduces the same gradients.
- Dep: docs/maki/train.md + maki tensor types + maki autograd orchestration + maki optimizers.

ADAM HALF LANDED 2026-07-11 (fable d1194aaf): Adam-driven training step (shared
tick + bias-correction chain, ADAM-TICK/C1/C2 driving OPTIM:TT-ADAM! per param
from BW-BUILD gradients), MLP-under-Adam (60 steps, committed pins, converged)
and 1-layer attention block trained end-to-end (GC-ATTN shape, 40 steps, loss
strictly decreasing, pins), per-step gradient parity vs an INDEPENDENT
element-level reference for 3 Adam steps (bit-identical; falsification-proven
harness). Destruction review: 0 defects. REMAINING on this dot: the gradient
CHECKPOINTING half - docs/maki/train.md names it only as roadmap with NO
concrete policy (worker verdict: unspecified-residue; the step keeps full
deterministic forward recompute). Spec the policy in docs/maki/train.md first,
then implement; do not invent policy.
