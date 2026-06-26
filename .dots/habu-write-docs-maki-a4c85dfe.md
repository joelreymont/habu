---
title: Write docs/maki/train.md design
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:19.324298+02:00"
---

GATE. Design the training/eval loop + gradient checkpointing/rematerialization (how the forward, the checked backward, and the optimizer compose into a step; checkpoint policy).
- Files: new docs/maki/train.md.
- Verify: training step + eval + checkpoint policy specified.
- Dep: none. Gates maki training-loop impl.
