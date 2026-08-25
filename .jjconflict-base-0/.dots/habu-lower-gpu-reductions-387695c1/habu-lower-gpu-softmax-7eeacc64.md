---
title: Lower GPU softmax
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:02:41.438259+02:00"
blocks:
  - habu-schedule-gpu-reductions-08d0b16f
---

Full context: port the current row-reduction and softmax model lowering through KIR, scheduled GIR, and PTXIR2 under exact or bounded numeric policy. Acceptance: CPU/device golden, finite-difference gradcheck, backward/forward consistency, masks, tails, and sentinel pass without string emitters.
