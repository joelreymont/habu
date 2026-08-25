---
title: Build fusion RIR
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:03:42.450570+02:00"
blocks:
  - habu-move-gpu-knobs-1a190b82
---

Full context: design GPU Wave E makes immutable GPU-RIR the fusion output, preserving logical model regions, dependencies, effects, shapes, numeric policy, and source identity before KIR. Acceptance: illegal fusion/effect/order/shape/target cases reject; equivalent regions encode deterministically; no PTX or schedule facts leak into RIR.
