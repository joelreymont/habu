---
title: Lower elementwise PTXIR2
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:02:29.238923+02:00"
blocks:
  - habu-schedule-elementwise-gir-86a20801
---

Full context: lower validated elementwise GIR into the existing sole GPU-PTXIR2 owner and exact passes, rendering only at the final sink. Acceptance: instruction/state/resource validators pass; address CSE/load reuse are structural; no operation emitter writes text.
