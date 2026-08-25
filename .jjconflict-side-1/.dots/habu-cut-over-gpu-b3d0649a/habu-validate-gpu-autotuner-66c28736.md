---
title: Validate GPU autotuner candidates
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:03:42.480194+02:00"
blocks:
  - habu-produce-gpu-schedule-83f6ad52
---

Full context: design Wave E allows the autotuner to benchmark only candidates whose KIR/GIR/PTXIR2, witnesses, launch, resources, and correctness policy validate. Acceptance: invalid candidates never compile/run/promote; crashes/timeouts/wrong outputs remain evidence, not silent fallback; replay is content-keyed.
