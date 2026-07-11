---
title: V2 differential runner core
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.511089+02:00"
blocks:
  - habu-v2-differential-suite-2d896ced
---

Implement isolated deterministic differential execution for one scalar checker suite and one tensor forward suite. Store every input/output/environment, compare under declared domain, minimize discrepancies without replacing the original, and emit evidence or structured counterexample diagnostics. Acceptance: injected mismatch minimizes and replays, timeout/crash is distinct from numeric mismatch, PyTorch reference adapter remains outside Habu semantics, and success evidence is subject/suite/environment keyed.
