---
title: External deterministic golden for the block
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T22:41:31.077467+02:00"
---

The block forward golden compares the executor to GBR-FWD, an internal hand-written reference built to mirror the engine's contraction order cell-for-cell - self-consistent, not external. Produce an external deterministic golden (PyTorch in the ~/Work/ml venv, the adam-torch-ref pattern: fixed seeds, committed fixture with provenance) for the full attention-bearing block forward AND a training-step trace, and assert against it.
