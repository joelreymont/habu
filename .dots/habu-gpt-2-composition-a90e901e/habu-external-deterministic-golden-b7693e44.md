---
title: External deterministic golden for the block
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T22:41:31.077467+02:00\""
---

The block forward golden compares the executor to GBR-FWD, an internal hand-written reference built to mirror the engine's contraction order cell-for-cell - self-consistent, not external. Produce an external deterministic golden (PyTorch in the ~/Work/ml venv, the adam-torch-ref pattern: fixed seeds, committed fixture with provenance) for the full attention-bearing block forward AND a training-step trace, and assert against it.

Claim: agent=blockgold workspace=.jj-ws/fable-blockgold machine=spark (owns the external PyTorch golden fixture + its assert block in gptblock-attn-test.f; NOTE the tie-in-block lane concurrently edits the same test file - orchestrator hand-merges)
