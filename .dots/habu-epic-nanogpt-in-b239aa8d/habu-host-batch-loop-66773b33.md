---
title: Host batch-loop grad-accum trainer
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T17:36:22.585491+02:00\""
---

Interim nanoGPT training execution: run the 2D IR once per sequence in a host loop binding each T x C slice via EX-BIND (executor.f:413), accumulating parameter gradients across iterations via a per-slot running-sum extension of SC-GRAD-AT (from-scratch-train.f:80) + ADAM-UPD/ATN-APPLY (adam-train.f:250-253); Adam applied once per step from accumulated grads. Same B*T-row layout as the final design so it is replaced, not rewritten, by the segment op. No IR/checker change. Fail-closed: running buffer zeroed at step start; focused test for the silent grad-leak across steps. Full contract: docs/batch-sequence-design.md section 5 BTC-3.

2026-07-20 serialization released (grad-clip landed e25762ae).
Claim: agent=batchloop workspace=.jj-ws/fable-batchloop machine=spark (owns maki/adam-train.f from-scratch-train.f + executor read-consumers + existing test files; checkpoint stays serialized behind this lane)
