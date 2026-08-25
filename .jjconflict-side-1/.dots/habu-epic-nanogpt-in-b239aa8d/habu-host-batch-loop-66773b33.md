---
title: Host batch-loop grad-accum trainer
status: closed
priority: 2
issue-type: task
created-at: "2026-07-18T17:36:22.585491+02:00"
closed-at: "2026-07-20T12:05:54.540863+02:00"
close-reason: "Landed 24b9f3f6: BTC-3 interim host batch-loop per docs/batch-sequence-design.md section 5 - one 2D IR run per sequence over the B*T-row AMT-BX buffer (B outermost), per-slot running-sum accumulation, MEAN normalization by 1/B before ONE AdamW step (nanoGPT semantics; clip applies to the meaned accumulated norm, clip-after-accumulation order). Pure additions, 325 lines, zero deletions. Proofs: accumulator equals sum of independent per-sequence reference gradients within 1e-8; B=1 batch run bit-exact equal to the single path; grad-leak fail-closed red-first (zeroing removal fails 4 poison assertions); B=4 locks 188->-2453 deterministic; clip and schedule composition locks distinct; single-path -2749 survives. Replaced-not-rewritten by the segment op (BTC-1) by design, labeled INTERIM in code"
---

Interim nanoGPT training execution: run the 2D IR once per sequence in a host loop binding each T x C slice via EX-BIND (executor.f:413), accumulating parameter gradients across iterations via a per-slot running-sum extension of SC-GRAD-AT (from-scratch-train.f:80) + ADAM-UPD/ATN-APPLY (adam-train.f:250-253); Adam applied once per step from accumulated grads. Same B*T-row layout as the final design so it is replaced, not rewritten, by the segment op. No IR/checker change. Fail-closed: running buffer zeroed at step start; focused test for the silent grad-leak across steps. Full contract: docs/batch-sequence-design.md section 5 BTC-3.

2026-07-20 serialization released (grad-clip landed e25762ae).
Claim: agent=batchloop workspace=.jj-ws/fable-batchloop machine=spark (owns maki/adam-train.f from-scratch-train.f + executor read-consumers + existing test files; checkpoint stays serialized behind this lane)
