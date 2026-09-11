---
title: Enforce native region size budgets
status: closed
priority: 1
issue-type: task
created-at: "2026-07-18T11:52:19.533722+02:00"
closed-at: "2026-07-21T16:27:50.340568+02:00"
close-reason: "Landed 29b66ebd + re-measure 7216c664: per-region engine-text budgets close the attribution gap the whole-file and total ratchets leave open. A committed 43-row per-region budget manifest (measured at the byte fixpoint, asserted to sum exactly to the committed text total) enforced directionally in the build gate: over-budget fails NAMING the region, under-budget fails stale, new-unbudgeted and vanished rows both fail closed. Proof both ways includes the exact gap scenario: a compensating +4/-4 swap between regions keeps every existing ratchet green while the new one fails naming the region. Budgets live in the gate manifest, NOT baked into the engine (baking would self-reference the fixpoint) - engine byte-identical at the landing. And the ratchet earned its keep within the hour: the Mac's structure-make landing (+48) attributed exactly as startup +16 and dictionary-code +32 on its first live drift. Full tests green at the merged tip"
---

Context: test/gate-build-size.f catches only final container growth, so a 216-byte region regression can cross a 16 KiB page after merge and obscure its originating emitter. Fix: turn src/habu/engine-size.f region marks into committed per-target budgets with exact before/after deltas, fail on growth or stale shrink in the owning change, and retain independent __text and container ceilings. Require an explicit evidence row for any approved increase; generated artifacts remain uncommitted. Acceptance: +4 bytes in each representative region fails naming that region and baseline; shrink requires ratchet update; unchanged fixpoint is green; macOS page-crossing prediction is reported from measured layout, never inferred for Linux. This can land independently before typed IR; canonical IR later becomes its producer. Files: src/habu/engine-size.f, test/gate-build-size.f, test/gate-engine-lib.f, docs/size-rca.md. Verify: focused budget mutation fixtures, exact size map, byte-identical fixpoint, full native size gate.

Claim: agent=region workspace=.jj-ws/fable-region machine=spark (owns native region size budget enforcement)
