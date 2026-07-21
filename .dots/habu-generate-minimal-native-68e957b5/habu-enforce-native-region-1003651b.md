---
title: Enforce native region size budgets
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-18T11:52:19.533722+02:00\""
---

Context: test/gate-build-size.f catches only final container growth, so a 216-byte region regression can cross a 16 KiB page after merge and obscure its originating emitter. Fix: turn src/habu/engine-size.f region marks into committed per-target budgets with exact before/after deltas, fail on growth or stale shrink in the owning change, and retain independent __text and container ceilings. Require an explicit evidence row for any approved increase; generated artifacts remain uncommitted. Acceptance: +4 bytes in each representative region fails naming that region and baseline; shrink requires ratchet update; unchanged fixpoint is green; macOS page-crossing prediction is reported from measured layout, never inferred for Linux. This can land independently before typed IR; canonical IR later becomes its producer. Files: src/habu/engine-size.f, test/gate-build-size.f, test/gate-engine-lib.f, docs/size-rca.md. Verify: focused budget mutation fixtures, exact size map, byte-identical fixpoint, full native size gate.

Claim: agent=region workspace=.jj-ws/fable-region machine=spark (owns native region size budget enforcement)
