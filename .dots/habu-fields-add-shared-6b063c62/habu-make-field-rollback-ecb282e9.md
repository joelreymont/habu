---
title: Make field rollback snapshots byte-canonical
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T14:16:55.120825+02:00"
---

Full context: src/core/type-family.f PF-ROLLBACK near the shared PF validator only rewinds PF-N/high-water state, while TFAM-SNAPSHOT-PERSIST near the snapshot hooks persists the full PF record capacity. Retired bytes from rejected or nested declarations therefore remain observable in snapshot/fixpoint identity even when logical registry state is identical. Fix the root invariant: rollback scrubs every retired PF record and arena tail back to canonical zero, and persistence serializes only committed rows with canonical zero fill for unused capacity. Preserve nested begin/commit/rollback semantics and monotonic token safety. Add checked regressions for failed declaration, nested rollback, growth/reuse, identical logical histories yielding byte-identical snapshots, AOT restore, and native fixpoint. Verify type-family/type-decl suites, snapshot/AOT/fixpoint gates, typed-local diff lint, and full native gate. Ownership: shared PF rollback and persistence only; coordinate with field-token hardening and the broader cross-scope mutation rollback dot.
