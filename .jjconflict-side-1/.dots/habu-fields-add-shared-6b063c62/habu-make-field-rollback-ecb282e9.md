---
title: Make field rollback snapshots byte-canonical
status: closed
priority: 1
issue-type: task
created-at: "2026-07-18T14:16:55.120825+02:00"
closed-at: "2026-07-21T06:39:56.181876+02:00"
close-reason: "Landed 4f6ac573: PF-ROLLBACK and TFAM-ROLLBACK-RESTORE scrub retired product-field rows to canonical zero via PF-SCRUB (pointer-free records, zeroed row = canonical absent row); PF-PERSIST-CANONICAL zero-fills unused live capacity and the dead boot buffer before snapshot persist, so identical committed registries bake byte-identically regardless of rollback history. Falsified non-vacuous (scrub removed -> F390). Suites + battery green; fixpoint x2 byte-identical."
---

Full context: src/core/type-family.f PF-ROLLBACK near the shared PF validator only rewinds PF-N/high-water state, while TFAM-SNAPSHOT-PERSIST near the snapshot hooks persists the full PF record capacity. Retired bytes from rejected or nested declarations therefore remain observable in snapshot/fixpoint identity even when logical registry state is identical. Fix the root invariant: rollback scrubs every retired PF record and arena tail back to canonical zero, and persistence serializes only committed rows with canonical zero fill for unused capacity. Preserve nested begin/commit/rollback semantics and monotonic token safety. Add checked regressions for failed declaration, nested rollback, growth/reuse, identical logical histories yielding byte-identical snapshots, AOT restore, and native fixpoint. Verify type-family/type-decl suites, snapshot/AOT/fixpoint gates, typed-local diff lint, and full native gate. Ownership: shared PF rollback and persistence only; coordinate with field-token hardening and the broader cross-scope mutation rollback dot.
