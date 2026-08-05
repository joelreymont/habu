---
title: Rebind trust rows when a dot closes
status: closed
priority: 2
issue-type: task
created-at: "2026-08-03T23:16:35.214816+02:00"
closed-at: "2026-08-04T00:22:52.590913+02:00"
close-reason: ledger retired with the governance mirror
---

TRUSTED.md lines 514-515 named habu-migrate-the-first-fe78ec52 as the owner of the POKE (patch32) and EV (evaluate) boundaries in src/compiler/native/publish.f and migrate.f. Commit 7b5d7ac5fb67 'Close the migrated-entry dot' (2026-08-01 19:04) deleted .dots/habu-migrate-the-first-fe78ec52.md and left both rows pointing at a dot that no longer exists, so 'bin/hb --load tools/trusted-inventory.f -- strict' reports 'missing owning dot'. It was invisible for two days because MAIN-STRICT throws on UNCLASSIFIED# before it reaches STRICT-OWNERS, and 91 sites were unclassified. Repaired in habu-reconcile-the-drifted-48eefbd9 by repointing POKE at habu-checker-capability-gate-14022ba9 (TRUSTED.md's declared owner-of-record for the patch32 boundary) and EV at habu-parse-a-migrated-b38a83d9 (already the owner of the sibling EV row and the dot whose landing removes that evaluate seam). What remains is the structural fix, which is habu-migrate-trust-rows-ba6c0e98's second half: make closing a dot that still owns TRUSTED.md rows a gate red, so the rebinding happens in the closing change instead of being found by whoever next runs strict. Also worth ordering MAIN-STRICT so an unclassified-site failure does not hide an invalid-owner failure - collect both, report both.
