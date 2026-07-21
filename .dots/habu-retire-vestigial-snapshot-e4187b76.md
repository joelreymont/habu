---
title: Retire vestigial snapshot call-rebase scan
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T06:27:08.990358+02:00\""
---

Loose end from the direct-BL landing (1e9a3926): EM-SNAPSHOT-REBASE-CALLS in src/habu/habu2.f is now a correct NO-OP - no absolute movz/movk call chains remain to rebase, and BL is rebase-invariant because the code region is pinned at __text+REGION-OFF. Snapshot tests pass with it vestigial. Retire the scan (and its callers/constants) as its own change; src/ change so exact-CODELEN Linux rows re-measured same-commit; snapshot suite + fixpoint proof.

Claim: agent=snapscan workspace=.jj-ws/fable-snapscan machine=spark (owns retiring EM-SNAPSHOT-REBASE-CALLS in src/habu/habu2.f + CODELEN Linux rows same-commit; NOTE Mac sizepins lane edits macOS rows in the same ratchet files - different lines, orchestrator hand-merges)
