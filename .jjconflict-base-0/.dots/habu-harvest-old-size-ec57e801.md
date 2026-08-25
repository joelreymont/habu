---
title: Harvest old size-guard lane snapshots
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T11:33:20.665614+02:00"
---

Forensic sweep 2026-07-19: seven old size-guard lanes predate the ACTIVE recovery (dot habu-recover-size-guard-31d26b61 / habu-shrink-the-c-721f214d re-derives this work). Before retiring them, diff each tip against the landed slice-1..slice-3 outcome and harvest anything not re-done. Lanes: size-guards (dirty tip with 7 modified engine-size files; bookmark recover-size-guards, pushed to origin), size-guard-rebase (bookmark recover-size-guard-rebase, LOCAL-ONLY: chain contains a conflicted commit 396cf28a jj refuses to push - resolve locally if harvesting), size-guard-integration, size-guard-claims (pre-existing bookmark, pushed), habu-ratchet-measured-engine-b4032d74, habu-repair-bootstrap-shared-43b927d9, habu-repair-shared-guard-5518ad25. Close by recording per-lane verdicts (re-done vs harvested) and retiring all seven.
