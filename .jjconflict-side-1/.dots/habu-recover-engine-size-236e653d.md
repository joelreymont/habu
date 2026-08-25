---
title: Recover engine size-guard / AOT helper sharing lane
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T20:49:55.906700+02:00"
---

Forensic sweep 2026-07-19: the engine size-guard + AOT shared-helper campaign is stranded, not on master, and unlike the other recovered campaigns has no dedicated recovery dot - only the original design dot habu-shrink-the-c-721f214d (active). Superset preserved as bookmark recover-size-guards (pushed to origin, 5 own commits touching src/habu/aot-closure.f, aot-lib.f, habu1.f, habu2.f, bootstrap/cg/forth.fs, test/engine-suite.f, test/gate-aot-positive-lib.f, test/gate-aot-negative-lib.f, test/seal-absence.f, plus three nested dots under habu-shrink-the-c-721f214d). A second tip recover-size-guard-rebase (4 own commits) is preserved ONLY in the Mac jj store: conflicted ancestor commits block a git push, so it cannot reach origin as-is - harvest it on the Mac. Subset lane directories were retired after the sweep. The lanes are roughly 261 commits behind; re-derive against today's engine under habu-shrink-the-c-721f214d, review, and land in slices.
