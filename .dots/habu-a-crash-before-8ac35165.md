---
title: "A crash before the rename leaves an atomic write's temp file"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T17:58:10.084448+01:00"
---

Problem: a crash between FS:RESERVE-SIBLING (src/lib/fs-mutate.f:724) and the rename leaves <dir>/.tmp-<seed>-<n>, a 0600 copy of the content, for good: the name does not record its target, so no later write or caller can reclaim it safely in a shared directory such as HOME. Acceptance: no temp file outlives a crashed write, or the next write to the same target reclaims it, with no reclaim of another writer's live sibling. Depends: none. Claim: unassigned.
