---
title: Fix already-dead race in proc-watch smoke
status: open
priority: 2
issue-type: task
blocks:
  - habu-synchronize-proc-watch-b92e8257
created-at: "2026-07-23T12:06:17.042103+02:00"
---

Duplicate control record only. It is unassigned and superseded by
`habu-synchronize-proc-watch-b92e8257`.

The proposed `/proc` zombie-state probe is not an implementation candidate and
must never land. The accepted synchronize design uses the production watch
primitive itself as the non-consuming death barrier, then opens a second watch
on the zombie.

Close this dot only after commit
`a5d1365518c53bf8c223d1428ce9c8fe8ee025b7` lands through the synchronize dot
and its integration gates pass. Do not implement or claim this duplicate.
