---
title: "Give WL:MAX a reader or remove it"
status: open
priority: 3
issue-type: task
created-at: "2026-11-05T12:39:52.468259+02:00"
---

Found by the WL rename review (2026-11-05). src/habu/layout.f:142-143 says WL:MAX ($FFFFFFFE) 'bounds every WID-indexed table and the snapshot's registry validation', but no code reads it (master had no reader of WID:MAX either); habu2.f:12395 names it in a comment. Acceptance: either a wid-indexed table or validation reads WL:MAX and the comment is true, or WL:MAX, its mirror and the comments go. Workspace: none yet. Claim: unassigned.
