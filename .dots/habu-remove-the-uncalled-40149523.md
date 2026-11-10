---
title: Remove the uncalled OWNED-CELLS words
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:21:34.730190+02:00"
---

Problem (code review and a peer's hunt, 2026-11-10): IMAGE-LIFECYCLE:OWNED-CELLS (src/lib/image-lifecycle.f:129) and TASK:OWNED-CELLS (src/lib/task.f:2486) have no caller under src, tools or test; their comments describe a stripped image's claim list. Other OWNED-* words in task.f may be uncalled too. Acceptance: each public OWNED-* word in the two files either has a caller named as path:line or is removed with its comment; ../etch searched. Files: src/lib/image-lifecycle.f, src/lib/task.f. Verify: rg for each word; engine built twice equal; suite. Depends: none. Ownership: those words. Claim: unassigned.

Claim: agent=jerry workspace=.jj-ws.noindex/jerry-owned.
