---
title: Record a call row for a naked-variable call refused before its step
status: open
priority: 3
issue-type: task
created-at: "2026-10-25T08:43:30.954866+01:00"
---

After 518a9737 a type-rejected generic call spliced in a TRUSTED: body records its call row, but a naked-variable call refused before its step (the UNJ-SETTLE path in src/core/checker.f) still records none. Not yet shown to change behavior: first reproduce at both tiers and show an observable failure; record the row on that path only if one exists, else close this dot with the evidence. Found by the eb23ce2d lane review, 2026-10-25. Claim: unassigned.
