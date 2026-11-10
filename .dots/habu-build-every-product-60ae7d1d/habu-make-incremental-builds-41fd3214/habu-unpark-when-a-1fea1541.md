---
title: "Unpark when a link's key push fails"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:14:38.211828+02:00"
---

Problem: src/habu/linker.f LINK-REQUEST runs 'true PUSH-KEY' between PARK and the 'finally' that calls RETURNED (UNPARK). If LOADERS cannot grow, PUSH-KEY throws with OBJECT-DEPTH one too deep and PARKED one frame too long for the rest of that failed build. The code before the per-depth OBJECT had the same order. Acceptance: no throw between PARK and its UNPARK leaves the depth stepped; say whether a failed build's later steps read OBJECT or PARKED at all, and if none does, close with that fact instead of a change. Files: src/habu/linker.f. Verify: by reading; engine build cold and warm byte-equal if code changes. Depends: none. Ownership: carl. Claim: unassigned.
