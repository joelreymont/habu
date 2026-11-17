---
title: "Encode bearer's form values with URI:FORM-ENCODE"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T08:00:01.873547+01:00"
---

Problem: src/lib/net/bearer.f UNRESERVED?, HEX-DIGIT and ENCODED+ (:615-629) hand-roll a second form encoder beside URI:FORM-ENCODE, and differ from it on `~` (kept) and space (%20, where a web form writes +). Acceptance: PAIR+ (:631) encodes each value with URI:FORM-ENCODE; UNRESERVED?, HEX-DIGIT and ENCODED+ are deleted; the bearer tests pass with the request body the shared encoder writes. Depends: none. Claim: unassigned.
