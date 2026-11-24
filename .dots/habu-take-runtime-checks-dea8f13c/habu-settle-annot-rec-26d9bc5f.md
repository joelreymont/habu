---
title: "Settle ANNOT-REC-SYM's fall-through"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T19:12:11.787193+01:00"
closed-at: "2026-11-24T15:46:28.355442+01:00"
---

Problem: src/core/checker.f ANNOT-REC-SYM ends its search loop with an internal die 'wordlist has no package'; whether any input reaches the fall-through is unread, and deleting the die needs the value the loop answers there. Acceptance: either the loop is shown to always find a package and is written with no fall-through, or the case is a rejection naming the file and the fix; no internal die remains. Depends: none. Claim: unassigned.
