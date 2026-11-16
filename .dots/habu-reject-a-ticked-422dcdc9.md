---
title: "Reject a ticked parsing word's operand as the load does"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T17:22:57.539756+01:00"
---

Problem: with `: p ( -- ) parse-name 2drop ;` then `' p execute nosuchword` then `nosuchtwo` on the next line, the load's first record is nosuchtwo at 3:1 (p's run took nosuchword as its operand), while the verifier rejects nosuchword at 2:13: an execute of a ticked parsing word is not followed as a parsing call. Acceptance: the three readers give the load's first record; a test pins it. Depends: none. Claim: unassigned.
