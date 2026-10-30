---
title: Name the NOMINAL-REC-SYM sentinels in checker.f
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.255546+02:00"
---

Problem: src/core/checker.f NOMINAL-REC-SYM compares against the literals -2 and -1 (a retired record wid and a missing one). Acceptance: named constants stating the meaning, used at every compare. Files: src/core/checker.f. Verify: the literals are gone from the compare sites; checker rows green. Depends: none. Ownership: checker.f NOMINAL-REC-SYM. Claim: unassigned.
