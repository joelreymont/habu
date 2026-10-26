---
title: Compile CAST of a float to its bits at tier 1
status: open
priority: 3
issue-type: task
created-at: "2026-10-26T11:48:23.557953+02:00"
---

Tier 1 defect, dotted only (no new tier 1 work while the tier experiment runs): 1 set-tier  CAST: FB ( r -- n )  : P ( -- n ) 1.0 2.0 f+ FB 1+ ;  fails with 'ncomp: cannot compile P' and uncaught throw -8580; tier 0 and the interpreter give 4613937818241073153.
