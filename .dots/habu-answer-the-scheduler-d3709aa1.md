---
title: "Answer the scheduler's unchecked arbitrary-xt catch before the cut"
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T16:46:06.598875+02:00"
---

lib/process.f:344 TRUSTED: TASK-RUN-USER ( -- n ) TASK-SELF TCB.USER-XT @ catch ; is a genuine unchecked arbitrary-xt catch in the scheduler - the xt comes from untyped TCB memory, so checked code cannot express it (E-EXEC-OPAQUE-XT). Outside the census population but the hard cut (a5aa3f1f) claims the tree: this site needs an answer - a typed task-entry contract (the correct shape), or a named tested TRUSTED boundary with its capability dot per the Habu Only rule. Found by the exceptions design probe (6ceb7667). Files: lib/process.f. Depends: sequencing with the cut, none technical.
