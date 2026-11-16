---
title: "Make the checker's cost linear in stack depth"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:43:01.421540+01:00"
---

Problem: checking a body of N literals costs 0.28/1.06/4.17 s for N 1000/2000/4000, and a 5000-deep body with drops 11.44 s; test/program-diagnostic-long-row.f (about 4100 literals) takes 6.57 s. The quadratic step is not yet found. Acceptance: a root cause at file:line, then check time that grows linearly with depth on the same fixtures. Depends: none. Claim: unassigned.
