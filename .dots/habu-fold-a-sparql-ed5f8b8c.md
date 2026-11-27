---
title: Fold a SPARQL path before a disconnected join
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T19:45:15.901897+01:00"
---

Problem: emit.f RUN joins every joined item of a run before LATE folds its walks, so two patterns sharing no variable around a path form a cross product the walk seeds from (survey: 1,464 x 1,464 rows, 1.35 s against 86 ms folded first). Acceptance: a walk whose seed end is bound is folded before a join that shares no variable; same rows; the survey query's walk step reads no cross product. Depends: none. Claim: unassigned.
