---
title: "A stale cell passes the cast's linear and class walks unread"
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:32:50.154398+02:00"
---

Problem: CAST-MAY-LINEAR? and CAST-INTRO? (src/core/checker.f) have no T-STALE branch, so a stale cell's inner type answers false in both; NLINEAR-SPREAD* beside them reads through STALE>INNER. Unknown: whether a T-STALE term reaches STORED-TERM-HIT (a cast's declared rows hold none). Acceptance: both walks read a stale cell's inner type as NLINEAR-SPREAD* does; a program that binds a cast's stored variable to a stale cell holding a linear value is refused E-CAST-LINEAR by a case in test/compiler/stored-type-param.f that loads before the fix, or the report shows no term reaches there and the branches stand as fail-closed code with a comment-free edit. Depends: none. Claim: unassigned.
