---
title: Decide whether a pointer to a linear type owns it
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:38:54.725606+02:00"
---

Problem: LIN-TYPE-COUNT* counts a T-PTR as 0 linear cells (src/core/checker.f:3644), so ': PTY ( ptr F7C:toky -- ) drop ;' with a DEFLINEAR toky certifies (probe f7c.f: 'package F7C', 'public', 'DEFLINEAR F7C:toky', the definition and ';package', one per line; on the installed hb it loads, rc 0) while ': PTZ ( F7C:toky -- ) drop ;' in its place is refused at 'drop', rc 70; reading the token through it is refused (f7d.f: ': PTR ( ptr F7C:toky -- F7C:toky ) @ ;' in its place, refused at '@', rc 70). docs/forth.md:1435-1442 allows 'a checked ptr PKG:type' as a handle and says a typed variable or buffer cannot store a linear type (:2333), leaving open what a ptr to one is. Acceptance: one stated rule - either a ptr to a linear type is linear (drop and dup refused, as for the token) or it is a borrow that owns nothing, with docs/forth.md saying which - and a refusal row or a positive row for f7c.f. Files: src/core/checker.f, docs/forth.md, test/linear-suite.f. Verify: the probes. Depends: none. Ownership: type system. Claim: unassigned.
