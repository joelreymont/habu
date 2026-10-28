---
title: Decide whether a pointer to a linear type owns it
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:38:54.725606+02:00"
---

Problem: LIN-TYPE-COUNT* counts a T-PTR as 0 linear cells (src/core/checker.f:3644), so ': PTY ( ptr F7C:toky -- ) drop ;' with a DEFLINEAR toky certifies (probe /tmp/tim-dots/f7c.f, rc 0) while ': PTZ ( F7C:toky -- ) drop ;' is refused; reading the token through it is refused (f7d.f, E-REJECTED at @). docs/forth.md:1431-1438 allows 'a checked ptr PKG:type' as a handle and says a typed variable or buffer cannot store a linear type (:2333), leaving open what a ptr to one is. Acceptance: one stated rule - either a ptr to a linear type is linear (drop and dup refused, as for the token) or it is a borrow that owns nothing, with docs/forth.md saying which - and a refusal row or a positive row for f7c.f. Files: src/core/checker.f, docs/forth.md, test/linear-suite.f. Verify: the probes. Depends: none. Ownership: type system. Claim: unassigned.
