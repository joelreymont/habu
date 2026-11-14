---
title: Bind a locals group inside a quotation
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T07:52:05.977092+02:00"
---

Problem: a quotation cannot bind a {: … :} group of its own: src/core/checker.f LOC-BEGIN (:18137) refuses one inside a quotation (E-BAD-LOCAL-SHAPE), so a quotation body that needs a named value becomes a named word (docs/forth.md 'Words & factoring'). No site in Habu, Tender, Zen or Maki is known to wait on it. Acceptance: [: {: x:n :} x x * ;] loads and runs on every T0 target; E-BAD-LOCAL-SHAPE keeps only the dead-path case. Design: the quotation's T0 unit takes frame slots as a word does. Depends: a quotation's read of a local on every T0 target; an x86-64 build refuses it today (docs/forth.md:1682-1683). Claim: unassigned. Consumers: none meets this today.
