---
title: Occurs-check inside a stale cell
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T14:36:49.424691+02:00"
---

Problem: src/core/checker.f OCC-WALK does not walk into a T-STALE cell's type, so a variable may unify with a term that holds it under a stale cell; no program is yet known that this admits. Acceptance: a case where it admits a cyclic type, fixed in OCC-WALK, or OCC-WALK's comment states why a stale cell cannot hold the variable. Depends: none. Claim: unassigned.
