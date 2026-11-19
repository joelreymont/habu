---
title: Bracket every recursive term walker
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T15:17:14.308953+01:00"
---

Problem: src/core/checker.f's header (Term-walk depth, :92-96) says every recursive term walker brackets each RECURSE with TWALK-DEEPER and TWALK-SHALLOWER, so a too-deep term dies with a named diagnostic; many walkers recurse without them (about 160 RECURSE lines in checker.f carry no TWALK-DEEPER on master 758d1fdc, among them the P>REST walks near :2328-2346 and the term walk near :2563-2571). Acceptance: every recursive term walker in the checker brackets its RECURSE, and a term past the bound dies named in each (a probe per walker kind). Depends: none. Claim: unassigned.
