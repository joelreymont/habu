---
title: Call no word whose body is a bare return
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T18:15:15.724060+01:00"
---

Problem: generated code calls tiny primitives as subroutines, among them cell-view, whose body is a bare ret, data-base and NULL-PTR (hunt fficall, 2026-11-17, Maki's /tmp/tim-fficall dump.out, raw2.out); every Habu word that uses them pays a call and a return for nothing. Inlining guarded scalar stores (!) was dropped by the user (habu-optimization-73d71564) and is not asked here. Acceptance: no generated call of a word whose body is only a return, the engine's size and a build's time measured before and after. Depends: none. Claim: unassigned.
