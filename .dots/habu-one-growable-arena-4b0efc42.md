---
title: One growable arena array for the grow-on-demand buffers
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:18:47.685640+01:00"
---

Problem: the map-on-first-use, double-until-it-fits, ARENA-BYTES-GROW pattern is still written out in checker.f, include.f and rejection.f ROOM-GROW, beside src/core/arena-pages.f ARENA-ROOM, which the declaration help, label and TDA buffers share. Acceptance: each of those buffers calls ARENA-ROOM, with no change of behaviour. Depends: none. Claim: unassigned.
