---
title: One growable arena array for the grow-on-demand buffers
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:18:47.685640+01:00"
---

Problem: the map-on-first-use, double-until-it-fits, ARENA-BYTES-GROW pattern is written out per buffer (src/core/sumtype.f TDA-ROOM, structure-decl.f HELP-ROOM, checker.f, include.f, rejection.f ROOM-GROW). Acceptance: one word set holds the pattern and each buffer calls it, with no change of behaviour. Depends: none. Claim: unassigned.
