---
title: Fix trailing omitted &key binding corruption
status: closed
priority: 1
issue-type: task
created-at: "\"2026-03-07T21:52:04.870729+01:00\""
closed-at: "2026-03-07T22:20:10.149392+01:00"
close-reason: done (fixed generic &key trailing-omitted binding corruption by reserving keyword temp slots in lambda layout and binding &key supplied-p vars after all key param names; added integration regression for omitted trailing key with supplied-p)
---

Generic CL runtime bug surfaced while implementing open/append: trailing omitted &key params corrupt value and supplied-p binding when an earlier keyword is supplied but later ones are omitted. Repro: lib/stdlib.habu open wrapper and pure CL probe  then  returns corrupted bindings, while both keywords supplied works. Investigate src/interp/vm.zig doCall keyword setup / default handling and fix generically; add regression tests in src/tests/integration.zig.
