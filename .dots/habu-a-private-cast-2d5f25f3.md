---
title: "A private cast's row variable yields any type"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T22:23:08.869386+02:00"
---

Problem: CAST-INTRO? (src/core/checker.f:8493) and CAST-MAY-LINEAR? (:8529) answer false on a row variable (ISROW IF RES-FALSE, :8500, :8536), so a cast in a private section yields any type through a row variable. `CAST: Q ( [ -- n ] -- [ -- S ] )` makes [: FIVE ;] Q execute a width<n> and prints 5 (~/.cache/tmp/carl-vwidth/rv/probes/cast-row-forge.f); the same row forges a pointer, an xt or a linear token. Input row variables forge too: `CAST: Q ( [ R -- S ] -- [ n -- ] )` prints 7 (~/.cache/tmp/carl-vwidth/rv3/p/rowvar-in2.f); `( [ R -- S ] -- [ S -- R ] )` prints 7 then E-UNDERFLOW (rowvar-swap.f there). Acceptance: each row variable of a cast sits at the same place on both sides, else E-CAST-CLASS with a message naming the row; the three probes are refused, each by a negative case in test/cast-negative-suite.f that loads before the fix; casts whose rows pass through the same row variable still load. Depends: the CAST: rows design of habu-a-type-param-679d7eed, which may fold this fix into its own. Claim: unassigned, owner carl.
