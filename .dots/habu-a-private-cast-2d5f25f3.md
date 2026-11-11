---
title: "A private cast's row variable yields any type"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T22:23:08.869386+02:00"
---

Problem: CAST-INTRO? (src/core/checker.f:3923) and CAST-MAY-LINEAR? (:3965) answer false on a row variable (ISROW IF RES-FALSE, :3930, :3972), so a cast in a private section yields any type through a row variable. `CAST: Q ( [ -- n ] -- [ -- S ] )` (~/.cache/tmp/carl-vwidth/rv/probes/cast-row-forge.f) and `( [ R -- S ] -- [ S -- R ] )` (rowvar-swap.f in ~/.cache/tmp/carl-vwidth/rv3/p/) are refused by E-CAST-LINEAR only because their output quotation's rows end in different tails; a cast's input reads such a quotation as any quotation, so `CAST: Q ( [ R -- S ] -- [ n -- ] )` prints 7 (rowvar-in2.f there). Acceptance: each row variable of a cast sits at the same place on both sides, else E-CAST-CLASS with a message naming the row; the three probes are refused, each by a negative case in test/cast-negative-suite.f that loads before the fix; casts whose rows pass through the same row variable still load. Depends: none. Claim: unassigned, owner carl.
