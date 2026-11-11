---
title: "A private cast's row variable yields any type"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T22:23:08.869386+02:00"
---

Problem: src/core/checker.f CAST-INTRO? (:25104 on master) and CAST-MAY-LINEAR? (:25146) answer false on a row variable (ISROW IF RES-FALSE), so CAST: Q ( [ -- n ] -- [ -- S ] ) in a private section yields any type on its output row: probe ~/.cache/tmp/carl-vwidth/rv/probes/cast-row-forge.f makes [: FIVE ;] Q execute a width<n> and prints 5; the same row forges a pointer, an xt or a linear token on the installed engine. Input row variables forge too (lane 2 re-review, probes ~/.cache/tmp/carl-vwidth/rv3/p/rowvar-in2.f: CAST: Q ( [ R -- S ] -- [ n -- ] ) prints 7 with no row variable in the output; rowvar-swap.f: ( [ R -- S ] -- [ S -- R ] ) prints 7 then E-UNDERFLOW); CAST-MAY-LINEAR? shares the cause. Acceptance: each row variable of a cast sits at the same place on both sides, else E-CAST-CLASS with a message naming the row; cast-row-forge.f, rowvar-in2.f and rowvar-swap.f are refused; casts whose rows pass through the same row variable still load. Files: src/core/checker.f, test/cast-negative-suite.f. Verify: the three probes; a negative case per probe that loads on the engine before the fix; full suite. Depends: the CAST: rows design (Plan run on .out/briefs/design-cast-stored-linear.md, lane 3 of habu-a-type-param-679d7eed), which may fold this fix into its own. Ownership: carl. Claim: unassigned.
