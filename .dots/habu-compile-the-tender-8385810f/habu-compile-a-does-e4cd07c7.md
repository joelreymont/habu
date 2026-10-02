---
title: Compile a does>-only parent through NCOMP
status: closed
priority: 2
issue-type: task
created-at: "2026-10-02T14:18:50.835141+03:00"
closed-at: "2026-10-02T15:41:26.622390+03:00"
close-reason: "done: NELAB counts a does> parent's patch (order minted at entry, CALL-NEED set before the patch call); oi-does-only agrees in both loops at tier 1 (1 6 10 8), spark gen1=gen2=gen3 2955"
---

Problem: a colon definition whose body is only a does> clause fails at tier 1 in the engine's own loop. `1 set-tier : OI-PAT ( -- ) does> ( -- n ) @ 1 + ; create OI-B 5 , OI-PAT OI-B .` prints `ncomp: cannot compile OI-PAT` and exits 67 with -8550 E-NELAB-CALL (base engine 31d3, ThinkPad). The same clause after a create in the parent compiles: `: OI-MK ( n -- ) create , does> ( -- n ) @ 1 + ; 5 OI-MK OI-B OI-B .` prints 6. x86 runs only tier 1, so every does>-only parent fails there. Acceptance: the first program prints 6 at tier 1 through both the engine's loop and the Habu loop; the reduction names whether NELAB, the does> capture or the declared effect is wrong, and the fix lands at that layer; test/outer-interpret.f gains the case as one both loops agree on. Files: src/compiler/native/ (the layer the reduction names), test/outer-interpret.f. Verify: spark gen2==gen3, outer-interpret, does-clause-record, native-create-does.
