---
title: Spill for real instead of refusing
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.396651+02:00"
---

Close the declared CALL-PRESSURE gap (E-A64RA-SPILL at 7 live values across a real call) and its class: live-range splitting at call boundaries (values dead across the call keep registers; values live across it get split ranges with spill/reload at the boundary the residency machinery already prices), plus rematerialization of constants and cheap pure ops instead of stack traffic. The allocator's hull intervals stay; the splitter runs when the pool check would refuse today. Verifier re-derives every spill decision (a reload of a value whose cell is stale, a spill nothing reloads — both refusals). Acceptance: the corpus4 gap declaration comes OFF (the coverage check will demand it), L7-class shapes compile and win against the engine, no existing row regresses.
