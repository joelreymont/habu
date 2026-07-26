---
title: "Checker: ground MATCH diagnostic and parametric locals"
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T11:00:15.083737+02:00"
---

Two related front-end gaps found 2026-07-26 by the A1 migration pathfinder, both proven PRE-EXISTING (identical behavior with the legacy SUMTYPE declaration restored). (1) A single word that constructs a variant and then MATCHes it is refused with E-UNDEFINED naming the family token (in p3-b: undefined word id-result), which reads as a missing word rather than what it is: the checker refuses a MATCH over a non-ground scrutinee. Minimal reproducer recorded in the A1 lane report: R-OK MATCH id-result ok OF ... inside one definition in package JOURNAL; MATCH over a concretely instantiated argument certifies, MATCH over a generic id-result<a> argument is refused the same misleading way. Fix the diagnostic to name the real cause and document the ground-scrutinee rule; if the refusal itself is over-conservative for constructed-in-body scrutinees, treat that as a capability decision with its own fixture. (2) A typed local cannot carry a parametric family type: {: r:id-result<CAD-KIND:audit-event-id> :} fails with unknown type in signature even though the same type is legal in a stack effect. Either support parametric types in local signatures or reject with a message that says locals cannot carry parametric families. Acceptance: negative regressions pinning today's refusals with the improved diagnostics, positive fixtures for whatever becomes legal, checker suite green. Owner: src/core/checker.f MATCH lowering and the locals signature parser. Dependencies: none.
