---
title: Narrow what a call site saves
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T01:23:19.340293+02:00"
---

Measured: tools/codegen-compare-corpus3.f's T-SGD! is 340 bytes against the old emitter's 448 and is SLOWER - 52.06 ns against 45.00 ns in the third table. Its loop body is three calls (two T-GET and one T-SET) and its four locals, two loop counters and accumulator are live across all of them, so src/compiler/native/select.f CALL-SAVE writes seven values into data-stack slots and CALL-RESTORE reads them back, three times per turn. The discipline is correct - nothing in a Habu word's convention is callee-saved, and src/compiler/native/hir.f gives the argument - but it is stated over EVERY live value rather than over the ones the callee can really reach. What would narrow it: a callee whose own destroyed set is known (a chain-compiled callee publishes one) lets the site save only what that set intersects, and a leaf callee that touches no memory needs no order threaded either. It is a measurement question against the committed third table, not a guess: the row's direction is pinned in tools/codegen-compare-test.f. Owners: A64SEL, NABI.
