---
title: Fold the crossing pair a splice can leave
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T20:00:39.490923+02:00"
---

src/compiler/native/elaborate.f DO-INLINE now crosses a copied body's argument positions to cells, because that is what the record was elaborated against (a routine reads its arguments out of data-stack slots, so its entry block takes cells). When the copied body's FIRST use of such an argument is a float operation, COERCE1 crosses the same value straight back, and the pair is two FMOVs that cancel: hir.real>bits followed by hir.bits>real over one value computes nothing. Nothing in the third or fourth corpus hits it today - every recorded body's arguments are used as cells - so no measurement moves, but a float callee small enough to record will pay two instructions per argument as soon as one exists. What to build: a fold that removes a bits>real whose only operand is the result of a real>bits, and the reverse, read off the opcode pair rather than off where the values came from, so a crossing placed by a return, a call, an edge or a splice is folded by one rule. Owner: NELAB or A64SEL. Test: a recorded callee of arity ( r -- r ) copied into a caller that hands it a computed double, whose emission holds no FMOV between the files at all.
