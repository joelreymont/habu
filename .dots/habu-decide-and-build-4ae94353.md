---
title: Decide and build how loop shape reaches an IR pass
status: open
priority: 2
issue-type: task
created-at: "2026-08-06T20:15:13.330024+02:00"
---

THE actual blocker for vectorizing the byte loops - not registers, which turned out to be largely built already.

Verified: no loop structure survives into the IR. src/compiler/native/elaborate.f:2248-2265 DO-OPEN-DO emits only HIR-OPCODE:SUB and a TERM-BRZ; :2280-2303 DO-CLOSE-LOOP emits ADD, a literal 1, LT, TERM-BRZ and a back TERM-BR-H, then CS-POP. The counted loop's start and limit live in CS-IDX/CS-LIM/CS-HEAD, which are the elaborator's own COMPILE-TIME control stack, and they are discarded. Downstream: no HIR opcode (hir.f:252-296) and no A64IR opcode (a64ir.f:349-410) is a loop form; the A64IR attribute keys (a64ir.f:849-913) are exactly imm/shift/slot/frame/dslot/dbytes/dback/entry/cond; and grep for trip/induction across src/compiler/ finds only prose.

And blocks have NO attribute mechanism at all. src/compiler/ir/fun.f:233-242 is a fixed nine-cell row with no attribute window. Functions have one (fun.f:223-224) and operations have one (src/compiler/ir/op.f:198), with IR-FUN:ADD-FUN-ATTR and IR-OP:ADD-ATTR - but there is no ADD-BLOCK-ATTR anywhere in the tree.

The only surviving loop fact is derived, not recorded: a back edge is an edge to a LOWER block ordinal, because the elaborator numbers a latch above its header (select.f:2104-2111 relies on this for region acyclicity).

DECIDE between, and say why in the leaf before building:
(a) recognise in elaborate.f, where CS-IDX/CS-LIM/CS-HEAD are in hand for free - cheapest, but the elaborator emits only HIR, would need vector HIR ops, and sees no aliasing or effect information;
(b) preserve the shape into the module - a schema-declared attribute on the header terminator, or a marker operation, carrying (index value, limit value, latch block) - then recognise in a pass. This is the structurally honest route given the chain's own doctrine that a fact must be in the module or it cannot be checked (a64ir.f:221-227, regalloc.f:121-127), and it is what the block-attribute vacuum currently forbids. Operations already have attributes, so a marker OPERATION may need no new substrate at all - probe that before minting a block attribute window.
(c) re-derive from bare brz-to-lower-ordinal CFGs - considerably larger, and the option to justify against, not default to.

Found by agent neon while scoping habu-vectorize-the-byte-a0da35a7.
