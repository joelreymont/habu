---
title: Spill from a block that is neither the entry nor the exit
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T20:29:42.092121+02:00"
---

src/compiler/native/regalloc.f (MB-KEEP-BLOCK) will only spill a value whose definition and every read are in the block the caller enters or the block control leaves through. The reason is the memory order the dialect's frame forms thread: it has to be read exactly once on every run, and two frame-touching blocks where one is reachable from the other are two readers of one order on one path (src/compiler/native/regalloc-verify.f ORDER-CK). The entry block dominates everything and every returning run passes through the exit block, so that pair is the one that can never have the problem. Spilling inside an arm or a loop body needs the frame's order to MERGE at a join, which means a token block argument on every block with more than one predecessor and a refusal for a merge fed by a two-way branch, which carries no operands. Until it lands, a value read inside a branch arm holds its register and the shortage is refused E-A64RA-SPILL.

Scope finding (2026-08-05, spill lane measurement): NEITHER corpus-4 refusal needs this redesign — CALL-PRESSURE closes with an around-the-loop split (habu-split-call-crossed) and PRESSURE-LOOP likely with loop-invariant rematerialization (habu-remat-loop-invariant). This dot proceeds only if a real program shape demands spill placement inside a middle block after those two land; until then it has no measured consumer.

Ownership cross-reference (2026-08-05, agent=callsplit): habu-split-call-crossed-6eda1613 owns the elaborator-side call-crossing protocol change (splitting a call-surviving local around a loop at elaboration); this dot keeps only the allocator-side middle-block frame-order redesign, which that change does not touch and does not need.
