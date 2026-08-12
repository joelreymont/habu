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

THE GATE IS SATISFIED (2026-08-12): both prior roads are done or dead -
split-call-crossed landed (f0983cf2, conditional threading), remat
constants landed (9adfc89d), and remat LOADS is refused on the IR's own
facts (1faad3e1's leaf carries the refutation: both load forms declare a
memory READ answering a new order, ALDR with UNRESTRICTED aliasing, so a
re-emitted load IS the middle-block frame access KEEP? exists to refuse).
PRESSURE-LOOP - the judge's LAST refused row - demands exactly this
capability, and per the user's no-refusals ruling it is cut-blocking.
This dot is now the owner of the final corpus refusal. Design-first: the
leaf's own design question stands (the frame's memory order must MERGE at
a join - a token block argument on every multi-predecessor block and a
refusal for a two-way-branch-fed merge, which carries no operands); the
alternative road is a loop-invariant-load HOIST with a real dependence
proof (the missing fact: "the alias class this load reads is unwritten
between definition and re-emission point" - derivable from the token
chain, derived by no pass today). Probe BOTH before building either.
