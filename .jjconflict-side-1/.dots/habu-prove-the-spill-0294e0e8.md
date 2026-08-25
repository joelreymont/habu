---
title: Prove the spill rewrite preserves the program
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T20:08:40.777474+02:00"
---

src/compiler/native/spill.f builds a second machine module in which the allocator's spill decisions are store and load operations, and nothing checks that the new module computes what the old one did. src/compiler/native/regalloc-verify.f cannot: it is handed one module, and from one module alone 'this load reads the slot its value was stored to' is only 'this load reads the slot the last store wrote' - the identity of the value a reload was MEANT to carry is a fact about the module the rewrite came from. Today the two are tied together by execution tests only (test/compiler/native-emit.f runs a spilled five-literal chain and compares the sum). The check that would settle it is a lockstep walk of the two modules: every operation of the old block appears in the new one in the same order with the same opcode and attributes, only frame operations are inserted, and each operand of a copied operation is either the value the old operand mapped to or a load whose slot's last store carried that value. That is decidable from the two modules and reads nothing the allocator kept, so it belongs beside A64SPILL:REWRITE as its own acceptance step, in the shape A64RA/A64RAV already have. Owners: A64SPILL. Depends on habu-lower-spills-and-ef14a0dd.

GROOMED 2026-08-04 (dot-groom). Dangling blocker repointed. habu-lower-spills-and-ef14a0dd is
no longer in the graph: it was closed and archived by commit bc6485eb7 "Close the
spill-lowering dot". The lowering it delivered is in the tree at src/compiler/native/spill.f
(package A64SPILL), which is the rewrite this dot has to prove preserves the program, so the
dependency is satisfied and the subject exists. Nothing blocks this dot now.

SECOND CONSUMER NAMED 2026-08-12 (lane spill-close). The allocator can now take a
class's register by arranging to write its constant AGAIN in front of each read
(src/compiler/native/regalloc.f MB-REMATABLE?, spill.f EMIT-REMAT), so the
rewrite this dot has to prove now inserts three kinds of operation and not two.
The gap is the same one and the answer is the same lockstep walk: from one module
alone, "this move-wide carries the immediate the class it stands for held" is as
unprovable as "this load reads the slot its value was stored to". Each operand of
a copied operation is either the value the old operand mapped to, or a load whose
slot's last store carried that value, or a re-emission of the operation that
defined it - one more arm on the walk already described above.

WANTED PROMPTLY. Until it exists both lowerings are held by execution alone: the
chain's answers against the engine's own compilation of the same text, plus
hand-derived arithmetic (test/compiler/native-migrate.f SPILL-CASE and
REMAT-CASE). That does falsify a wrong immediate and a wrong slot, so the claim
is testable today - but it is a differential over answers, not a proof about the
rewrite, and the number of shapes it covers is the number of fixtures somebody
wrote.
