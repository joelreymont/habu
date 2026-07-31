---
title: Prove the spill rewrite preserves the program
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T20:08:40.777474+02:00"
---

src/compiler/native/spill.f builds a second machine module in which the allocator's spill decisions are store and load operations, and nothing checks that the new module computes what the old one did. src/compiler/native/regalloc-verify.f cannot: it is handed one module, and from one module alone 'this load reads the slot its value was stored to' is only 'this load reads the slot the last store wrote' - the identity of the value a reload was MEANT to carry is a fact about the module the rewrite came from. Today the two are tied together by execution tests only (test/compiler/native-emit.f runs a spilled five-literal chain and compares the sum). The check that would settle it is a lockstep walk of the two modules: every operation of the old block appears in the new one in the same order with the same opcode and attributes, only frame operations are inserted, and each operand of a copied operation is either the value the old operand mapped to or a load whose slot's last store carried that value. That is decidable from the two modules and reads nothing the allocator kept, so it belongs beside A64SPILL:REWRITE as its own acceptance step, in the shape A64RA/A64RAV already have. Owners: A64SPILL. Depends on habu-lower-spills-and-ef14a0dd.
