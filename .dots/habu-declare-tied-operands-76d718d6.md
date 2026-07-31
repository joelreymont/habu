---
title: Declare tied operands in the operation schema
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-31T00:29:10.155135+02:00\""
---

The move-wide overwrite Movk keeps the bits of its destination that it does not write, so in the encoding its source and its destination are one register field. src/compiler/native/regalloc.f has to put the kept value and the result in the same physical register, and today it learns which opcode is the overwrite by asking the dialect for A64IR-OPCODE:MOVK through A64RA:BIND-DIALECT and keeping the symbol. That works and restates no spelling, but the tie is a property of the instruction form and belongs in the form's own schema, the way LLVM records a tied operand in its instruction descriptor. Add a tie declaration to IR-SCHEMA (which result is tied to which operand), have src/compiler/native/a64ir.f declare it on a64.movk, and change the allocator and its validator to read the constraint instead of knowing a name. Acceptance: a dialect that declares a tie on a new form is honoured with no allocator change; an operation whose schema declares a tie and whose assignment breaks it is refused by the validator; the allocator no longer holds any opcode identity for the tie. Owners: IR-SCHEMA, A64IR, A64RA, A64RAV.

Claim: agent=tielane workspace=.jj-ws/habu-declare-tied-operands-76d718d6
