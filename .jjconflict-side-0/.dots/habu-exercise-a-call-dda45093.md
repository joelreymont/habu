---
title: Exercise a call to another word in a routine that spills
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T10:40:38.963399+02:00"
---

src/compiler/native/spill.f copies a64.wordcall and its a64.entry attribute like every other form - the opcode has a slot in its family table and the key has a slot in its key table, so a spilling routine that calls another word rewrites correctly by construction - but nothing runs it. The path is unreachable from production today for a separate reason: src/compiler/native/migrate.f EMITTED runs allocate, accept and emit with no A64SPILL:REWRITE between them, so a migrated routine whose values do not all fit is refused by the validator rather than lowered. test/compiler/native-chain.f reaches the spill lowering through NFIX:RUN-HABU-CALL-SPILL and proves it for a self-call (NCH-RSPILL); the same shape for a call to another word needs the chain fixture to carry a callable declaration and a published callee, which is rigging that suite does not have yet. Two things to land together: give the migration entry the spill lowering, and add the chain case. Until both, a call to another word is only proved in routines whose values fit their registers.
