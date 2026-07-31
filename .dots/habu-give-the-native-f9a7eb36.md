---
title: Give the native back end one entry point over spills
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T20:09:13.990589+02:00"
---

Since src/compiler/native/spill.f landed, a caller that wants bytes has to drive four stages by hand and in the right order: allocate the selected module, ask A64RA:SPILLS, and if it answers more than zero build a second A64IR builder, bind A64RA, A64RAV and A64EMIT to it, call A64SPILL:REWRITE, allocate the result and accept THAT. test/compiler/native-emit.f and test/compiler/native-regalloc.f each spell it out, and test/compiler/native-chain-fixture.f - the file that exists so two callers drive the chain identically - still only knows the no-spill route (NFIX:FINISH allocates, accepts and emits). A caller that forgets the spill branch gets E-A64RAV-OVERLAP from a perfectly good program, which is a confusing way to learn that a step is missing. The fix is one word in the chain fixture (and later in the real driver) that takes a frozen machine module, a routine contract and the source text, and answers the module that was emitted - the original when nothing spilled, the lowered one when something did. Owners: NFIX, A64SPILL. Depends on habu-lower-spills-and-ef14a0dd.
