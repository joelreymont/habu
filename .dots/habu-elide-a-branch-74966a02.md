---
title: Elide a branch to the next block
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T16:19:32.719348+02:00"
---

Phase 2 follow-on to the compare-branch fusion (habu-fuse-comparisons-into-a10e7fb3). src/compiler/native/emit.f lays blocks out in the order the module records them and emits every terminator in full, so a64.b, a64.cbz and a64.cmpbr each end with an unconditional branch to a block that is very often the very next one. In the MAX2 shape the fused entry block's b.ge jumps to block one, which starts at the next instruction, and block one is a two-instruction stub whose own b goes to the join - so two of the fifteen instructions are branches to the following instruction. Wanted: a layout that knows a successor is the next block and leaves the branch out, with the instruction count per operation then depending on the LAYOUT rather than only on the form - which is the part that needs designing, because emit.f's INSNS-OF is read twice, once to place the blocks and once to emit them, and both readings have to agree. Acceptance is the same as every Phase 2 pass: the full 11-row codegen-compare table re-run, measurably fewer bytes with identical results, reverted if it moves nothing.
