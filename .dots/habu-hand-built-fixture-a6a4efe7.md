---
title: Hand-built fixture for the branch-chain counter
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T15:14:50.827031+02:00"
---

Three columns lost their non-zero path when the branch-chain collapse landed in src/compiler/native/emit.f (ORDER-BLOCKS: a block that emits nothing before its terminator is branched past and dropped), and all of them need a routine assembled by hand rather than one a code generator happens to emit. SEARCH FIRST, THEN BUILD: every shape below was measured absent across all 54 migrated corpus rows AND across the engine's own compilations of the same bodies, so there is no friendlier row to go looking for.

(1) tools/codegen-branch-inventory.f CHAINS. A chain is a branch whose target instruction is itself an unconditional branch inside the same routine. A block whose FIRST instruction is a branch has nothing before its terminator, so it emits nothing before it, so every branch naming it is redirected to the far end and the block is not laid out at all. A chain therefore cannot survive this emitter by construction. test/compiler/codegen-branch-inventory.f now asserts only zeros in that column, which a counter that always returned zero would also pass.

(2) The same file's IDLE column, for the same reason. An idle branch is one whose target is the instruction already after it, and it used to happen when a ZERO-LENGTH block was laid between a block and its successor - FALL-THRU? asks about positions, so the two were adjacent in bytes and two positions apart. A zero-length block is exactly a pass-through block, so the collapse removed the cause and no branch jumps over nothing any more.

(3) tools/codegen-tail-probe.f TAIL-BRANCH?, whose FALSE case was a loop whose recorded body ended on its unconditional back edge - the shape that made a predicate reading only the opcode report an ordinary loop as a tail call. Counted loops now end their body on a b.cc, because the latch that carried the unconditional back edge is one of the blocks the collapse drops. test/compiler/codegen-tail-probe.f records that and asserts the new truth; its TRUE case (NTP-FIXTURE:TAILED-N) is unaffected, so the predicate is pinned in one direction only.

Fix: assemble the bodies with src/arch/arm64/asm.f and publish them with dictionary records the readers accept - NTAILPROBE:CODE-BYTES reads a recorded length that EXCLUDES the trailing return, and a name nothing published is refused as E-CODEGEN-COMPARE-SUBJECT. Two bodies cover all three: `b +2 ; b +1 ; ret` gives CHAINS 1, HOPS 2 and IDLE 1; a body whose last instruction is a backward `b` to its own first instruction gives TAIL-BRANCH? false with an unconditional last body instruction. Acceptance: each column asserts a non-zero or true answer on code no code generator produced, and the mutations that used to catch them - removing the NBR:B? test in HOP?, reading the extent as body-plus-one, reading TAIL-BRANCH? off the opcode alone - turn the suites red again. Depends: none. Ownership: test/compiler/codegen-branch-inventory.f, test/compiler/codegen-tail-probe.f. Claim: unassigned.
