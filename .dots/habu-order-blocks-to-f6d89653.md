---
title: Order blocks to make more fall-throughs
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T16:54:33.083156+02:00"
---

Follow-on to habu-elide-a-branch-74966a02. src/compiler/native/emit.f lays blocks out in the order the module records them and now leaves out a terminator's trailing unconditional branch when its target is the block laid out next. Which branches that deletes is therefore whatever the elaborator's build order happened to make it. A layout pass that CHOSE the order - putting each block's most likely successor next, and the join of an if/then after the arm that reaches it directly - would delete more of them, and would also decide which arm of a two-way branch is the taken one. It is its own pass with its own measurement: it must not change what any routine computes, and it has to be paid for on the eleven-row codegen-compare table (bytes and drift-corrected cost) or reverted. Note the constraint the elision leaves behind: the layout order is now load-bearing, so a reordering pass and the emitter's FALL-THRU? rule have to be read together, and test/compiler/native-chain.f pins exact block starts and branch displacements that a reordering will move.
