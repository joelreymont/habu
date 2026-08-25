---
title: Reject operations outside every block at freeze
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T17:48:27.747349+02:00"
---

Full context: DEFECT found by the Rocq structure proof and CONFIRMED by running the shipped words. src/compiler/ir/fun.f lines 78-80 claim an operation appended outside a block leaves a gap the tiling check rejects. That is true only BETWEEN blocks: the next BEGIN-BLOCK captures a start that no longer equals the previous block's end and END-BLOCK's STEP-CK refuses. An operation appended after the LAST block is past every window, so no STEP-CK ever examines it, and an operation row carries no parent field by design, so no second record can disagree. Probe: one function, one block covering operation 0, END-FUN succeeds, then one more operation appended with no block open — IR-OP:OPS reads 2, block 0's window covers 1 operation, nothing throws. The same argument holds one level up for blocks left behind by an abandoned function versus the block count. Closing check (proved necessary as well as sufficient by theorem coverage_is_necessary in formal/Common/Structure.v): the last block's operation-window end must equal IR-OP:OPS, and the last function's block-window end must equal IR-FUN:BLOCKS. Neither file performs it. Implement in the freeze verifier with a negative regression that appends an operation after the last block and requires a named reject. Also correct the false claim in the fun.f header.
