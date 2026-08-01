---
title: Re-derive the multi-block allocation in the validator
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T11:06:40.674335+02:00"
---

src/compiler/native/regalloc-verify.f re-derives live ranges over one straight-line block and has to re-derive them over the whole control-flow graph once regalloc.f does. It must compute the block order, the liveness and the intervals INDEPENDENTLY - never read A64RA's tables - and add the edge clause the single-block validator has no need for: for every terminator that hands a successor its block arguments, the register of operand i must be the register of the successor's argument i. That clause is what makes a fixup to the wrong block, a swapped successor pair and a block-argument register mismatch die at acceptance rather than at execution. Depends on the multi-block allocator.
