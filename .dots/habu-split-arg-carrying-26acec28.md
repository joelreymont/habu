---
title: Split argument-carrying edges in the native selector
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T11:06:27.608899+02:00"
---

src/compiler/native/select.f lowers hir.br to a64.b whose operands ARE the successor's block arguments, so the register allocator has to put each operand in the register the destination's argument was given. With MAX2 (2dup < if swap then drop) the two arms hand the join (a,b) and (b,a), so coalescing the operand with the argument merges two interfering values into one class and no allocation exists. The fix is ordinary critical-edge splitting at selection: emit one a64.mov per argument in front of the branch and pass the mov results, so every class member is a fresh short-lived value that cannot interfere with another member. Costs one register per live argument and no peephole elides an identity copy - that is a separate optimisation dot. Blocks the multi-block allocator.
