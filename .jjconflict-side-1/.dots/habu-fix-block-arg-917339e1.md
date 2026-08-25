---
title: Fix block-argument aliasing on abandon
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T17:48:27.762138+02:00"
---

Full context: DEFECT found by the Rocq structure proof and CONFIRMED through public checked words only. src/compiler/ir/fun.f lines 98-101 claim two blocks cannot share an argument value at all. That direction holds, but its DUAL is false: a (block, argument position) pair does not identify one value. IR-FUN:ADD-BLOCK-ARG names the block as the ordinal the NEXT block will receive, so this sequence — BEGIN-BLOCK, ADD-BLOCK-ARG, ABANDON-BLOCK, BEGIN-BLOCK, ADD-BLOCK-ARG, terminator, END-BLOCK, END-FUN — all succeeds, and afterwards value 0 and value 1 BOTH report VALUE-BLOCK@ 0 and VALUE-ARG@ 0 while block 0's argument window points at value 1. ARGS-CK checks the window's elements and never asks whether another value also claims the pair. The file header acknowledges stranded argument rows for a REJECTED END-BLOCK and points at builder ABORT; ABANDON-BLOCK is a normal non-error path with the same effect and carries no such note. Fix by one of: make ABANDON-BLOCK a hard error when arguments were minted; mint argument values only at END-BLOCK; or have the freeze verifier require every argument-kind value row to sit in exactly one block's argument window (theorem arg_ownership_forces_injectivity). Add a negative regression from the probe above and correct the header claim.
