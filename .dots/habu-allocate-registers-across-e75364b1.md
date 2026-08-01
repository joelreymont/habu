---
title: Allocate registers across blocks by real live intervals
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T11:06:40.661324+02:00"
---

src/compiler/native/regalloc.f still refuses a function of more than one block (BLOCK-OF, E-A64RA-SHAPE). The elaborator and the selector now produce multi-block A64IR, so this is the next link. Design settled while building the front half: number operations globally in module block order with one extra position per block for its arguments; compute per-block live-in/live-out by backward dataflow over the successor edges (a terminator's operands are uses in its own block, a block's arguments are defs of that block, so nothing flows across an edge by accident); give each value the hull interval [min(def, entry of every block it is live-in to), max(last use, end of every block it is live-out of)], which is contiguous for the reducible structured loops Forth control words produce; then coalesce each block argument with the values feeding it into one class with the union hull, and allocate one register per class. Refuse by name when two members of one class interfere. Depends on habu-split-arg-carrying-26acec28, which is what makes intra-class interference impossible for structured code.
