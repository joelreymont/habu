---
title: Choose between competing coalesce candidates
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T17:42:04.715856+02:00"
---

Follow-on to habu-coalesce-the-edge-5ac08118. src/compiler/native/regalloc.f step five merges a copy's two ends when their classes hold no interfering pair, and it takes candidates in the module's own order - blocks then operations. When two candidates want the same class and only one can have it, module order decides, which is an accident of how the elaborator built the module rather than a decision. A pass that RANKED candidates - by loop depth, by how many copies one merge deletes, by which end is already fixed - would delete more of them. Constraints it has to keep: the merge test stays the class invariant (no two members of a class live at once), the validator keeps checking the result and not the order (regalloc-verify.f OVERLAP-CK), and it has to be paid for on the eleven-row codegen-compare table with the drift-corrected methodology or reverted. Note the pressure cost this leaf leaves behind: a merged class is held over the hull of both parts, so coalescing across a gap can turn a routine that fitted into one that does not, and this path refuses (E-A64RA-SPILL) rather than spilling - a ranking pass should measure that too.
