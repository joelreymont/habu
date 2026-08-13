---
title: Fold a named constant to its value
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T02:03:05.417500+02:00"
---

THE pool class's real fix (pricing merged a89f786b, four-verdict experiment on 4055b7c7's archived leaf): a body naming a constant/create'd word today emits a CALL to a record-less callee that bars the whole register pool - E-A64RA-POOL for ~131 of 149 census rows; the identical body with the value spelled as a digit compiles. Fold the named word to its value at resolution (elaborate.f RESOLVE-STEP resolves every unmodelled name as CALLABLE; HIR-WORD:DECLARE-FIXED - the fold path - is reachable only via NMIGRATE:DEFINE-DATA today). NAMED BLOCKER, design-first: the constant-vs-zero-input-colon-word predicate WITHOUT executing (NDICT:FIXED-VALUE runs the word; no dictionary kind flag exists). Candidate design: the DEFINER records its kind at definition time - constant/create stamp a kind the dictionary carries, the XTCELL-rows-for-defer precedent (layout.f SNAP-RELOC) is the shape; probe what the record format allows before minting. Checkpoint with the predicate design before implementing. ALSO wanted in the same lane: a diagnostic naming the CROSSED CALLEE at the POOL refusal (turns the 131/18 text estimate into a measurement). First consumer: 131 census rows. Files: src/habu/habu1.f/habu2.f (definer kind), src/compiler/native/{elaborate,hir-word}.f. Depends: none.
