---
title: "Rollback invariant: no cross-scope family record mutation"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T01:43:10.163937+02:00"
---

Forward risk from TFAM 3 destruction review: TFAM rollback is counter-only (TFAM-ROLLBACK-SAVE/RESTORE in src/core/type-family.f restore TFAM-N/SUMV-N/PF-N/LAY-N and pool ends). In-place field mutators (TFAM-SCHEMA-ROOT!, TFAM-VAR-RANGE!, TFAM-LAYOUT!, TFAM-SPAN!, SV.CTOR-* back-patch) on a PRE-EXISTING record inside a scope are NOT undone by counter restore. Safe today (records are created+mutated in one scope), but items 6/8/14/15 introduce forward-declared/recursive families whose layout/schema resolve in a LATER scope - the first cross-scope in-place mutation silently survives rollback. Before any item lets a later scope mutate an earlier scope's family record: either assert the invariant fail-closed (die on cross-scope record mutation, comparing record id against the frame's saved TFAM-N) or add field-level undo entries to the rollback frame. docs/type-families.md section 21.1 codifies counter-restore; update it with whichever mechanism lands. Depends: TFAM 6 (first consumer at risk).
