---
title: A generic word that compares or subtracts its pointers refuses a record pointer
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T21:40:57.120048+02:00"
---

Problem (stored-type kind lane, 2026-11-11, workspace carl-vkind, probes by its worker): `: EQ ( ptr t ptr t -- bool ) = ;` called with `ptr turn` (turn a two-cell STRUCTURE) is refused E-MISMATCH (`at 'EQ' expected: ptr a ptr a actual: ptr turn ptr turn`), and so is a body that subtracts its pointers (`NULL-PTR - swap NULL-PTR - =`). Cause: primitive rows keep TVK-ANY (the kind lane's rule 2 marks only checked colon definitions): the pointer row of `=` (src/habu/prims.f:487 PE-PTR-A) and of `-`; a call that pairs a STORED variable with a callee's ANY variable leaves ANY (checker.f STORED-LIFT), so EQ is published ANY and LAYOUT-BLOCK? refuses the layout. Acceptance: a primitive or keyword pointer row that never reads or steps its pointee carries STORED (a census of src/habu/prims.f and KEYWORD-ROWS names each such row and what the primitive does with the pointee); EQ and a pointer difference load and run with `ptr turn`; a row that reads, writes or steps keeps its kind. Files: src/habu/prims.f, src/core/checker.f, test/compiler/stored-type-param.f. Verify: the two cases added to stored-type-param.f as admitted; the full suite. Depends: none. Waiting: not a lane until a consumer needs it. Ownership: the files above. Claim: unassigned.
