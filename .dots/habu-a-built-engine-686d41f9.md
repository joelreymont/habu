---
title: "A built engine's primitive rows lose the fixed-ABI and C2 flags"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T19:13:08.187929+02:00"
---

Problem: in a built engine a primitive's row is read lazily from its facts text (src/core/checker.f READ-FIRST, READ-ROW), and only three flags come back as tags (render.f REND-PE: primitive, trusted-only, top-only). PE-SPEC-ROW, which sets the others, runs only at bootstrap and for primitives the building hb lacks. A probe on a rebuilt engine shows xt!'s row with flags 1, so PE-FIXED-ABI is gone and SELECTED-PRIM-ABI? never sees it; PE-C2-INIT and PE-C2-RECORDS (C2-STOW-PRIM-KIND) are not tags either. Read by the worker of habu-checked-src-can-88e8a977 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/nullread-fix2-stop.md, Out of scope); no program that answers differently is known; not rerun by the lead. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: for each of the three flags, a program whose check or compile differs between the bootstrap engine and a built engine, or the reason none can (who reads the flag, and on which rows). Acceptance: each flag a caller's check or the compiler reads reaches a built engine's rows from the engine's own primitive table, as PE-MEMORY does (READ-MEMORY), or the flag and its readers are removed as unread. Files: src/core/checker.f, src/habu/prims.f, as the hunt confirms. Verify: the hunt's program on a built engine, or the removal's suite. Depends: habu-checked-src-can-88e8a977 (it adds READ-MEMORY, the model). Ownership: the primitive flags a built engine reads. Claim: unassigned.
