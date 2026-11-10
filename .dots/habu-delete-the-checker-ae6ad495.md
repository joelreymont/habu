---
title: "Delete the checker's grid machinery: nothing reads a grid"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T00:20:19.597057+02:00"
---

Problem: with CELL-BODY removed (55bd934c) the rest of the checker's grid machinery still runs and has no reader: STORAGE-GRID, RECORD-GRID-*, RLINK.GRID, BODY-PEND, CHECKER-BODY-CLOSE and src/core/include.f's call of it, CHECKER-GRID-ATTACH, the GRID owner offsets in src/core/checker-owner-abi.f, RECORD-CONTRACT's grid cell with test/linker.f case R, and the comma-run cells. Its only reader, src/habu/aot-data-cell.f through CHECKER-STORAGE-CELLS, was deleted in 0306b422. DECISIONS.md:838-840 records Joel's choice 'Remove all of it'. This is the reopened scope of the closed dot habu-delete-the-grid-820baecd, carried until now in the text of the closed dot habu-a-build-refuses-a7935d86. Still true of comma-run grids and to go with them: the guard at checker.f:17856-17858, the comments near checker.f:29897 and :30143. Acceptance: the words above and their callers are gone, a comma is a plain store of a cell, docs/forth.md and the card no longer describe a grid; the engine reaches a fixpoint and the suite passes. Before a brief: list each word's callers on arm64 (x86-64's comma run is dot habu-x86-64-keeps-e1506c6d). Files: src/core/checker.f, src/core/include.f, src/core/checker-owner-abi.f, src/habu/habu1.f (the comma), test/linker.f, docs. Verify: chain check, full suite, the consumer measure. Depends: none. Not on the release path: no consumer meets it. Ownership: carl. Claim: unassigned.
