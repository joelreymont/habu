---
title: "User source declares a PRIM: row"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T20:13:39.562547+02:00"
---

Problem: 'PRIM: FOOB PE-PTR-N PE-OUT PRIM;' in user source is accepted, under --load and under tools/check.f --json-errors: user source gives an engine word's row to a name of its choice, which mints any type without a check. Found by the storage-window design (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/storage-window-design.md, section 0 item 2); TRUSTED: in user source forges the same way and has its dots (habu-sweep-trusted-out-41e973ce, habu-delete-the-trusted-42b30edd). SOURCE-UNIT:USE is likewise reachable from user source (refused there only for the probe's quotation row; bare USE-INTERPRETER is E-UNDEFINED): not carried further. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: PRIM: in user source is refused; the engine's own PRIM: rows still load; SOURCE-UNIT:USE from user source is refused or shown harmless with the run that shows it. Files: src/core/checker.f (PRIM:), src/habu/prims.f, the source-unit package. Verify: both programs as refused cases, failing before. Depends: none. Ownership: who may declare a primitive row. Claim: unassigned.
