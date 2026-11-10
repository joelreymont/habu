---
title: Checked source reads and writes any address through a base plus an offset
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:43:12.993249+02:00"
---

Problem: ': T ( -- n ) data-base $100000000000 + @ ;' passes the checker and 'T' ends hb with a segmentation fault (rc 139) under --load and under tools/check.f. Reproduced by the lead on master 951b3f90. The hunt of habu-checked-src-can-88e8a977 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/nullread-hunt.md, Out of scope) also reports, not rerun by the lead: 'V BYTE-VIEW NULL-PTR BYTE-VIEW - NULL-PTR swap + @' reads V's cell and the same route with '!' writes it, rc 0. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: which bases admit an offset with no bound, what the checker knows of the offset at the read or write (the kinds, the offset mark), and which source in the tree uses each form. Acceptance: no checked program reads or writes outside the object its base names: it is refused with a reason or stops with a catchable code. Files: named by the hunt. Verify: the reproducers, refused or caught. Depends: habu-checked-src-can-88e8a977 (its fix changes what a null base plus an offset answers). Ownership: bounds of a base plus an offset. Claim: unassigned.
