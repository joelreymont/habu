---
title: Measure and fix the variable and constant axioms
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T05:46:50.948255+02:00"
---

Sibling of the landed create fix (75d82809, deliberately left alone there): PRIM: variable ( -- ptr a ) and PRIM: constant ( -- a ) carry the identical category error (truths ( -- ) and ( n -- )) - the rows state what each word DEFINES. Neither is nameable from a checked body today (XREF-START 0, E-UNDEFINED) so no call site reads them - but constant's correction changes an INPUT count and top-row.f TR-CERT-STEP takes din from the record's MIN-IN flag, so the naive fix would make the top-level tracker claim precision while the machine pops a cell. Needs its own measurement per reader (the create landing's reader-classification method is the worked example) before any row moves. Files: src/core/checker.f, src/core/top-row.f. Depends: none.
