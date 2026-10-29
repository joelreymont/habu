---
title: "Lower T0 from the checker's events, not the text"
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T12:42:17.593341+02:00"
blocks:
  - habu-checker-files-the-99b78109
---

Big-picture step 2: the observer keeps the events; the walk dispatches on kind; keyword tables, OP?/CELL-OP?, text re-parsing, spelling lookups and NFETCH's hash-and-copy go; widths read live; does> lowers parent before the clause scan. Record readers move from NDICT into XREF. This is the load fix (replaces the A6 recording-narrowing plan). Measure zz interleaved.
