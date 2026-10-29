---
title: "Lower T0 from the checker's events, not the text"
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T12:42:17.593341+02:00"
blocks:
  - habu-checker-files-the-99b78109
---

Big-picture step 2: the observer keeps the events; the walk dispatches on kind; keyword tables, OP?/CELL-OP?, text re-parsing, spelling lookups and NFETCH's hash-and-copy go; widths read live; does> lowers parent before the clause scan. Record readers move from NDICT into XREF. This is the load fix (replaces the A6 recording-narrowing plan). Measure zz interleaved.

Lane S2A runs as lanes in jerry-s2a, on the owner field TAPE-EVENT (worxlvkp): S2A-1 T0 reads from the checker's tape (rows per ordinal; strings, chars, ticks, quotations, split by ordinal; COMPILE-TEXT scans), brief /tmp/jerry-big/s2a1-brief.md; then S2A-2 dispatch on event kind (control, locals, numbers, match) and S2A-3 names by record (inline-op table keyed by record, NDICT spelling lookups go). S2B follows (widths and fetch facts read live, NFETCH hash-and-copy deleted, does> order).
