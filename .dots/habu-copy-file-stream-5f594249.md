---
title: COPY-FILE-STREAM closes its source when the destination is refused
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T18:05:04.091971+02:00"
---

Problem: src/lib/fs-mutate.f COPY-FILE-STREAM opens the source (COPY-OPEN-SRC) and then COPY-OPEN-DST runs EXISTS? and >PATHZ2 on the destination, which throw E-FFI-NUL or E-FS-PATH with the source descriptor still open: every refused destination leaks one descriptor. Acceptance: no path through COPY-FILE-STREAM leaves a descriptor open, shown by a test that copies to a refused destination and finds no descriptor left. Depends: none. Claim: unassigned.
