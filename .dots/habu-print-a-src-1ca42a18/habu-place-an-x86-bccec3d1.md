---
title: Place an x86-64 data-space refusal by the file rule
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T22:18:03.347143+02:00"
closed-at: "2026-11-10T21:50:20.763323+02:00"
close-reason: merged into habu-print-a-src-1ca42a18
---

Lane sealed (change lkxwpzlw, .jj-ws.noindex/jerry-sealed) moves every arm64 error-place writer to SRCLOC:FILE-BUFFER?'s rule: the line counts from the file's first owned byte (OWNED) only while the buffer read lies inside the file's bytes, else the place is the file alone (Joel 2026-10-31, DECISIONS.md "error places"). src/habu/kernel-x64.f DPBAD-HELPER (~1683-1693) still counts `:<line>` from SRCLOC:INB-CELL with no buffer test and prints `:1` when INB is 0, though its header (~1661) says it mirrors LCOMPILEDIE's place; on x86-64 a data-space refusal (docs/forth.md ~1796 `hb: data space out of range`) inside an evaluated string or slice names a line counted inside the string. Found by reading in the sealed re-review (/tmp/jerry-sealedrv2-tmp); not run on x86-64. Fix at the helper: load OWNED, apply LSRCAT's three compares (habu2.f ~12348-12351: OWNED nonzero, INB >= OWNED, INE <= OWNED+OWNEDLEN), count from OWNED; name it beside LSRCAT in FILE-BUFFER?'s comment. Part of the x86-64 T0 work (one arch first; needs pulsar). Acceptance: the slice and string cases of test/load-reject-diag-test.f TEST-EVALUATED-SLICE shape, with a data-space refusal, on the x86-64 engine.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
