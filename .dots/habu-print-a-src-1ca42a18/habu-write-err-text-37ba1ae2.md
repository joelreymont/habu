---
title: Write error text through ERROR-TEXT everywhere it loads
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T16:46:42.353904+02:00"
---

Joel 2026-10-31: helpers with one meaning are one word. Copies of ERROR-TEXT:TYPE / DIGITS$ left after 23c59cb9, each in a file that loads after util.f in every build (reviewer aa79 on 3ee77c1f): include.f:1946-1950 RESIDUE-DIGIT/RESIDUE-DEC (twin of the deleted SAY-DEC); aot-data-cell.f:340 AETXT (twin of TYPE), :339 AE1; driver-io.f:50-61 DRV-FAIL-DIGITS with DRV-FB/DRV-FN/DRV-FV (DIGITS$ + TYPE after the sign); main.f:211 '2 SB$ write drop'; checker-owner.f:47 REFUSE; image-bytes.f:96 M-FAIL; inline descriptor-2 writes in xref.f:581-583, repl.f:176-181, include.f:380, :492-494, :1957-1962, :2214-2215, dynamic-storage.f:342-344, checker.f:1258, :9556-9559, t0.f:1211, elaborate.f:989. Keep tools/cold-load.f:93 SAY (compiled before the rewind reloads util.f) and tools/check-verify-child-lib.f WRITE (dies on a short write). Each replacement only where the body is exactly that write.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.

Also a copy of the DIGITS$ loop: src/habu/repl.f:160-170 LDEC!.
