---
title: Print a source place through one word
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T14:10:08.062834+02:00"
---

Problem: three words print a source place (path:line) each in its own way: src/compiler/native/definition.f SAY-WHERE (:378), src/core/util.f ERROR-TEXT:PREFIX (:126; called from src/habu/interpreter.f:315, definition.f:406 and :427, src/core/checker.f:24370 and src/core/include.f:2216), and the engine's routine at label LPLACE, src/habu/habu2.f EM-SOURCE-PLACE (:2706). Joel 2026-10-31 (docs/forth-card.md '1 Names and packages'): helpers with one meaning are one word. Acceptance: the Habu-side printers share one word at their lowest common owner (or call the engine's), with the same output on each refusal they print. Files: the three above. Verify: the refusal rows that print places (test/compiler/*refusal*.f). Depends: none. Ownership: carl. Claim: unassigned.

Acceptance case 2026-11-10 (bob; merged from habu-place-an-x86-bccec3d1 "Place an x86-64 data-space refusal by the file rule", one cause with this dot): Lane sealed (change lkxwpzlw) moves every arm64 error-place writer to SRCLOC:FILE-BUFFER?'s rule: the line counts from the file's first owned byte (OWNED) only while the buffer read lies inside the file's bytes, else the place is the file alone (Joel 2026-10-31). src/habu/kernel-x64.f DPBAD-HELPER, (:1517, its place code :1539-1557) still counts `:<line>` from SRCLOC:INB-CELL with no buffer test and prints `:1` when INB is 0, though the header above it (:1512-1516) says COMPILE-DIE-LBL is LCOMPILEDIE's twin; on x86-64 a data-space refusal (docs/forth.md ~2101 `hb: data space out of range`) inside an evaluated string or slice names a line counted inside the string. Found by reading in the sealed re-review; not run on x86-64. Fix at the helper: load OWNED, apply LPLACE's three compares (habu2.f :2712-2715: OWNED nonzero, INB >= OWNED, INE <= OWNED+OWNEDLEN), count from OWNED. Part of the x86-64 T0 work (one arch first; needs pulsar). Acceptance: the slice and string cases of test/load-reject-diag-test.f TEST-EVALUATED-SLICE shape, with a data-space refusal, on the x86-64 engine.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
