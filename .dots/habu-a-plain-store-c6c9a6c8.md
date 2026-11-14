---
title: "A plain store into a public header pointer cell goes to the running hb: warm differs from cold"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T00:02:21.398672+02:00"
---

Problem: a program can name the engine's own header cells, so a top-level 'NULL-PTR NCOMP-DISPATCH:DECL-A !' changes the running hb that builds it, not the image, and a warm build that links the part leaves it unchanged: a probe that stores into DECL-A, and one that stores into TARGET-DECL-A, printed 1 7 cold and 2 7 warm, rc 0 both. ': P ( -- ) NULL-PTR NCOMP-DISPATCH:DECL-A ! ;' loads under bin/hb --load, and so does the same word storing into NCOMP-DISPATCH:TARGET-DECL-A. Cause: a program's top-level code runs inside the hb that builds it, and DECL-A (src/habu/layout.f:619; TARGET-DECL-A :620) is that hb's own header cell. A plain store (BSTORE src/habu/habu1.f:1723 at top level, T0's `!` op inside a word, src/habu/t0.f:1543 OP-MAP) has no ROUTE-HEADER (src/habu/habu2.f:1971); only xt! and ptr-cell! route, so FIND-STORES (src/habu/linker.f:2376) saves nothing. c!, +!, the atomic stores and a store through CELL-VIEW take the same path. Routing every plain store is wrong: the build hb's own header stores would land in the image.

Settled (lead, 2026-11-11): the defect is that a program can name the engine's own cells, not how the store is compiled. It is a case of the privacy rule (a word is restricted by a private section and the build strips private names; Joel, 2026-10-08, in habu-move-engine-internal-fc2ba551; Joel, 2026-11-06, "can tweak sealing and stripping later"), so it waits for that work and gets no cell-by-cell patch.

Acceptance: NCOMP-DISPATCH:DECL-A and TARGET-DECL-A (read in src/compiler/native/checker-owner.f, src/habu/interpreter.f, src/habu/verify-source.f and src/habu/xref.f; stored only by src/habu/runtime.f INSTALL :18-19) and SRCLOC:CURSOR (layout.f:296) are names a program is refused as undefined; a store into each through !, c!, +!, an atomic store and CELL-VIEW is refused. The raw data-base offset store is dot habu-checked-src-reads-4f721c9b. Not determined: whether clearing the build hb's DECL-A mid-build changes what it registers; who stores the same offsets under other names ($2800, $2810, $2850, $2D68, $36A8, $800, STGT-INP-CELL, CK-TKA-OFF). Depends: habu-re-enable-hb-a2404c26, habu-move-engine-internal-fc2ba551. Claim: unassigned, owner carl.
