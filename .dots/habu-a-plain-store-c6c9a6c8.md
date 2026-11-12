---
title: "A plain store into a public header pointer cell goes to the running hb: warm differs from cold"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T00:02:21.398672+02:00"
---

Problem: a program can name the engine's own header cells, so a top-level 'NULL-PTR NCOMP-DISPATCH:DECL-A !' changes the running hb that builds it, not the image, and a warm build that links the part leaves it unchanged: probes ~/.cache/tmp/carl-aud1/decl4 and decl2 print 1 7 cold and 2 7 warm, rc 0 both. Cause: a program's top-level code runs inside the hb that builds it, and DECL-A (src/habu/layout.f:680; TARGET-DECL-A :681) is that hb's own header cell. A plain store (BSTORE src/habu/habu1.f:1894 at top level, PUT-PSTORE src/compiler/native/emit.f:1553 inside a word) has no ROUTE-HEADER (src/habu/habu2.f:2397); only xt! and ptr-cell! route, so FIND-STORES (src/habu/linker.f:2316) saves nothing. c!, +!, the atomic stores and a store through CELL-VIEW (~/.cache/tmp/carl-store decl5, decl6) take the same path. Routing every plain store is wrong: the build hb's own header stores would land in the image.

Settled (lead, 2026-11-11): the defect is that a program can name the engine's own cells, not how the store is compiled. It is a case of the privacy rule (a word is restricted by a private section and the build strips private names; Joel, 2026-10-08, in habu-move-engine-internal-fc2ba551; Joel, 2026-11-06, "can tweak sealing and stripping later"), so it waits for that work and gets no cell-by-cell patch.

Acceptance: NCOMP-DISPATCH:DECL-A and TARGET-DECL-A (read in src/compiler/native/checker-owner.f, src/habu/interpreter.f, src/habu/verify-source.f and src/habu/xref.f; stored only at src/compiler/native/compiler.f:811-812), SRCLOC:CURSOR (layout.f:340) and the global CK-AOT-SIG-POOL-FIELD (src/core/checker.f:23257) are names a program is refused as undefined; probes decl4 to decl7 are refused. The raw data-base offset store is dot habu-checked-src-reads-4f721c9b. Not determined: whether clearing the build hb's DECL-A mid-build changes what it registers; whether built copies of checker.f's top-level code reach TRANSFER-CHECKED (checker.f:27046) in a program build, or reach it by address; who stores the same offsets under other names ($2800, $2810, $2850, $2D68, $36A8, $800, STGT-INP-CELL, CK-TKA-OFF). Depends: habu-re-enable-hb-a2404c26, habu-move-engine-internal-fc2ba551. Claim: unassigned, owner carl.
