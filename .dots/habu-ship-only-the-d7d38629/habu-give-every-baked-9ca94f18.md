---
title: Give every baked package one owning load path
status: closed
priority: 1
issue-type: task
created-at: "2026-09-30T18:50:08.001947+02:00"
closed-at: "2026-10-01T18:40:00.000000+02:00"
close-reason: "Landed: every baked package has one owning file, error constants are minted by their owners, the label cells live in packages of their own, the five TFAM and CHECKER-TAPE tests run in whitebox children and Etch has IMAGE-LIFECYCLE:COUNT. Fixpoint, whitebox, native suite and dot lint pass; the product size is unchanged (docs/engine-size.md)."
---

Problem: sealing every captured package (habu-seal-every-captured-c550102f) refuses the sources on the product path that reopen a package the engine bakes. They are:
- `src/habu/aot-decl.f` (AOT-SIG, AOT-SPAN, AOT-WINDOW), `src/habu/address-carrier.f` (SNAP-RELOC) and `src/habu/code-origin.f` (TIER-PROV);
- `lib/span.f:156` (MEM);
- the JR, FMATH, PG and DB-ROWS packages, which `lib/errors.f:217,244,1370,1423` create inside the engine, so that `lib/json-read.f`, `lib/fmath.f`, `lib/pg.f` and `lib/db/rows.f` reopen engine packages;
- five product-suite tests, reopening TFAM (3) and CHECKER-TAPE (2);
- Etch `test/hook-count.f:15` (IMAGE-LIFECYCLE);
- the stage sources' reopens of packages `src/habu/layout.f` defines (PROT :631, HIDX :564/:946, AOT-SIG :657, AOT-SPAN :680, AOT-WINDOW :1773, SNAP-RELOC :1553), to add public label cells: `src/habu/habu1.f:137,150,178,189,342,2351` and `src/habu/habu2.f:196,5550,6002,6377,6430,6627,11430`. The product-hosted refresh compiles them in a `bin/hb --build stage2-src` child (`tools/build-fixpoint.f:516-530`) after PREFIX-REWIND:TO-CORE re-arms the seal floor (`prefix-rewind.f:88-92`), so each dies rc 84 in C-PACKAGE-PROT-GUARD (`habu2.f:8426-8445`) once sealed, as `package PKG-AUTH ;package` does on master today.
Acceptance:
- No source loaded on the product engine reopens a package the engine bakes; each package has one owning file.
- Package-qualified error constants are minted in the file that owns their package, with `tools/error-code-lint.f` clean.
- `MEM:ALLOC-SPAN` keeps its spelling, so no caller changes (12 in Habu, 49 in Tender); its definitions move to the file that owns MEM.
- The five files that reopen TFAM and CHECKER-TAPE run only in children on the whitebox engine (WHITEBOX-CHILD:ENGINE$); their four rows stay plain SUITE per docs/gate.md.
- The meta-compiler's label cells live in packages of their own: no stage source opens a package `layout.f` defines.
- Etch's reopen is reported to Etch with the public accessor it needs.
Files: the sources above, `src/habu/habu1.f`, `src/habu/habu2.f`, `test/gate-stdlib-cases.f`.
Verify: native build; `bin/hb --load tools/build-fixpoint-refresh.f -- stdin` on the built engine; `bin/hb --load test/run.f` (SUITEs build-fixpoint-fixtures and aot-chain-producer run the `--build` child); the moved packages' suites; generations byte-identical.
Depends: none. Parallel with 974304d0.
Parent: habu-ship-only-the-d7d38629. Design: the Fable surface design of 2026-09-30 (~/.cache/tmp/heron-arm64/design-surface.md); census: ~/.cache/tmp/heron-arm64/size-census/.
