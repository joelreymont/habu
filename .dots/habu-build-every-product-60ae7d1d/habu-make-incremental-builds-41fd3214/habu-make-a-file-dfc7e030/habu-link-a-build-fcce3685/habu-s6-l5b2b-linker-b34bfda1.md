---
title: "S6 L5b2b: LINKER:WRITE; a stripped arm64 program runs"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T00:13:13.457957+02:00"
---

Problem: design-s6-l5.md L5-2 and L5-5 with Revision 2: a build's space links into a product with a boot (L5b2a), but nothing writes it to a file, so no stripped program built this way runs (S7). Acceptance: public LINKER:WRITE ( ptr u8 n -- ) in src/habu/link-data.f throws E-COMP-STATE outside WITH and in mode engine, dies 74 when the space's APP-ENTRY:XT-CELL copy is 0, then runs LINK-STRIPPED and DRV-EMIT-IMAGE with SIGN-ID:PROG$; test/compiler/arm64-link-run.f, one build writing three programs by storing each entry with xt! into APP-ENTRY:XT-CELL: LR-HI prints Hi and exits 0, LR-THROW exits 3, LR-XT stores an xt at run time and prints Hi, exit 0; WRITE outside WITH gives E-COMP-STATE; its suite row after arm64-link-data's. Files: src/habu/link-data.f, test/compiler/arm64-link-run.f, test/gate-arm-cases.f. Verify: brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-s6-l5b2b.md: bin/hb --load test/run.f -- arm64-link-run arm64-link-data arm64-place linker. Depends: habu-s6-l5b2-the-d9745dbd. Ownership: carl. Claim: unassigned.
