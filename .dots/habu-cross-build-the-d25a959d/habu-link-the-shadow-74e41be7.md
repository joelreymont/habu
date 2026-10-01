---
title: Link the shadow blob into the x86 image
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:29.000279+03:00"
blocks:
  - habu-link-records-and-647852d3
---

Problem: the shadow blob's sites are unresolved until the writer places them.
Acceptance: rel32 calls to records and kernel bodies through the xt->record index and the kernel labels; `MOVABS` DATA/CODE sites; xt cells; does records; refusals for unresolved symbols and out-of-range displacements before any byte is written.
Files: `src/habu/link-x64.f`, the X4b test.
Verify: spark: the host-side test, including each refusal before any output byte.
Depends: habu-link-records-and-647852d3 (X4b), habu-record-symbolic-x86-10037f07 (C6).
Route: direct.
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-10-01, X2b landed): the capture's shadow tables are `AOT-SHADOW` (`src/habu/aot-decl.f`), filled by `AOT-CAPTURE:SHADOW-CAPTURE` (`src/habu/aot-shadow.f`) and carried as AOT sections 18-21 (VERSION 14); `docs/x86-64.md` "Dual emission" states the format. A site's target is `SITE-REC-TAG` with a window record index or `SITE-NAME-TAG` with a prefix name's pool offset; xt cells are XT rows keyed by record. A site may name a window record that has no x86 routine (a tier-0 word, or one defined before the shadow opened: `WCELL`, `HOOK`, `ARM-HOOK` in `test/aot-shadow-capture.f`); this leaf builds that routine or refuses it by name. An address cell holding a quotation entry is refused under a shadow: mapping it needs the primary emission's function offsets in the NSHADOW map.
