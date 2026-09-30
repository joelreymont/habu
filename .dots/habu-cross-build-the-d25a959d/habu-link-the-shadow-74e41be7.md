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
