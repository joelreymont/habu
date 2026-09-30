---
title: Emit the x86 dictionary index and search rows
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T09:22:47.250064+03:00"
blocks:
  - habu-size-the-x86-fb635c93
---

Problem: I2's `OUTER:FIND` runs on `search-wl` at x86 run time, and the x86 kernel has no dictionary index or search rows. Split from K9 by the K-lane design (2026-09-30).
Acceptance: twins of `HIDX:LREBUILD`/`LHIDXADD` (`habu1.f:220-225`, `4090-4230`) and `WLFIND:LENTRY` (`habu1.f:3253-3327`); rows `search-wl` (`habu1.f:3329-3341`: refuses `OWNER-API-PRI-WID` and `DNAME-INT`) and `xref-search-wl` (`habu1.f:3345-3348`). Cases through the booted harness in `test/x86-64-kernel-engine.f` over a seeded dictionary: found, absent, a private wid refused, an interpret-only name refused; the table joins `docs/x86-64.md` `### Engine-state rows`.
Files: `src/habu/kernel-x64.f`, `test/x86-64-kernel-engine.f`, `docs/x86-64.md`.
Verify: host K3's product engine: `--load test/x86-64-kernel-engine.f`; ThinkPad: the images natively.
Depends: habu-scaffold-the-x86-9af80979 (scaffold). Hand-written through `X64ASM`.
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: unassigned.
