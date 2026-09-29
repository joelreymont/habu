---
title: Carry the shadow emission in the capture
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.715680+03:00"
blocks:
  - habu-emit-a-second-8a260f5a
  - habu-read-recorded-sites-d4949953
---

Problem: the capture carries only the primary emission; the x86 writer needs the shadow per record.
Acceptance: the X2a reader over `NSHADOW`'s rows produces a shadow section (per-record x86 span, sites, does offsets; `aot-decl.f`/`aot-file.f` VERSION bump); xt-cell rows map through record index; the ARM64 product unchanged when no shadow; the capture part of the `docs/x86-64.md` dual-emission section.
Files: `src/habu/aot-decl.f`, `src/habu/aot-capture.f`, `src/habu/aot-file.f`, `test/aot-*`, `docs/x86-64.md`.
Verify: spark: the `test/aot-*` capture suites; rebuild; chain gen2==gen3; gate.
Depends: habu-emit-a-second-8a260f5a (X1), habu-read-recorded-sites-d4949953 (X2a). Serialise with X2a and I7 on `aot-capture.f`/`aot-closure.f`.
Route: Alder (shared: src/habu/aot-decl.f, src/habu/aot-capture.f, src/habu/aot-file.f, test/aot-*).
Ownership: krait (Intel lane).
Claim: unassigned.
