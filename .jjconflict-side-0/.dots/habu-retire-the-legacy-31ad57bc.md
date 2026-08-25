---
title: Retire the legacy protected-WID capture leg
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T17:23:42.891802+02:00"
---

src/habu/aot-capture.f ACAP-PWID-CAPTURE reads the metabuild host's tag cell and takes a LEGACY path (ACAP-PWID-LEGACY, plus the PROT-WID-LEGACY-* aliases in src/habu/layout.f) when the cell holds a table-era row count instead of PROT-REG-TAG. That leg exists only for the changeover in which a table-era host builds a bitmap-era engine; both layout.f and aot-capture.f name this dot in their comments as the owner. The changeover has happened (install --force from an old-table host and from the new-bitmap host converged on the same bin/hb bytes), so once the seed on every build host has rolled past it, delete ACAP-PWID-LEGACY, PROT-WID-LEGACY-N-CELL/OFF/MAX, and the tag dispatch in ACAP-PWID-CAPTURE, leaving the tag as a pure shape assertion. Also delete the HABU_PWID_LEGACY_N refusal mode in test/aot-wid-build.f and its case in test/aot-wid-suite.f. Test gap this closes: the leg's row-to-bit conversion has no repeatable fixture, because it reads u32 rows at a FIXED live address and no table-era host exists to supply them; the suite can only prove its guards (empty registry accepted, an impossible row count refused). Its only evidence for a non-empty table is the one-time convergence measurement. Do not retire before the seed has rolled everywhere: a host still shipping a table-era binary would then build a garbage band.
