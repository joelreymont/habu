---
title: Seal the diagnostic renderer into packages
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T18:56:23.074173+02:00"
---

Full context: src/core/render.f is a fully global core surface (~100 words: EMIT1, RSTR, RFOLD, RNUM, DIAG-BUFFER!, FAM-QNAME-REND, the QREND recursion, the JSON packet writers) resolved bare by checker.f. Because it is global and not on the exact-diff package gate's allowlist, changing ONE LINE of any existing body in it reds the gate — so no change to the diagnostic renderer could land at all. An interim exact-path entry was added by habu-fold-diagnostic-renderer-479319d0 following the established precedent, with five pinning cases in the lint's own test. Replace that exemption with real package owners and DELETE the entry from GLOBAL-IMPLEMENTATION? in tools/package-diff-lint-core.f. Split at natural seams rather than one catch-all package: the byte/emit sink, the diagnostic-buffer surface, the type-row renderer, and the JSON packet writer are four concerns. Coordinate with habu-seal-the-checker-5314c0ab — same subsystem, same caller cascade across src/core, src/habu and the test tree. Size this as a program, not a leaf.
