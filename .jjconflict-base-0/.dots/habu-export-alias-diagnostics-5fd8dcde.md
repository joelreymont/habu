---
title: "EXPORT alias diagnostics: defining-package provenance"
status: open
priority: 2
issue-type: task
created-at: "2026-07-10T04:50:59.309688+02:00"
---

Refinement F3 from habu-compiler-pkg-re-688212c1: CHECKER-EXPORT records a fresh scheme under the ALIAS sym, so reject diagnostics through the alias name the alias (habu: in <caller>: at 'XB:W' ...), not the defining package. Spec asks renderer/diagnostics to show the DEFINING package. Store a defining-sym backref on the alias USIG record (or a sym->sym alias table under the RBF watermarks) and teach render.f DCODE paths to append 're-export of XA:W' when the failing token's record carries the backref. Base capability landed without this; see the EXECUTION LEDGER in the parent dot. Files: src/core/checker.f (record field or side table), src/core/render.f, test/type-export-suite.f negative-diagnostic pins.
