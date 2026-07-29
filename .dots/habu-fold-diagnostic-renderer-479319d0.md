---
title: Fold diagnostic renderer qualified family name
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T15:18:46.585382+02:00"
---

Full context: maki/fusion-plan-test.f cases 97 and 98 fail. The test asserts the JSON diagnostic contains the case-folded interned names (cad-kind:region and cad-kind:node-id), per its own comment and dot habu-checker-diagnostic-renderer-66c3e741, but src/core/render.f around lines 153-158 emits the package prefix through TFAM-PKG$ verbatim, producing CAD-KIND:node-id. Confirmed by dumping DIAG-BUFFER$ for a rejected candidate. This was MASKED until now: every path to the test died first at throw 7135 E-CAST-OWNER, which habu-cast-v2-family-741e7bae has fixed. Decide which spelling is authoritative — word names fold in diagnostics, so folding is the likely answer — fix render.f, and keep a negative regression in test/type-decl-suite.f for a locals-sourced term. Acceptance: maki/fusion-plan-test.f rc 0, maki/test.f advances past that phase, test/type-decl-suite.f and tools/check-test.f green. Ownership: src/core/render.f.
