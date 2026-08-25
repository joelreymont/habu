---
title: Exempt habu2 globals in package gate
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:36:18.177524+02:00"
---

Full context: measured 2026-07-29 — adding a one-word comment to the body of ANY existing global word in src/habu/habu2.f reds tools/package-diff-lint.f with E-PACKAGE-OWNERSHIP, because habu2.f has no entry in GLOBAL-IMPLEMENTATION? (tools/package-diff-lint-core.f:254), unlike src/core/checker.f, render.f and type-family.f, which carry documented interim entries for exactly this situation. As configured the commit gate rejects every change to the native engine emitter — ~7300 lines of global-by-construction code that loads before any package exists. Either add an interim entry with the same reasoning and retirement condition as the checker entry, pinned in tools/package-diff-lint-test.f (a body edit to an existing habu2.f global passes; a genuinely NEW unpackaged global still fails), or state the engine sealing plan that replaces it. Note the packaging seams KWDATA/LOOP-EMIT/LASTC-TRUST/DOESPATCH/INTERP-EMIT/COMPILE-EMIT/LABELS/ENGINE-BUILD already exist in habu2.f (dot habu-cont-habu2-emitter-493363e7 continues them), so the retirement condition is real.

Claim: agent=snapreloc workspace=.jj-ws/habu-relocate-snapshot-region-752042fe (RELEASED 2026-08-21: workspace gone, no live lane - gc)
