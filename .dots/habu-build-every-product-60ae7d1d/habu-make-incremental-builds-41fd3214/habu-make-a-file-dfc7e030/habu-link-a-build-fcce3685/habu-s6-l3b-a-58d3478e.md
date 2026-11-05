---
title: "S6 L3b: a stripped build's DATA image and its address list"
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T21:41:26.359584+02:00"
---

Problem: design-s6-r2.md D3/D7: a product's DATA is the build space's raw bytes copied to DATA-VA at start, with a list of the DATA cells holding code addresses; L4 placed the routines but nothing writes the DATA image, its list, the closure's roots at xt cells, or code's data address values. Acceptance: src/habu/link-data.f (SPACE-DATA, ROWS, DATA-ADDRESS, ROOT-CELLS, DATA-IMAGE, LINK-STRIPPED, LINKER private); QUOTE-ROOT and a SOURCE-FUNCTION fallback in link-band.f LINK-ADDRESS; test/compiler/arm64-link-data.f through LINKER:WITH. Files: src/habu/link-band.f, src/habu/link-data.f, test/compiler/arm64-link-data.f, test/gate-arm-cases.f. Verify: brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-s6-l3b.md: bin/hb --load test/run.f -- arm64-link-data arm64-place linker. Depends: S5b1 (landed 6f3717e8), L4 (landed). Ownership: carl. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s6-l3b

Blocked (2026-11-05): the worker's WIP commit 3c68df8ef5bc (change vnklwpnr, "WIP: Copy a build's data into its product", +391 -4, five files) in .jj-ws.noindex/carl-s6-l3b fails arm64-link-data, exit 70: LINKER:WITH run while a host package is open refuses every definition in the space ("no authenticated package context"); the test runs inside `package LINKER private package ARM64-LINK-DATA`. Reduced: carl-file/p5.f fails, p1.f passes. Through a global-variable entry from outside the package (carl-file/probe.f, not committed) every in-process case passes and probe-bad.f exits 74. Handoff: carl-file/s6-l3b-handoff.md. The fix is WITH's open saving the host's package scope and entering none, and its close restoring it: jerry's (linker.f, S5), asked on 2026-11-05 (/tmp/carl-with-pkg.md). No test-only hook.
Redo items for the next brief: the DATA-pointer fixture uses `check@ 0 set-check ... ptr-cell! set-check`, a switch Joel ruled removed; a space has no `require` (E-UNDEFINED, /tmp/cpq/b.f) and so no PERSISTED-PTR-VARIABLE, so the DATA-pointer case waits for a space that loads src/core (S5b2 L2) and the rest lands without it; QUOTE-ROOT's path for a quotation made outside any definition is untested and the worker saw LIVE-TARGET name a stale WALKING record there.
