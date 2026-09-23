---
title: "Take an executed quotation's own mask in RSEXEC"
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T19:01:45.625948+03:00"
---

Problem: RSEXEC takes the conservative edge for an executed quotation - it ignores the quotation's own intact mask (THROW-EDGE-ROWS) and records every input overwritten for the word that executes it - so a word that ticks a throwing callee into execute or finally records 'every input overwritten' for itself, and a caller's ['] THAT-WORD catch stales its whole window although only the callee touched one cell (measured by the tick-route lane: ['] THROWING catch in test/compiler/native-finally.f stales the point because FAIL-BODY, executed inside THROWING, dropped it; the row now proves depth only). Correct but coarse. Acceptance: RSEXEC folds the executed term's exceptional masks (QXDA/QXRA) into the executing word's intact evidence the way RSCATCH consumes them, so a word whose only throw path is a caught or executed quotation records the quotation's masks, not all-overwritten; a row in test/catch-stale-suite.f with a wrapper that executes a callee keeping its window, ticked and caught by its caller, keeps the window typed; every existing catch-stale row unchanged. Files: src/core/checker.f RSEXEC, THROW-EDGE-ROWS, test/catch-stale-suite.f, docs/forth.md. Verify: test/catch-stale-suite.f, test/xt-effect-test.f, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
