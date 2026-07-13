---
title: "typed-top: compile-mode immediate depth guard"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T10:38:59.767876+02:00"
---

Sub-dot 5 of docs/typed-top-level.md sec 5 (landed 8cefda08). Files: src/habu/habu2.f (compile-path BLR gains the min-in/hook gate against the interpret row), extend test/underdepth-gate.f. Acceptance: p4 shape (immediate at compile-time underdepth) fails closed rc 70 named diagnostic BEFORE the below-base read; both cold-prefix paths covered, matching the FOO2 regression pattern. Related: the p5 wrong-certificate dot habu-checker-fitting-arity-70dc94e4 (sub-dot 1) is the model fix; this is the depth side.
