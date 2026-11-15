---
title: Verify a library selection
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:10:10.689451+01:00"
closed-at: "2026-11-16T04:07:33.824157+01:00"
---

Problem: `check.f -- --verify-only` defers at `LIBRARY` with W-CHECK-DEFERRED (no `parses:` row, src/core/checker.f:16436) although src/lib/ffi-abi.f states `parses: LIBRARY 1`, and checks nothing after it: the engine's `parses:` (src/core/cell-effects.f:55-66) checks a row and keeps nothing, and the verifier never reads an engine-provided file (src/habu/verify-source.f PRS-FIND :3063). VERSIONED-LIBRARY has no row at all. Acceptance: --verify-only finds the rows of LIBRARY and VERSIONED-LIBRARY and checks the source after their operands. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
