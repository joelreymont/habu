---
title: Re-measure the stale macOS CODE-TEXT row
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T14:53:29.697807+02:00"
---

test/gate-size-attribution-test.f:283 commits MACOS-CODE-TEXT 117740, and SIZE-ATTR:HOST-CODE-TEXT enforces it on macOS through GE-CODELEN-RATCHET (test/gate-engine-lib.f:399). The row is stale by 324 bytes: measured on macos-arm64 in .jj-ws/habu-status-census-mirror at proofs 50a5fda9 with src/ untouched, the engine build slice reports 'candidate 118064 / baseline 117740' and 'FAIL: candidate CODELEN ratchet: __text grew past the CODE-TEXT row'. Proven pre-existing and independent of the STATUS.md retirement: a clean checkout of 50a5fda9 with only the GE-CENSUS-RATCHET call removed produces the identical 118064/117740 pair, and the retirement commit changes no file under src/. Before the retirement the slice died earlier at the census ratchet, so this red was masked. Fix: re-measure MACOS-CODE-TEXT at the byte fixpoint of whatever tree lands it and transcribe the measured number (never predict it, per docs/worker-briefing.md), then prove the enforcement red-first by mutating the row. Related but distinct: habu-commit-the-macos-17438b18 owns the per-region macOS budgets, which are OWED and skipped, not stale; this dot owns the single enforced CODE-TEXT total. Files: test/gate-size-attribution-test.f only. Verify: the engine build slice (bin/hb --load ... test/gate-engine.f -- build) reaches 'PASS: exact CODELEN ratchet' on macos-arm64. Note: test/gate-size-attribution-test.f is inside the ownership of active dot habu-split-gate-size-4a6fad8b; sequence against it.
