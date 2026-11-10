---
title: "Add SCRATCH-ALLOT, SCRATCH-VARIABLE and SCRATCH-BUFFER:"
status: active
priority: 1
issue-type: task
created-at: "2026-11-10T14:03:49.353416+02:00"
---

Problem: an owner says scratch in a second statement after declaring storage (variable X  X scratch-cell-mark; create B n allot  B n SCRATCH-BYTES), so it can be forgotten or sized wrong (DECISIONS.md 2026-11-10 'scratch storage'; design .jj-ws.noindex/jerry-wasm/.out/scratch-declared-design.md). Acceptance: global SCRATCH-ALLOT ( n -- ) reserves n bytes at here, rounded up to whole cells, and declares them scratch; SCRATCH-VARIABLE NAME makes one scratch cell, NAME ( -- ptr a ); n SCRATCH-BUFFER: NAME makes a scratch byte buffer, NAME ( -- ptr u8 ); PTR-VARIABLE, PTR-U8-TABLE and PTR-U8-TABLE-VARIABLE are built on SCRATCH-ALLOT with unchanged effects; the checker and the source verifier know the new definers (src/core/cell-effects.f, src/habu/verify-source.f); the card's section 4 states them. The record stays one row per cell for now (the range record is its own dot). Files: src/core/pointer-storage.f, src/core/cell-effects.f, src/habu/verify-source.f, src/lib/string.f, docs/forth-card.md, test/pointer-storage-test.f, test/fixtures/file-objects/main.f. Verify: engine built twice equal; test/pointer-storage-test.f; test/file-objects.f; a built program reads 0 from a SCRATCH-VARIABLE and a SCRATCH-BUFFER: its top level stored into. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-scratch.
