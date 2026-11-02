---
title: Throw a number in dynamic.f NAMED-SLOT
status: open
priority: 3
issue-type: task
created-at: "2026-11-02T18:22:35.312331+02:00"
---

Problem: test/wasm/dynamic.f:86 ': NAMED-SLOT ( -- ) ['] emit throw ;' is refused by the checker (throw wants n, gets xt), so test/wasm/device.f stops there once the tier-1 CAST: fix (carl-castq 788e619b, dot habu-tier-1-refuses-bf4bf9f0) lets it past line 62. Line 85 CODE-SLOT already converts with >N. Acceptance: NAMED-SLOT converts the xt as CODE-SLOT does, if the test means to throw that number (read what device.f expects from it); device.f runs on this Mac. Files: test/wasm/dynamic.f. Verify: bin/hb --load test/wasm/device.f (needs wasm-tools and bun). Depends: habu-tier-1-refuses-bf4bf9f0. Ownership: unassigned. Claim: unassigned.
