---
title: "Share the linker's copy steps and its past-the-data refusal; drop EACH-ROW"
status: active
priority: 3
issue-type: task
created-at: "2026-11-10T19:18:46.610170+02:00"
---

Problem (feature review B, F7): src/arch/wasm/link-module.f DATA+ repeats src/habu/link-data.f DATA-IMAGE's copy steps (FILL-COPY CUT-DEAD KEEP-STRINGS KEEP-LIST SORT-ROWS), ROW+ repeats COPY-ROW's 'declared cell past the build's data' refusal word for word, and src/habu/link-copy.f EACH-ROW has no caller. Acceptance: one word in link-copy.f for the copy steps and one for the refusal, both callers use them, EACH-ROW is gone; no behaviour changes. Files: src/habu/link-copy.f, src/habu/link-data.f, src/arch/wasm/link-module.f. Verify: engines converge; test/wasm/hb-build.f and test/compiler/arm64-link-run.f end test: ok. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-r3.
