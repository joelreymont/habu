---
title: "Wasm 3: remove the capture build and tier 1's Wasm link"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T19:57:09.747379+02:00"
---

Problem: tools/wasm-build.f, src/habu/link-wasm.f, src/arch/wasm/capture.f and dynamic.f, kernel.f's rows, link.f's lanes reserve, t0.f FIXED-SHADOW, test/wasm/capture.f and link-capture.f remain after every test moved onto hb-build. Acceptance: they and their mentions (README.md, docs/xcomp.md, src/habu/AGENTS.md, tools/AGENTS.md) are gone; a test whose subject stays is disabled, not deleted; test/wasm/device.f loads and passes with WPROF:OUT-CAP. Files: those named. Verify: engines converge, the suite, bin/hb --load test/wasm/device.f. Depends: the test moves. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-rm1 (engine and source removals) and .jj-ws.noindex/jerry-rm2 (tier 1's kernel rows and the tests).
