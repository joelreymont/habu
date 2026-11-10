---
title: "Wasm 3: a linked Wasm module's reserve export"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:46:47.678752+02:00"
---

Problem: a module hb-build --target wasm links has no reserve export (src/arch/wasm/link-module.f WRITE-MODULE never calls WLINK:RESERVE!; src/arch/wasm/link.f RESERVE-BODY calls only a callee of lanes, tier 1's map-anon), so src/lib/browser/turn.js place cannot deliver to it. Acceptance: every linked module carries its map-anon routine and exports reserve, (i64 bytes) -> (i64 address), 0 when refused, through a frame callee; tier 1's lanes callee keeps working until src/habu/link-wasm.f is removed; test/browser/echo-test.f builds its module through tools/hb-build.f -- --target wasm. Files: src/arch/wasm/link.f, src/arch/wasm/link-kernel.f, src/arch/wasm/link-module.f, test/browser/echo.f, test/browser/echo-test.f. Verify: bin/hb --load test/browser/echo-test.f ends test: ok; test/wasm/hb-build.f, link.f and build.f end test: ok. Depends: habu-wasm-3-a-bfacb8f9. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3r.
