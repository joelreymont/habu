---
title: "Wasm 3: build the storage test through hb-build"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T19:57:09.657003+02:00"
---

Problem: test/wasm/storage-test.f builds its modules with tools/wasm-build.f, the capture build that hb-build --target wasm replaces. Acceptance: each entry of test/wasm/storage.f is the MAIN of a module hb-build builds; the test ends test: ok. Files: test/wasm/storage-test.f, test/wasm/storage.f. Verify: bin/hb --load test/wasm/storage-test.f. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.
