---
title: "Wasm 3: build the storage test through hb-build"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T19:57:09.657003+02:00"
closed-at: "2026-11-10T21:18:01.717142+02:00"
close-reason: "landed in master 17bf4285; its gate: engines converge, test/wasm/device.f (thirteen test: ok), arm64-link-run.f, error-code-lint.f, diag-position-test.f rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem: test/wasm/storage-test.f builds its modules with tools/wasm-build.f, the capture build that hb-build --target wasm replaces. Acceptance: each entry of test/wasm/storage.f is the MAIN of a module hb-build builds; the test ends test: ok. Files: test/wasm/storage-test.f, test/wasm/storage.f. Verify: bin/hb --load test/wasm/storage-test.f. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.
