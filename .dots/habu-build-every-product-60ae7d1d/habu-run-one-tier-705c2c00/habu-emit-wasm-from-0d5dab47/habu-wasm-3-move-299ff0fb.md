---
title: "Wasm 3: move the old build test's cases onto hb-build"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T19:57:09.707368+02:00"
closed-at: "2026-11-10T21:18:01.754205+02:00"
close-reason: "landed in master 17bf4285; its gate: engines converge, test/wasm/device.f (thirteen test: ok), arm64-link-run.f, error-code-lint.f, diag-position-test.f rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem: test/wasm/build.f tests tools/wasm-build.f, which the removal deletes; its cases about what a module does have no other home. Acceptance: those cases run in test/wasm/hb-build.f through hb-build --target wasm; build.f and its require in device.f are gone. Files: test/wasm/hb-build.f, test/wasm/build.f, test/wasm/device.f. Verify: bin/hb --load test/wasm/hb-build.f. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3b.
