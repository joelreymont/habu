---
title: "Wasm 3: build the differential and data tests through hb-build"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T19:57:09.731252+02:00"
closed-at: "2026-11-10T21:18:01.766403+02:00"
close-reason: "landed in master 17bf4285; its gate: engines converge, test/wasm/device.f (thirteen test: ok), arm64-link-run.f, error-code-lint.f, diag-position-test.f rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem: test/wasm/differential.f and test/wasm/data-test.f build their modules with the capture build. Acceptance: each builds its modules with hb-build --target wasm (a wrapper source whose MAIN is the entry) and ends test: ok; data-test.f names WPROF:DATA-BASE. A row that reaches an unprovided word is a lane or a waiting case, named. Files: test/wasm/differential.f, data-test.f, data-region.f, numeric-rows.f. Verify: each test by bin/hb --load. Depends: the depth routine. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.
