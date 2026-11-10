---
title: "Wasm 3: build the differential and data tests through hb-build"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T19:57:09.731252+02:00"
---

Problem: test/wasm/differential.f and test/wasm/data-test.f build their modules with the capture build. Acceptance: each builds its modules with hb-build --target wasm (a wrapper source whose MAIN is the entry) and ends test: ok; data-test.f names WPROF:DATA-BASE. A row that reaches an unprovided word is a lane or a waiting case, named. Files: test/wasm/differential.f, data-test.f, data-region.f, numeric-rows.f. Verify: each test by bin/hb --load. Depends: the depth routine. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.
