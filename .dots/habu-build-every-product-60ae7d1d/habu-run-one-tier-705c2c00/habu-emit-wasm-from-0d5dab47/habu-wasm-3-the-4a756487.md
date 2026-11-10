---
title: "Wasm 3: the browser tests build through hb-build"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T18:57:34.646504+02:00"
closed-at: "2026-11-10T20:15:33.006045+02:00"
close-reason: "landed in master e4ec011f; its gate: engines converge, 12 focused tests rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem: test/browser/{display,navigation,app,request,storage,take}-test.f spawn tools/wasm-build.f, which includes a deleted file, so none of them runs; storage-test.f names the removed WPROF:IMAGE-BASE. Acceptance: each builds its fixture with tools/hb-build.f -- --target wasm (the fixture defines MAIN) and ends test: ok. Files: those tests, their fixtures, src/lib/browser/event.f's header. Verify: bin/hb --load test/browser/<name>-test.f ends test: ok for the six. Depends: habu-wasm-3-a-61c73ac9. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.
