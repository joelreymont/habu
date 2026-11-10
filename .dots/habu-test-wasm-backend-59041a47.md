---
title: test/wasm/backend.f fails 29 cases on the Wasm stack
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T17:08:40.287047+02:00"
---

Problem: 'bin/hb --load test/wasm/backend.f </dev/null' on the Wasm stack's engine (stack tip 1233ea00, engine of d0ee26bf's tree) fails 29 cases and ends 'hb: uncaught throw code -9803' (WLEB E-UNPADDED), rc 67: 'each record publication committed is filed once, with an emission of its own' expected 12 got 34; 'each emission's header names its one body, its lanes the word's arity' reads garbage; 'an address row sits on the padded field after i64.const' expected 66 got 1. The installed engine of master 6f6e9305, the stack's base, passes ('test: ok'). So a stack commit (2a 02960187 to be2f9ce5) broke tier 1's Wasm backend test, which stays with tier 1 (wasm-design.md 'Kept'). Cause unknown. Acceptance: the cause as path:line with the commit that introduced it; then a fix so the test ends 'test: ok' on the stack. Files: unknown until the hunt. Verify: the command above. Depends: none. Ownership: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3b (hunt, brief .jj-ws.noindex/jerry-wasm/.out/briefs/w3-backend-hunt.md).
