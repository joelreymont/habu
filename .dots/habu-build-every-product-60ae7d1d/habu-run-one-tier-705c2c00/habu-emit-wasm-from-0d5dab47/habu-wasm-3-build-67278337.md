---
title: "Wasm 3: build the calls test through hb-build"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T19:57:09.692109+02:00"
closed-at: "2026-11-10T21:18:01.742160+02:00"
close-reason: "landed in master 17bf4285; its gate: engines converge, test/wasm/device.f (thirteen test: ok), arm64-link-run.f, error-code-lint.f, diag-position-test.f rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem: test/wasm/dynamic.f links a tier-1 capture window with WASMLINK, which the removal deletes. Acceptance: test/wasm/calls.f (package WCALLS) holds the execute, catch and finally entries; CHECKS runs natively to 0 and as a module's MAIN to 0; each trapping entry is a module that traps; test/wasm/calls-test.f is the driver in storage-test.f's shape and test/wasm/dynamic.f is gone. Files: test/wasm/calls.f and calls-test.f (new), test/wasm/dynamic.f (deleted), test/wasm/device.f. Verify: bin/hb --load test/wasm/calls-test.f. Depends: the depth routine. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.
