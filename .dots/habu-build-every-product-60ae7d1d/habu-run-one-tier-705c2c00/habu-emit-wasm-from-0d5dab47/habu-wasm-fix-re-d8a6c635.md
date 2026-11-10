---
title: "Wasm fix re-review: fault comments overstate, the pass case hides its check"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T20:06:11.658951+02:00"
---

Problem (fix re-review, low): src/arch/wasm/link.f's wrappers comment and test/wasm/harness.f's header say every trap leaves a fault kind and address, but only a checked fault stores them (a freed-twice page or a full OUT traps with 0 0); test/wasm/hb-build.f PASS-CASE says fault-addr is the failed check's number and never reads it; test/browser/storage-test.f has a 129-column comment line. Acceptance: the two comments say a checked fault stores them; PASS-CASE asserts WASM-HARNESS:FAULT-ADDR is 0; the line is wrapped. The second copy of OUT-CAP in test/wasm/device.f goes with habu-wasm-3-remove-5698c955. Files: those named. Verify: bin/hb --load test/wasm/hb-build.f; the engine's bytes unchanged. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.
