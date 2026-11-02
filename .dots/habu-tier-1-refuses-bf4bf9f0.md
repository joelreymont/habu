---
title: "Tier 1 refuses a CAST: with a quotation input"
status: active
priority: 2
issue-type: task
created-at: "2026-11-02T17:15:50.442499+02:00"
---

Problem: on master 6af9c1ac, test/wasm/dynamic.f fails at load with uncaught throw -8651 (E-NELAB-QUOT), rc 67, so test/wasm/device.f (the wasm device check, not a gate row: it needs wasm-tools and bun) cannot run. Reported by tim 2026-11-02 on a converged engine (generation 1 built from master with the installed hb as host, generation 2 byte-identical: .jj-ws.noindex/tim-h2-base/.out/eng2/hb); the installed hb fails the same way on master's sources. tim bisected the file: lines 1-61 load; line 62, CAST: >NONE ( [ n n n -- ] -- [ -- ] ), throws -8651, while line 61, CAST: >CELL ( n -- ptr n ), loads. The file selects tier 1 (dynamic.f:57 '1 set-tier') before those lines. Last commits touching src/compiler/native/elaborate.f and compiler.f: 27e3cf87, 4b54ed21, 58b72b8d, f1bfc41d (history not bisected). Acceptance: the root cause named at its layer (which compiler path elaborates a CAST: body whose input is a quotation type, and why it refuses), fixed there; test/wasm/dynamic.f loads past line 62, and device.f runs on this Mac; a rejected-or-admitted case in the existing CAST: tests under tier 1 that fails before the fix, if no gate row covers it. Files: per the root cause. Verify: bin/hb --load test/wasm/device.f; the CAST: test rows. Depends: none. Ownership: the root-cause layer. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-castq brief=carl-dots/.out/carl-file/brief-cast-quot.md.
