---
title: "Wasm 3: a Wasm module's catch and finally"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:17:22.920479+02:00"
---

Problem: a Wasm module provides no catch or finally (refused by name; Maki's viewer reaches both). Tier 0 keeps the data, return and loop stacks in memory, so a caught throw must restore the return and loop depths (the i64 cells at WPROF:DATA-BASE + RSP-CELL and + LOOPSP-CELL) and a body's results lie under the code catch pushes. Acceptance: raw routines of src/arch/wasm/link-routines.f; 100000 caught throws from inside a loop or after a >r finish; finally keeps dynamic.f FINALLY-BODY's contract. Files: src/arch/wasm/link-routines.f, test/wasm/hb-build.f. Verify: bin/hb --load test/wasm/hb-build.f ends test: ok. Depends: habu-wasm-3-a-db3ad143. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3c.
