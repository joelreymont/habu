---
title: "Wasm 3: a Wasm module's throw, die and execute"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:17:22.905946+02:00"
---

Problem: a Wasm module provides no throw, die or execute (src/arch/wasm/link-module.f refuses them by name; Maki's viewer reaches all three), and WLINK's run wrapper leaves the return and loop depths of an uncaught throw for the next host turn (src/arch/wasm/link.f RUN-BODY). Acceptance: each is a raw routine of src/arch/wasm/link-routines.f with tier 1's behaviour (kernel.f THROW-ROW, DIE-ROW; dynamic.f EXECUTE-BODY), its call_indirect named to WLINK:INDIRECT+; run zeroes RSP-CELL and LOOPSP-CELL. Files: src/arch/wasm/link-routines.f, link-module.f, link.f, test/wasm/hb-build.f. Verify: bin/hb --load test/wasm/hb-build.f ends test: ok. Depends: habu-wasm-3-a-e5fad024. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3t.
