---
title: "Wasm 3: a Wasm module's map-anon and munmap"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T18:27:26.197314+02:00"
closed-at: "2026-11-10T20:15:32.935187+02:00"
close-reason: "landed in master e4ec011f; its gate: engines converge, 12 focused tests rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem: a Wasm module provides no map-anon or munmap (src/arch/wasm/link-module.f refuses them by name; Maki's viewer reaches both through DYNAMIC-BUFFER growth and arenas). Acceptance: each is a raw routine of src/arch/wasm/link-routines.f with tier 1's allocator (src/arch/wasm/kernel.f MAP-ROW, UNMAP-ROW): the same free list in ctx, answers, refusals and double-free trap. Files: src/arch/wasm/link-routines.f, test/wasm/hb-build.f. Verify: bin/hb --load test/wasm/hb-build.f ends test: ok. Depends: habu-wasm-3-a-e5fad024. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3m.
