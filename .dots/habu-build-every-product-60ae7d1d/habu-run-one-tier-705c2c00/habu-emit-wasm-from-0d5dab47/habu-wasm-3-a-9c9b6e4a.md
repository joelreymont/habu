---
title: "Wasm 3: a Wasm module's division, views, atomics and max"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T18:14:06.357755+02:00"
closed-at: "2026-11-10T20:15:32.789732+02:00"
close-reason: "landed in master e4ec011f; its gate: engines converge, 12 focused tests rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem: Maki's viewer reaches kernel words a Wasm module does not provide (src/arch/wasm/link-module.f refuses them by name): `/`, `mod` (tier 0 inlines them only for a literal divisor), `cell-view`, `byte-view`, `atomic!`, `atomic-cas` (src/core/dynamic-storage.f LOCK, UNLOCK through arenas) and `max`. Acceptance: each resolves, as a raw routine of src/arch/wasm/link-routines.f or, for max, a kernel-words.f word named in kernel-map.f; a zero divisor throws E-DIV-ZERO and MIN-N / -1 answers MIN-N as tier 1's SEL-DIV. Files: src/arch/wasm/link-routines.f, kernel-words.f, kernel-map.f, test/wasm/hb-build.f. Verify: bin/hb --load test/wasm/hb-build.f ends test: ok. Depends: habu-wasm-3-a-e5fad024. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3a.
