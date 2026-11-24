---
title: "WKMAP and kernel-words.f still carry what only tier 1's capture asked for"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T20:22:09.814379+02:00"
---

Problem: src/arch/wasm/kernel-map.f WORD-OF is asked by LINKER:WASM:PROVIDER (src/arch/wasm/link-kernel.f) with the name of a kernel word a module calls. Rows such as NULL-PTR, data-base, BYTE-COPY and BYTE-COPY-LEN were added for tier 1's capture window; whether a tier 0 build ever asks for them is not measured. Acceptance: each row a tier 0 build of the goal programs (the Wasm tests, Maki's viewer) never asks for is gone with its word; each WKMAP row that stays has a SAME case in test/wasm/kernel.f or a named module test (today `.s` and `data-base`, which read Wasm addresses and cannot run on the host, and `max`, `fdrop`, `fdup`, `NULL-PTR`, `BYTE-COPY`, `BYTE-COPY-LEN` have none). Files: src/arch/wasm/kernel-map.f, kernel-words.f. Verify: engines converge; test/wasm/hb-build.f, storage-test.f, the browser tests. Depends: none. Ownership: none. Claim: unassigned.
