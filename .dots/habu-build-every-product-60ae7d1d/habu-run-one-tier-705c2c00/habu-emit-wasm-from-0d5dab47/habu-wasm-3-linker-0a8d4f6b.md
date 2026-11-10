---
title: "Wasm 3: LINKER:WRITE for a Wasm target"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T16:58:14.915763+02:00"
---

Step 3 of the Wasm campaign, census stop 2. Problem: LINKER:WRITE (src/habu/link-data.f) and the layout it reaches (link-band.f LAYOUT) are arm64's: a Wasm build dies 'link: MAIN is not whole 4-byte words'; tools/hb-build-core.f APP runs KERNEL-BUILD for a Wasm target, which writes an arm64 kernel object the link then adopts; the reach reads a site's value as an arm64 chain (link-band.f CHAIN-AT, link-data.f READ-EM). Acceptance: 'hb-build --target wasm' of ': MAIN ( -- ) 53 emit ;' writes a module wasm-tools validates and test/wasm/run.mjs runs under bun, printing 5; no arm64 kernel object is built for it; an arm64 engine and stripped program are byte-identical before the signature. Files: src/habu/link-band.f, link-data.f, tools/hb-build-core.f, a new LINKER part beside src/arch/wasm/link.f, kernel routines. Verify: the build and run above; arm64 bytes. Depends: the layout lane, the kernel census, the start-up rows. Ownership: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3w (the writer lane; the shared reach and copy words moved to src/habu/link-copy.f first, in jerry-wasm).
