---
title: "Wasm 3: a Wasm module's kernel routines emit and !"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:04:23.729327+02:00"
---

Problem: a Wasm module has no hb kernel, so src/arch/wasm/link-module.f CALLEE refuses every kernel word no kernel-words.f word provides. Two block every real program: `emit` (`link: a Wasm module has no emit`) and the one-cell store: src/habu/t0.f DO-STORE compiles `!` as a call to the kernel word on every machine (`w 1 = if to loc DO-CALL`), so `variable ACC : MAIN ( -- ) 0 ACC ! ;` is refused `link: a Wasm module has no !`. Decision: tier 0 stays one shape on every machine (a call to the kernel's `!`); the Wasm module's kernel provides it as a raw routine beside emit, as hb's arm64 kernel provides the primitive. An inline store in tier 0 is a speed change for every machine, not this gap's fix. Acceptance: `hb-build --target wasm` builds `: MAIN ( -- ) 53 emit ;` to a module that prints 5 under bun, and a program that stores a variable from a word builds and runs to 0; an unprovided kernel word is still refused by name. Files: src/arch/wasm/link-routines.f (new), src/arch/wasm/link-module.f, test/wasm/hb-build.f (new), test/wasm/device.f. Verify: bin/hb --load test/wasm/hb-build.f ends test: ok. Depends: habu-wasm-3-linker-0a8d4f6b. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3e.
