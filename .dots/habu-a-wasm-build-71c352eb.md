---
title: A Wasm build refuses a file that requires src/lib/entropy.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T19:02:36.543580+02:00"
---

Problem: measured 2026-11-10 with the Wasm-through-the-linker engine (hb-build.f --target wasm): 'require src/lib/entropy.f' plus an empty MAIN is rc 74, 'ncomp: cannot compile DLOPEN-RAW at DLOPEN-SLOT', 'E-UNDEFINED: DLOPEN-SLOT at src/lib/ffi-abi.f:183'; the same with src/lib/json.f is rc 0. entropy.f's only source is libc getentropy through PROCESS-SYMBOLS and FUNCTION:, and a Habu Wasm module imports nothing (src/arch/wasm/link.f 'nothing is imported'), so no Wasm program can load src/lib/map.f, src/lib/xml/state.f or anything else that keys a table with ENTROPY:FILL. JSON:VALUE= was kept off it for that reason (it sorts b's members instead). Acceptance: decided with the Wasm work: where a Wasm module's entropy comes from (a host-filled seed, an import, or none and keyed tables refused by name on that target), then entropy.f chooses its source by target at top level and a Wasm fixture loads map.f. Files: src/lib/entropy.f, src/arch/wasm, test/wasm. Verify: a Wasm build of a file requiring map.f. Depends: Wasm builds through the one linker (habu-emit-wasm-from-0d5dab47). Ownership: entropy.f. Claim: unassigned.
