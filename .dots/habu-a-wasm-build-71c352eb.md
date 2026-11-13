---
title: A Wasm build refuses a file that requires src/lib/entropy.f
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T19:02:36.543580+02:00"
---

Problem: with the Wasm-through-the-linker engine (hb-build.f --target wasm), 'require src/lib/entropy.f' plus an empty MAIN is rc 74, 'E-TARGET habu: src/lib/ffi-abi.f:28: the file does not build for wasm' (ffi-abi.f's ?TARGET admits macOS and Linux only); the same with src/lib/json.f is rc 0. entropy.f's only source is libc getentropy through FUNCTION: (src/lib/entropy.f:24), and a Habu Wasm module imports nothing (src/arch/wasm/link.f:26), so no Wasm program can load src/lib/map.f, src/lib/xml/state.f or anything else that keys a table with ENTROPY:FILL. Acceptance: decided with the Wasm work: where a Wasm module's entropy comes from (a host-filled seed, an import, or none and keyed tables refused by name on that target), then entropy.f chooses its source by target at top level and a Wasm fixture loads map.f. Depends: Wasm builds through the one linker (habu-emit-wasm-from-0d5dab47). Claim: unassigned.
