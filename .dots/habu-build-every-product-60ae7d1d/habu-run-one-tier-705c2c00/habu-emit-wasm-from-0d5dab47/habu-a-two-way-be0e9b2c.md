---
title: A two-way target selector picks its Linux arm for a Wasm target
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T17:30:08.287093+02:00"
---

Problem: HB-TARGET-WASM? makes Wasm a supported target, and docs/xcomp.md "Porting" says every selector handles each supported target explicitly, but a selector of the form 'HB-TARGET-MACOS? if A else B then' (or [if]) takes B, the Linux arm, for a Wasm target without a refusal. Known case: src/core/include.f ENGINE-FIND names /proc/self/exe for every target but macOS (a start-up file, so every Wasm module compiles it; not seen to run in a build). 73 files under src name an HB-TARGET word (rg -c, counted 2026-11-10; which of them are two-way was not counted). Acceptance: every selector in a file a Wasm build loads names Wasm or refuses by name; a Wasm build of a program that requires a file with no Wasm arm stops with that file's name. Files: the selectors a Wasm build of Maki's viewer loads, src/core/include.f. Verify: the Wasm build of Maki's viewer, and a rejected program requiring src/lib/ffi-abi.f. Depends: habu-wasm-3-linker-0a8d4f6b (a module that runs). Ownership: none. Claim: unassigned.
