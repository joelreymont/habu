---
title: "Delete tier 1's compiler"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T10:36:32.393144+02:00"
---

Problem: T0 compiles every arm64 and Wasm definition, yet hb still loads tier 1's whole pipeline (src/habu/t0.f requires src/compiler/native/compiler.f; src/habu/interpreter.f keeps a tier-1 path at :2182). Joel 2026-11-13 chose to delete tier 1 once Wasm is on T0, with x86-64 builds off until x86-64 builds through the linker (dot 3ed6f45d). Acceptance: no file of tier 1's pipeline (feed, tape, elaborate, HIR, loop, select, regalloc and its verifier, spill, emit, IR, session, backend registry, both backends) is loaded or kept; the shared files T0, the linker and the Wasm linker use stay; x86-64 tests are off with [if] naming 3ed6f45d; engine warm equals cold and the suite passes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-t1
