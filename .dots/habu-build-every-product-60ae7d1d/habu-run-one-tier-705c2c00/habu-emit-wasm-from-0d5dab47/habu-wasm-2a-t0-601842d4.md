---
title: "Wasm 2a: T0's Wasm generator, straight-line code"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T09:53:59.194994+02:00"
---

Step 2 of the Wasm campaign, first lane. Problem: src/habu/t0-emit.f slot 6 (wasm) has no generator, so a T0 definition has no Wasm body (t0.f SHADOWED runs only for a machine EMIT-PROVIDED? admits). Acceptance: new src/habu/t0-wasm.f (COMP part WASM, required from src/habu/runtime-arm64.f) fills the straight-line cells (unit open/return/close, values, stack moves, frame slots, arithmetic, compare and flag, load, call, tail); each unit is a Wasm function (i32 ctx) -> (i32 status) with the data stack in memory and its pointer in a local written to the context around calls and at return; control, return stack, loops, quotations, recurse, the does> slot and the callback stub stay unset for later lanes. Lands together with the linker's Wasm write step, which brings the end-to-end test. Files: src/habu/t0-wasm.f, src/habu/runtime-arm64.f. Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w2a.md (an engine built with the generator equals the one it builds; a check opens a Wasm shadow, compiles straight-line words, hands the bodies to WLINK, wasm-tools validates and bun runs status 0; suite once). Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.

Merged into the integration workspace as kopnsnmk (not landed). Evidence: the engine it builds equals the one that built it; the Wasm check validates and runs to status 0; suite 269 of 270. Red: wasm-backend exits 86, because test/wasm/backend.f HOST-CASE is a tier 0 definition with a quotation compiled while the Wasm shadow is open, and tier 0 now walks it for Wasm and reaches the unset QUOT-OPEN cell. Run it again when the quotation lane fills the cells; turn it off only if it is still red then.
