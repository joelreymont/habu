---
title: "Wasm 2d: T0's Wasm generator, quotations, does> clauses and recurse"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T16:42:02.000720+02:00"
---

Step 2 of the Wasm campaign, fourth lane. Problem: a definition with a quotation, a does> clause or recurse compiled for Wasm ends hb with 'hb: unset quotation' (exit 86): src/habu/t0-wasm.f leaves EMIT-QUOT-OPEN-XT, EMIT-UNIT-OPEN-XT, EMIT-ADDR-QUOT-XT and EMIT-CALL-SELF-XT unset, and every start-up file of a build uses quotations. Acceptance: the four cells are filled; an address or a call naming a function of its own emission holds that function's offset from the emission's byte 0 (tier 1's rule, src/arch/wasm/capture.f FUN site and self call), which the linker's Wasm write step reads; the callback stub stays unset. Files: src/habu/t0-wasm.f. Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w2d.md (the engine builds; the generator check extended with quotations, a recursive word run to its result and two does> definers validates and runs to status 0). Depends: habu-wasm-2c-t0-ad4355e4 (merged in the integration workspace). Ownership: that file. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w2d.
