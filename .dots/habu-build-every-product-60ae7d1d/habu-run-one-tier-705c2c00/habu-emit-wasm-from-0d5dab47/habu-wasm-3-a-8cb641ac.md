---
title: "Wasm 3: a module provides depth and .s"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T19:57:09.674400+02:00"
---

Problem: a Wasm module the linker writes has no depth and no .s; test/wasm/numeric-rows.f ends every row with both and test/wasm/calls.f measures depth, so the differential and calls tests cannot move onto hb-build. Acceptance: depth is a kernel routine (src/arch/wasm/link-routines.f) answering the cells between ctx's stack base and top; .s is a kernel-words.f word printing each cell deepest first as the engine's; a module of '1 2 3 depth . .s' prints 3 1 2 3; test/wasm/hb-build.f's unprovided-word refusal names two other words. Files: src/arch/wasm/link-routines.f, kernel-words.f, kernel-map.f, test/wasm/hb-build.f. Verify: engines converge; bin/hb --load test/wasm/hb-build.f. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.
