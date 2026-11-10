---
title: "docs/xcomp.md Wasm describes tier 1's backend as the path"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T20:33:46.620508+02:00"
closed-at: "2026-11-10T21:18:01.791254+02:00"
close-reason: "landed in master 17bf4285; its gate: engines converge, test/wasm/device.f (thirteen test: ok), arm64-link-run.f, error-code-lint.f, diag-position-test.f rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem: docs/xcomp.md 'Wasm' (Decisions' Representation and Registers rows, Control, Calls' lanes and adapters, Execution tokens' adapters, Encoder) describes tier 1's selector and encoder as how a module is built; a module is built by tier 0's generator (src/habu/t0-wasm.f) and the linker's writer, every function (ctx:i32) -> (status:i32). Acceptance: the section holds only what is true of a tier 0 module; no comment cites a heading that is gone (src/arch/wasm/profile.f:19, select.f:42, encode.f:3). Files: docs/xcomp.md, src/arch/wasm/profile.f, select.f, encode.f. Verify: read the section against t0-wasm.f's header, link.f and link-routines.f; rg 'xcomp.md "(Control|Encoder|Calls)"' src test finds nothing. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3b.
