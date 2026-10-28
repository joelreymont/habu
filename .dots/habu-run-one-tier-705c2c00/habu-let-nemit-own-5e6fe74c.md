---
title: Let NEMIT own the address-site kinds
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T15:43:05.045992+02:00"
---

Problem: ADDR-DATA/ADDR-CODE/ADDR-KIND-MAX are defined three times (src/compiler/native/hir.f:570-572, x64ir.f:259-261, a64ir.f:262-264) plus wstruct.f's spelling; they describe a sealed-emission row and must outlive tier 1 for T0 (L2 of the umbrella, first half). Acceptance: one public definition in NEMIT (src/compiler/native/emission.f); every reader names NEMIT:ADDR-*; no alias; values unchanged. Files: emission.f and the readers in the census. Verify: engine build + compiler-shadow, x86-64-link-records, aot-shadow-capture, native-unit-file, compiler session/x64-emit/x64-chain, wasm wstruct/select/backend rows, hb-build-aot-test. Depends: none (lands on master). Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0-l2a
