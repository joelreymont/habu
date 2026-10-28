---
title: Let NEMIT own the address-site kinds
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T15:43:05.045992+02:00"
---

Problem: ADDR-DATA/ADDR-CODE/ADDR-KIND-MAX are defined three times (src/compiler/native/hir.f:570-572, x64ir.f:259-261, a64ir.f:262-264) plus wstruct.f's spelling; they describe a sealed-emission row and must outlive tier 1 for T0 (L2 of the umbrella, first half). Acceptance: one public definition in NEMIT (src/compiler/native/emission.f); every reader names NEMIT:ADDR-*; no alias; values unchanged. Files: emission.f and the readers in the census. Verify: engine build + compiler-shadow, x86-64-link-records, aot-shadow-capture, native-unit-file, compiler session/x64-emit/x64-chain, wasm wstruct/select/backend rows, hb-build-aot-test. Depends: none. Lands with the T0 stack, not alone: tools/native-unit-object.f (DEF-ADDR-REF) and src/habu/aot-shadow.f (SH-SCAN) read NEMIT:ADDR-* before the build window, so the installed engine cannot build it; it needs a staged host (built: .out/g1-staged/hb then .out/g2/hb), and the T0 stack needs one anyway, so one host transition and reinstall covers both. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0-l2a
