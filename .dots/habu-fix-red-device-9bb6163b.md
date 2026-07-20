---
title: Fix red-device-test arch setup (E-PTXTC-ARCH before RMSNORM)
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-20T13:04:58.896036+02:00\""
---

Pre-existing device-harness gap found by the affine-LayerNorm lane (ef4e8233) on the GB10: maki/lower/red-device-test.f fails at its (untouched) RMSNORM case with -3423 E-PTXTC-ARCH 'ASSEMBLE invoked before TC-ARCH! set' - the harness reaches ASSEMBLE without the arch probe having run, so device EXECUTION of the reduction-family lowerings (incl. the new affine LayerNorm forward) cannot be exercised on this box; only host-side PTX emission is proven (lower/red-test.f, in the gate). Fix: make the red-device harness establish TC-ARCH! (the auto-probe other device suites use - see maki/gpu.f and fusion-compare's self-emit pattern) before any ASSEMBLE, then run the suite on the GB10 and record which cases execute. Red-first: the current -3423 is the baseline. Territory: maki/lower/red-device-test.f harness preamble only - lowering emitters are proven and out of scope.

Claim: agent=reddev workspace=.jj-ws/fable-reddev machine=spark (owns maki/lower/red-device-test.f harness preamble; GPU device-suite runs, idle-check first)
