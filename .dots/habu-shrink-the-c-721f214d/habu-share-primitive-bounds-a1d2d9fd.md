---
title: Share primitive bounds guards
status: active
priority: 2
issue-type: task
created-at: "2026-07-17T14:17:36.899980+02:00"
---

Stop-line structural cut for proven duplication. Baseline master: __text=132576 and primitives/base=18276. Commit cf9fab59 made LPROTSPAN an AOT-closed private helper and routed ! through it, leaving __text=132392 and primitives/base=18044. Factor the five duplicated BFFI-GUARD-BOUNDS loops into one target subroutine calling LPROTSPAN, and route the remaining 16 duplicated GUARD-SPAN bodies through LPROTSPAN with exact ABI-preserving adapters and required FPRIM frames. Residual expected cut is 5268 bytes in primitives/base, reaching <=12776; measure the actual whole-__text delta rather than inferring it from the Mach-O page floor. Preserve trap 83, register/stack ABI, FFI loop semantics, primitive truth tables, AOT closure, and private-helper invisibility. Acceptance: targeted runtime negative/positive bounds regressions, fixpoint byte identity, emitted-region proof, AOT positive/negative gates, native engine gates; lower the exact platform baseline if and only if the final artifact shrinks; parent closure still requires immutable __text <=110592 and platform ceilings.
