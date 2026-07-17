---
title: Share primitive bounds guards
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T14:17:36.899980+02:00\""
---

Stop-line structural size cut after the live-REPL cutover was disproven. Current final macOS emitted __text is 132576 bytes and bin/hb is 165367 bytes; primitives/base is 18276 bytes. Factor the five duplicated BFFI-GUARD-BOUNDS loops into one target subroutine calling existing LPROTSPAN, and route the remaining 17 duplicated GUARD-SPAN bodies through LPROTSPAN with exact ABI-preserving adapters and required FPRIM frames. Expected exact primitives/base cut is 5500 bytes before alignment. Preserve trap 83, register/stack ABI, FFI loop semantics, and primitive truth tables. Acceptance: targeted runtime negative/positive bounds regressions, fixpoint byte identity, emitted-region proof, native engine gates; no size-baseline update until bin/hb is <=100000 bytes.
