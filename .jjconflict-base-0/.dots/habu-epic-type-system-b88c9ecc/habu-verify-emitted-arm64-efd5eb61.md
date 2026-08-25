---
title: Verify emitted ARM64 CFG clobbers
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T11:44:22.028146+02:00"
blocks:
  - habu-define-typed-arm64-4ab8894f
---

Context: tools/lint/clobber-lint.f PASS2-DEF is source-linear and cannot prove path joins, loop-carried state, actual fixup targets, or all indirect-call behavior. Cause: liveness is checked over tokens instead of the emitted control-flow graph. Fix: build routine CFGs from actual ARM64 instructions plus icode labels and fixups, propagate typed routine effects transitively, and run path-sensitive liveness and stack-frame verification. Check GPR, SIMD, NZCV, live-ins, returns, callee-saved registers, LR, SP balance and alignment, frame slots, syscalls, BL, BLR, loops, and no-return edges. Unknown indirect calls fail closed unless a typed contract is supplied. Acceptance: reject live-in to call to read, one-arm-only restore, loop-carried clobber, unsaved LR, unbalanced or misaligned SP, callee-saved destruction, syscall scratch reuse, NZCV loss, SIMD clobber, and unknown BLR; current native and recovery emitters pass; source-token lint is retired or reduced to a reporter.
