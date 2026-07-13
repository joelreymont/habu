---
title: Type asynchronous ARM64 machine state
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T15:56:44.912572+02:00"
blocks:
  - habu-define-typed-arm64-4ab8894f
---

Full context: docs/porting.md identifies crash and profiler signal handlers as target ABI boundaries, while the generated-machine-state program models ordinary callable labels, calls, syscalls, and returns. src/habu/crash.f, src/habu/prof.f, src/habu/habu2.f, and bootstrap/cg/forth.fs enter from kernel-defined register/ucontext state, read or mutate saved PC/SP/GPR fields, use signal-safe syscalls, and terminate through sigreturn or no-return exit. An ordinary BL contract cannot prove these entry registers, frame offsets, interrupted-state preservation, signal-safe call restrictions, reentrancy, or native/recovery target parity. Fix: add a target-indexed package-scoped asynchronous ABI effect schema for signal entry registers, typed ucontext fields, saved-state reads/writes, signal-safe operations, reentrancy/shared-state policy, sigreturn, and no-return exits; connect it to emitted-CFG verification without global prefixes. Acceptance: wrong target frame offset, wrong entry register, unmodeled saved-PC/SP write, unsafe helper call, non-signal-safe syscall, missing sigreturn/no-return terminator, reentrant scratch alias, and native/recovery drift reject; crash and profiler handlers certify for macOS/arm64 and Linux/aarch64; mutation fixtures plus target gate, bootstrap parity, fixpoint, typed-local lint, host/filemap lints, and full native gate pass. Files: new one-concern ARM64 async-effect package and tests, src/habu/crash.f, src/habu/prof.f, src/habu/habu2.f integration, bootstrap/cg/forth.fs parity, docs/porting.md. Depends on the typed ARM64 routine schema; coordinate emitted-CFG verification.
