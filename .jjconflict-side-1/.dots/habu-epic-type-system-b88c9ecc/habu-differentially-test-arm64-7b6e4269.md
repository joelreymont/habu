---
title: Differentially test ARM64 machine effects
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T11:44:22.345658+02:00"
blocks:
  - habu-verify-emitted-arm64-efd5eb61
---

Context: typed routine contracts and CFG verification remain axioms unless observed generated-machine behavior is compared with predictions. Fix: generate bounded deterministic ARM64 routines across every instruction and call-contract row, execute them under captured initial GPR, SIMD, NZCV, SP, and frame state, and compare observed reads, writes, returns, preserves, stack delta, and control outcome with the verifier model. Use checked Habu generators and reducers; mutation of an opcode row, routine contract, branch arm, or syscall effect must fail. Acceptance: every schema row has coverage, seeds and artifacts are content-keyed and reproducible, property failures are fatal and minimized, native and recovery encoders agree, and the owning gate stays within a measured budget.
