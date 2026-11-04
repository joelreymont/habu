---
title: Move T0 into COMP
status: closed
priority: 2
issue-type: task
created-at: "2026-11-02T13:47:08.633822+02:00"
closed-at: "2026-11-02T16:13:36.727519+02:00"
close-reason: "landed: T0 is package COMP with parts LABEL, ARM64, X86-64; COMP:INSTALL only public; full arm64 suite 514/514, chain gen 4=5; x86 rows match baseline"
blocks:
  - habu-resolve-a-qualified-30078ff4
---

Problem: T0's five files are five public packages (T0, T0-EMIT, T0-LABEL, T0-A64, T0-X64) where card section 1 wants one package with private words; Joel 2026-11-02 (LESSONS.md, Compiler and builds) chose 'Approve the move'. Acceptance: src/habu/t0.f and t0-emit.f become package COMP; the emitter seam's public words T0-EMIT:X become private EMIT-X; t0-label.f, t0-arm64.f and t0-x64.f become COMP's parts LABEL, ARM64 and X86-64; COMP:INSTALL replaces T0:INSTALL and selects the host architecture itself, so runtime-arm64.f and runtime-x64.f call COMP:INSTALL alone; every other public word goes private; comments naming T0:, T0-EMIT: and T0-LABEL: are fixed (layout.f:919, native-build-core.f:64, errors.f:1242, 1246, the five files). Approved public changes: COMP:INSTALL replaces T0:INSTALL; any other public word needs Joel's approval through the interview. Behaviour and host bytes unchanged. Files: src/habu/t0.f, t0-emit.f, t0-label.f, t0-arm64.f, t0-x64.f, runtime-arm64.f, runtime-x64.f, layout.f, tools/native-build-core.f, src/lib/errors.f. Verify: candidate build; rows tier, outer-interpret, cold-naming, compiler-native-create-does, compiler-compile-floor-gate and the tier0-* rows; x86 cross-build; then arm64 full suite and generation chain at landing. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0dual (change snsnlrtq).
