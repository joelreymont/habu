---
title: Lower HIR to x86_64 and emit it
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.542628+03:00"
---

Problem: docs/x86-64.md (campaign habu-campaign-c6-targets-86bb56bb): the compiler's target-free half (hir.f, hir-word.f, elaborate.f, loop.f) stops at a machine IR that only exists for ARM64 (a64ir.f, select.f, emit.f). Acceptance: x64ir.f, select-x64.f and emit-x64.f as a backend module that registers its lowering and emitter through the target registry when loaded, so bin/hb for arm64 does not carry it; calls between compiled words use the internal convention (stack cells in memory, live values in registers within a routine); relocatable literals are mov r64, imm64; the module compiles the compiler's own test programs from the arm64 host for the x86_64 contract, and the emitted bytes for a fixed set of words are pinned in a test. Files: src/compiler/native/x64ir.f, select-x64.f, emit-x64.f, src/compiler/target.f (registration), test/compiler/. Verify: the pinned-bytes test on arm64; the programs run on the Intel machine (next dot). Depends: the assembler, the primitive table, the register-file parameterisation, habu-bind-compiler-targets-ff970b99. Ownership: Joel (x86_64 lane). Claim: unassigned.
