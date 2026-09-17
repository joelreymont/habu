---
title: Parameterise the allocator by a register file
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.540795+03:00"
---

Problem: docs/x86-64.md (campaign habu-campaign-c6-targets-86bb56bb): src/compiler/native/regalloc.f, spill.f and prune.f name ARM64 registers and forms directly, so the x86_64 backend (16 registers, rbp r12 r13 r14 r15 rbx reserved for the VM) could not reuse them without forking. Acceptance: a register-file description (allocatable set, reserved VM registers, caller and callee saved sets, scratch registers, spill slot width) that the allocator, spill and prune read instead of ARM64 constants; the arm64 backend supplies its description and compiles to the same bytes as before (byte fixpoint proves it); a second description with 10 allocatable registers runs the allocator's own tests. Files: src/compiler/native/regalloc.f, spill.f, prune.f, a64ir.f (the arm64 description), test/compiler/. Verify: test/compiler suites; byte fixpoint; test/run.f. Depends: none. Ownership: hazel runs it on the arm64 host. Claim: agent=hazel-regfile workspace=.jj-ws/hazel-regfile.
