---
title: Delete tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.680038+02:00"
blocks:
  - habu-emit-wasm-from-0d5dab47
---

Problem: tier 1's compiler is gone: its elaborator and lowering, src/arch/*/passes.f and select*, set-tier and tier@, and every mention of tier 1 in src, tools, test and docs. src/compiler/ still holds 21 files, each loaded, directly or through another of them, by T0 (src/habu/t0*.f), the linker (src/habu/linker.f, src/arch/*/link*.f) or the build (tools/hb-build*.f): native/ abi.f, callback-stub.f, checker-owner.f, definition.f, dict.f, emission.f, frame.f, machine.f, publish.f, regfile.f, shadow.f; session/lease.f (shadow.f:415 calls NLEASE:IDLE-CK); target/model.f, target/native-binding.f; binding.f, digest.f, fixed-body.f, native-effect.f, numeric-policy.f, target.f, workspace.f. test/compiler/native-*.f run through T0 from test/gate-stdlib-cases.f. What remains of the acceptance: emission.f, which T0's emitters and the linker read, still lives at src/compiler/native/emission.f instead of src/habu/. Acceptance: emission.f moves to src/habu/ with its readers' requires; nothing under src/compiler/ is read only by deleted code. Files: src/compiler/native/emission.f and its readers. Verify: full suite on arm64, fixpoint, wasm rows. Depends: none. Ownership: those trees. Claim: unassigned.
