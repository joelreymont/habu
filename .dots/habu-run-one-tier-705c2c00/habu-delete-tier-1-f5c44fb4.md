---
title: Delete tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.680038+02:00"
blocks:
  - habu-cross-compile-through-66516afb
  - habu-delete-the-arm64-b319376e
  - habu-emit-wasm-from-0d5dab47
---

Problem: after L6, L8, L9 tier 1 has no reader. Acceptance: delete src/compiler/** except dict.f, string.f, workspace.f, checker-owner.f, fetch*.f, and emission.f moved to src/habu/; src/arch/*/passes.f, select*; test/compiler/native-* per t0-open-items.md section 6; docs updated. Files: src/compiler/**, src/arch/**, test/compiler/**, docs. Verify: full suite on arm64 and x86 peer, fixpoint, wasm rows. Depends: L6, L8, L9. Ownership: those trees. Claim: unassigned.
