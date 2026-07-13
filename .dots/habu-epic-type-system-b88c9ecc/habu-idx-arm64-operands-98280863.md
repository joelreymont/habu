---
title: Index ARM64 operands and control effects
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T11:44:22.090200+02:00"
blocks:
  - habu-define-typed-arm64-4ab8894f
---

Context: src/arch/arm64/mnem.f exposes most operands as n and coarse reg or label roles still accept semantic corruptions such as using SP where a GPR result is required, using the wrong frame-slot base, or substituting a terminal B for a BL call. tools/codegen-role.f documents and structurally catches only pinned examples after compilation. Cause: physical register class, slot identity, fixup kind, and control behavior are not indexed in the type. Fix: introduce a package-scoped typed instruction IR with indexed operand families for GPR, SP, SIMD, immediate and shift ranges, address spaces, frame slots, labels and fixup kinds, plus control effects for fallthrough, branch, call, return, and no-return. Migrate asm, icode, mnem, and emitted helper APIs without raw global prefixes. Acceptance: the existing CLOC-MAIN and spawn-slot corruptions fail at CHECK!, plus SP and GPR, label and fixup, B and BL, immediate-range, and address-space negatives; valid native and recovery source certifies; structural codegen-role decoding remains defense in depth.
