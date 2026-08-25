---
title: Index ARM64 operands and control effects
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T11:44:22.090200+02:00"
blocks:
  - habu-define-typed-arm64-4ab8894f
  - habu-lower-int-lir-20c79397
---

Compiler-IR reconciliation: this dot is the sole A64IR schema and instruction-selection owner. Compose the existing typed ARM64 routine-effect schema with indexed GPR, SP, SIMD, immediate, address, frame-slot, label, fixup, and control records, then select the Wave 2 integer LIR subset into validated A64IR. Do not create an IR around the old emitter, migrate broad legacy emitters in this leaf, allocate registers, or encode bytes. Acceptance: SQUARE-shaped LIR selects deterministically; wrong register class, SP/GPR, label/fixup, B/BL, immediate range, address space, effect, and target cases reject before allocation; the closed schema has canonical fixtures and a digest.

Claim: agent=a64index workspace=.jj-ws/habu-idx-arm64-operands-98280863 (RELEASED 2026-08-21: workspace gone, no live lane - gc)
