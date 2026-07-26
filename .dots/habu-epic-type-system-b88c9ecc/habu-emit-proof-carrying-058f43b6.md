---
title: Emit proof-carrying register allocation certificates
status: open
priority: 3
issue-type: task
created-at: "2026-07-13T11:44:22.408382+02:00"
blocks:
  - habu-allocate-straight-line-bc4e0075
---

Compiler-IR reconciliation: this dot owns the native register-allocation witness and independent validator required by design sections 7.9 and 10.2. Bind source/checker manifest, input A64IR, target, live ranges, assignments, spills/frame slots, call clobbers, SP facts, output, pass/schema versions, and payload digest. The producer and validator are separate packages; GPU witnesses remain with GPU stage owners. Acceptance: the Wave 2 allocation validates, while mutations to any binding, range, overlap, register, spill slot, call effect, SP fact, or digest reject before layout/encoding or promotion.
