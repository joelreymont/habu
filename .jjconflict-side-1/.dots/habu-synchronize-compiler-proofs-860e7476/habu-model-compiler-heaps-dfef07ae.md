---
title: Model compiler heaps
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T14:02:57.047025+02:00"
blocks:
  - habu-bind-compiler-target-b3dfa307
---

Scope: define formal/Common/Memory.v for program-memory reasoning shared by HIR, SIR, LIR, native, and GPU stages. Model allocation identity, address space, typed contents, byte offset, extent, alignment, mutability, lifetime, and executable load/store/alloc/free semantics; volatile, atomic, fence, trap, and unknown external effects remain explicit. Acceptance: Rocq 9.2 builds; bounds, alignment, address-space, lifetime, and use-after-free examples decide correctly; no Admitted. Full prerequisites: habu-bind-compiler-target-b3dfa307 and habu-intern-compiler-types-bf952f0f. Ownership: formal/Common/Memory.v only. Excludes separation/frame theorems, arena implementation ownership, optimizer proofs, native/GPU lowering, and ID proofs.
