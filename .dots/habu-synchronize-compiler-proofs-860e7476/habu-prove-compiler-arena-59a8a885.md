---
title: Prove compiler arena ownership
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T14:03:12.045861+02:00"
blocks:
  - habu-prove-compiler-separation-db458ea0
  - habu-own-compiler-ir-1e8e0bec
  - habu-freeze-compiler-ir-6f706100
---

Scope: define formal/Common/Arena.v and bind the executable compiler context, builder, arena, mark, growth, abort, freeze, promotion, and release lifecycle to the separation resource model. Acceptance: Rocq 9.2 proves sole mutable ownership, growth preserves published IDs, abort/release consume resources exactly once, frozen modules are read-only/shareable, and leak, use-after-free, double release, cross-context access, and frozen mutation are unreachable or rejected; executable hostile lifecycle vectors agree; no Admitted. Full prerequisites: habu-prove-compiler-separation-db458ea0, habu-own-compiler-ir-1e8e0bec, and habu-freeze-compiler-ir-6f706100. Ownership: formal/Common/Arena.v plus arena-proof parity fixtures only. Excludes source-language heap semantics, optimizer-pass theorems, native/GPU lowering, and ID proofs.
