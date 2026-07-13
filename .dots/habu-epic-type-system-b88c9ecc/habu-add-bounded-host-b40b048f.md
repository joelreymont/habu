---
title: Add bounded host region and lifetime spans
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T11:44:22.152615+02:00"
blocks:
  - habu-checker-capability-ptr-113a95e9
---

Context: ptr T preserves a pointee but carries no allocation identity, extent, alignment, lifetime, mutability, or transient versus persistent class. AOT capture, snapshot, mmap, process, and scratch buffers can therefore alias, escape, overrun, or persist dangling addresses without a checker mismatch. Fix: generalize the existing type-family span approach to host memory with span<region,type,extent,access,persistence>, fresh generative region identities, checked bounded indices and subspans, alignment evidence, linear owners, read and unique mutable borrows, and explicit FREEZE or owned-copy conversion from transient to persistent storage. Acceptance: reject raw or mismatched-extent indexing, cross-region unification, overlapping mutable borrows, use after owner consumption, escaped local borrow, and transient pointer serialization; prove bounded subspan access and persistent freeze round trips; migrate one AOT buffer, one mmap buffer, and one snapshot record path.
