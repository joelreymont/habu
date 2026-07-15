---
title: Add unique bounded MEM byte borrow
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:44:00.233981+02:00"
blocks:
  - habu-define-rigid-host-71b010a0
  - habu-tfam-11-linear-99fa9990
---

Problem: raw allocated pointers permit unchecked aliasing, copy/drop, out-of-bounds byte access, free-while-live, and stale use. Fix: reopen package MEM in new lib/memory-region.f and implement linear owner<r> -> unique transient span<r,u8,e,unique,transient,g> -> owner<r>, with typed INDEX, C@, C!, ;BORROW, and consuming FREE. Use PRODUCT values with concrete linear tokens; do not edit lib/memory.f in this leaf. Acceptance: raw index, cross-region, extent/generation mismatch, owner/span copy or drop, free while borrowed, post-;BORROW span use, and later-generation index reuse reject; first/last byte access works; negative/index=len throw E-MEM-BOUNDS without modifying sentinels; allocation identity exhausts before reuse. Files: lib/memory-region.f, lib/memory-region-test.f, FILEMAP.md. Verify: exact test load, checker/linear/type-family suites, lib/memory-test.f, refine/trust/host/filemap/dot lints, typed-local diff lint, full native gate.
