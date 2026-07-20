---
title: Add unique bounded MEM byte borrow
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:44:00.233981+02:00"
blocks:
  - habu-define-rigid-host-71b010a0
  - habu-tfam-11b-open-ee9c72c6
---

Problem: raw allocated pointers permit unchecked aliasing, copy/drop, out-of-bounds byte access, free-while-live, and stale use. Fix: reopen package MEM in new lib/memory-region.f and implement linear owner<r> -> unique transient span<r,u8,e,unique,transient,g> -> owner<r>, with typed INDEX, C@, C!, ;BORROW, and consuming FREE. Use PRODUCT values with concrete linear tokens; do not edit lib/memory.f in this leaf. Acceptance: raw index, cross-region, extent/generation mismatch, owner/span copy or drop, free while borrowed, post-;BORROW span use, and later-generation index reuse reject; first/last byte access works; negative/index=len throw E-MEM-BOUNDS without modifying sentinels; allocation identity exhausts before reuse. Files: lib/memory-region.f, lib/memory-region-test.f, FILEMAP.md. Verify: exact test load, checker/linear/type-family suites, lib/memory-test.f, refine/trust/host/filemap/dot lints, typed-local diff lint, full native gate.

Edge note 2026-07-17: blocker habu-tfam-11-linear-99fa9990 closed (core
complete); edge repointed to its successor habu-tfam-11b-open-ee9c72c6
(the open-arg lift) conservatively - if this dot only needed the landed
TFAM-11 core, drop the edge at claim time.

BLOCKER FOUND 2026-07-21 (rigid-domains lane, reproduced on the UNMODIFIED engine): the checker cannot cleanly bind consumer type vars across 3+ co-resident fresh atoms in one family application, and a concrete argument between two fresh atoms breaks binding in some slot arrangements. This dot targets span<r,u8,e,unique,transient,g> = >=3 co-resident rigid identities on one owner, which hits the limit. Either model the owner with <=2 co-resident fresh atoms (early slots) or fix the underlying var-binding limitation first. The rigid-domains fixtures (test/rigid-region-suite.f) deliberately stay at <=2 per family for this reason.
