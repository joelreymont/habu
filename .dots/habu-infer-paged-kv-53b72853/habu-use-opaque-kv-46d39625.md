---
title: Use opaque KV sequence IDs
status: closed
priority: 1
issue-type: task
created-at: "2026-07-29T21:43:29.427685+02:00"
closed-at: "2026-08-04T18:20:57.696695+02:00"
close-reason: "Retired: wide nested STRUCTURE fields retain the existing validated handle directly; the one-cell identity scan and owner-construction dependency add no product capability."
---

Problem: KV publicly exposes kvseq as a three-cell cache-id/slot/generation structure with MAKE and UNMAKE, and its tests persist those fields through raw columns. A scheduler cannot retain that value without reconstructing allocator authority. Result: hard-cut kvseq to a public copyable one-cell KV:seq nominal product with no type parameters and one private process-wide sequence identity. It is never an n. ALLOC-SEQ and FORK-SEQ are the only mints. Each cache row stores that identity; every KV operation resolves it against the supplied live cache and checks exact row identity, preserving the generation check internally so stale reuse and cross-cache handles reject before mutation. Resolution is one bounded scan over the configured sequence rows; add no public slot/generation fields, raw casts, hash index, second registry, compatibility structure, MAKE, or migration reader. Sequence-identity exhaustion rejects instead of wrapping. Owner: KV handle representation, row binding, and direct callers/tests only. Dependency: owner-only product publication. Production red: kv-cache-test.f currently UNMAKEs kvseq and later MAKEs it from stored raw fields. Acceptance: old KV-KVSEQ:MAKE/UNMAKE and the three public fields do not resolve; copied handles address the same live row; cross-cache, stale, reused, exhausted, and fabricated inputs reject; deleting the internal identity comparison makes KVT-STALE-REUSE and KVT-CROSS-CACHE fail; ALLOC/FORK/append/cancel behavior and bounded lookup pass with exact-capacity caches; INFER can store KV:seq as one typed cell without decomposition; focused KV, package, and exact-diff gates pass. Claim: unassigned.
