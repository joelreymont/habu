---
title: Migrate KV cache records
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T13:28:39.956654+02:00\""
---

Why: maki/infer/kv-cache.f is on the native inference critical path and still declares KV:kvseq and KV:kvcfg with legacy PRODUCT. Owner: maki/infer/kv-cache.f and maki/infer/kv-cache-test.f only. Replace both declarations directly with STRUCTURE inside the existing public KV package, preserving private nominal field roles, exact field names/schemas/order, KV-KVSEQ:MAKE/UNMAKE and KV-KVCFG:MAKE/UNMAKE spelling, three-cell and seven-cell layouts, cache identity/generation fail-closed semantics, reservation/accounting, allocation ownership, copy-on-write, errors, and public API. Update product comments. Forbidden: aliases, legacy parser edits, raw casts, handle/config redesign, lifecycle or allocator changes, copied test models, unrelated cleanup. Pre-change proof: token-aware production census finds exactly two executable PRODUCT declarations in this file. Acceptance: the real kv-cache test exercises both generated pairs plus initialization, admission, append, fork, cancellation, stale/cross-cache handles, capacity/overflow and accounting before/after; exact reflection/effects/layout remain stable; no executable PRODUCT remains; focused typed-local/package/trust and owning Maki gates pass.

Claim: agent=codex-kv-structure workspace=.jj-ws/habu-migrate-kv-cache-41c050c7
