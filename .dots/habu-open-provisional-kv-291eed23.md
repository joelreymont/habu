---
title: Open provisional KV batch
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T06:31:31.208766+02:00"
blocks:
  - habu-infer-kv-declared-a0319bef
  - habu-own-device-kv-8e5bbf98
---

Why: a cache needs one linear carrier before rows can be staged without changing committed sequence lengths. Result: package KV declares public linear owner products batch and ready with no public constructors. batch binds the cache identity and provisional generation; ready binds those values plus the authenticated device-session generation, descriptor generation, bounded row count, and cache-owned fixed descriptor area used by the later transition. Every destructured field is non-authoritative. BEGIN-BATCH ( KV:cache -- KV:begin-result ) returns begun(cache,batch) or refused(cache,batch-error); it authenticates the cache, proves declared admission and one-provisional-batch exclusivity, and records no row. Owner: exact batch and ready declarations plus BEGIN-BATCH only. Dependencies: declared admission, opaque sequence identity, device KV ownership, and layer-aware KV storage. Production red: no cross-package provisional carrier exists. Acceptance: owner-product construction is package-only; begin succeeds once, concurrent or stale begin refuses with cache unchanged, and no committed length, page, reservation, or descriptor changes; foreign UNMAKE fields cannot construct either carrier. Forbidden: ADD, descriptor row type or operation, cancellation, DEVRT type, launch, commit, public constructor, compatibility, metric, or lint. Smallest owning check: real KV cache begin, duplicate-begin, and owner-product negative paths through maki/infer/kv-cache-test.f. Claim: unassigned.
