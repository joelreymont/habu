---
title: "Infer KV: retryable cache disposal"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:38:16.947643+02:00"
blocks:
  - habu-infer-kv-atomic-c402952e
---

Why this exists:
cache disposal owns independent pool and metadata mappings, but throwing release can lose the remaining owner or double-unmap an already released mapping.

Required result:
use MEM:RELEASE-BYTES-RC for both mappings, clear only the owner whose release succeeded, and return a typed result that preserves every failed owner for retry.

Done when:
pool-only, metadata-only, both-fail, and second-attempt cases release each mapping exactly once; live sequence owners are retired before mapping release; successful disposal leaves an empty cache.

Expected touch points: maki/infer/kv-cache.f, maki/infer/kv-cache-test.f.
Smallest check: bin/hb --load maki/infer/kv-cache-test.f plus mmap/munmap syscall trace.
Prerequisites: atomic cancellation cleanup.
Owned result: cache disposal state machine only.
Claim: unassigned.
