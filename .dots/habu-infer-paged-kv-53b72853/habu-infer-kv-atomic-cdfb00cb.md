---
title: "Infer KV: atomic append transition"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T09:38:16.930498+02:00\""
blocks:
  - habu-infer-kv-declared-a0319bef
---

Why this exists:
page-boundary append and copy-on-write tail append perform several fallible checks and ownership mutations; a late failure can leave length, reservations, references, or free pages inconsistent.

Required result:
preflight every fallible condition, then commit an infallible append transition.

Done when:
injected failures at every preflight point leave the cache byte-for-byte invariant, a successful boundary append allocates exactly one page and consumes exactly one reservation, and copy-on-write changes only the mutable tail.

Expected touch points: maki/infer/kv-cache.f, maki/infer/kv-cache-test.f.
Smallest check: bin/hb --load maki/infer/kv-cache-test.f.
Prerequisites: declared maximum admission.
Owned result: append transition only.
Claim: agent=kv_atomic_append workspace=.jj-ws/habu-infer-kv-atomic-cdfb00cb.
