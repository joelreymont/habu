---
title: "Infer KV: atomic cancellation cleanup"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:38:16.941826+02:00"
blocks:
  - habu-infer-kv-declared-a0319bef
---

Why this exists:
cancellation and failed-prefill cleanup return several page and reservation owners; discovering corruption after the first return can leave an unretryable partial cleanup.

Required result:
preflight the complete sequence table and free-list capacity, then atomically retire the handle and return every reference and unused reservation.

Done when:
cancellation of empty, partial-tail, full-page, shared-prefix, and failed-prefill sequences returns exact ownership; injected invariant failures mutate nothing; stale and double cancellation reject structurally.

Expected touch points: maki/infer/kv-cache.f, maki/infer/kv-cache-test.f.
Smallest check: bin/hb --load maki/infer/kv-cache-test.f.
Prerequisites: declared maximum admission.
Owned result: cancellation and failed-prefill cleanup only.
Claim: unassigned.
