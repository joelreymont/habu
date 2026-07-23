---
title: "Infer KV: declared maximum admission"
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-22T09:38:16.924879+02:00\""
closed-at: "2026-07-23T14:42:51.561684+02:00"
close-reason: landed as e6837d7c and 0d2a0a02; root hunk review and fresh destruction review accepted; focused production suite and six mutation checks pass on master@origin 6f9be075
blocks:
  - habu-infer-kv-fixed-a219f7ba
---

Why this exists:
maki/infer/kv-cache.f can allocate a sequence without reserving its declared future KV growth, so an admitted request can become impossible to complete.

Required result:
require maximum context at admission, reserve its exact remaining page count, and bind the reservation to the generation-bearing sequence handle.

Done when:
exactly fitting admission succeeds, one-page-over rejects before slot publication, append consumes one reserved page at each boundary, cancellation returns the unused reservation, and stale handles cannot spend it.

Expected touch points: maki/infer/kv-cache.f, maki/infer/kv-cache-test.f.
Smallest check: bin/hb --load maki/infer/kv-cache-test.f.
Prerequisites: fixed block-table geometry.
Owned result: sequence admission and reservation ledger only.
Claim: agent=kvdecl workspace=.jj-ws/habu-infer-kv-declared-a0319bef
machine=spark.
