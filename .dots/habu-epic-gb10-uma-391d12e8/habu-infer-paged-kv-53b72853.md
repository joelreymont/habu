---
title: "Infer: paged KV cache allocator"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-21T15:57:38.309268+02:00\""
blocks:
  - habu-infer-kv-atomic-cdfb00cb
  - habu-infer-kv-retryable-b548fcd2
  - habu-lease-kv-snapshot-9ef40f19
---

This is the paged key/value-cache campaign record. Do not dispatch it as implementation work. Commit 1835f711 landed the base allocator. The leaves now own fixed tables, declared maximum admission, atomic append, fork and cancellation, retryable disposal, exact metrics, immutable device publication, and measured page geometry.

The rejected broad remainder remains preserved in `.jj-ws/habu-infer-paged-kv-53b72853` as evidence only. It must be split and reviewed against the leaf contracts; no parent claim remains active.

Claim: unassigned.
