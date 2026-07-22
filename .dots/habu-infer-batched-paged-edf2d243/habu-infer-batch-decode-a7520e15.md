---
title: "Infer batch decode: ragged descriptor"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.756554+02:00"
blocks:
  - habu-infer-kv-snapshot-1cdc055a
---

Why this exists:
small-batch decode needs one bounded device descriptor for per-sequence snapshot generation, length, table span, output slot, and completion mask.

Required result:
build and validate an immutable ragged batch descriptor before launch.

Done when:
mixed lengths and table sizes serialize deterministically; duplicate output slot, stale generation, over-bound batch, and invalid mask reject.

Expected touch points: new maki/infer/decode-batch.f, focused test.
Smallest check: focused descriptor test.
Prerequisites: KV snapshot handshake.
Owned result: batch descriptor only.
Claim: unassigned.
