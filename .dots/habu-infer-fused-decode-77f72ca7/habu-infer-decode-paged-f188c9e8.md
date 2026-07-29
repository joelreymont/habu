---
title: "Infer decode: paged gather iterator"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.362349+02:00"
blocks:
  - habu-infer-decode-supported-29bebe81
  - habu-infer-kv-atomic-cdfb00cb
---

Why this exists:
page-table traversal, logical-token order, tail bounds, and GQA head mapping must be correct independently of the transfer primitive.

Required result:
define checked address calculation that resolves a batch row, GPT-2 layer, logical token, and head to one page identifier and in-page byte offset from DESCRIBE-BATCH output. It emits descriptor arithmetic for DECODE-CG; it is not a host iterator, persisted descriptor, snapshot generation, or second KV layout.

Done when:
contiguous and scattered synthetic layouts enumerate identical logical addresses; malformed row length, missing page, layer/head/token overrun, and unsupported head mapping reject before launch.

Expected touch points: new lib/ptx/decode-paged-layout.f and focused test.
Smallest check: focused layout test.
Prerequisites: supported geometry contract and the sole KV batch descriptor.
Owned result: paged address calculation only.
Claim: unassigned.
