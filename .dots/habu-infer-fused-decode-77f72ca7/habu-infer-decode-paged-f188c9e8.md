---
title: "Infer decode: paged gather iterator"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.362349+02:00"
blocks:
  - habu-infer-decode-supported-29bebe81
  - habu-infer-kv-immutable-1ec13a88
---

Why this exists:
page-table traversal, logical-token order, tail bounds, and GQA head mapping must be correct independently of the transfer primitive.

Required result:
define a checked iterator that resolves logical token and KV head to one page base and in-page offset from an immutable device snapshot.

Done when:
contiguous and scattered synthetic layouts enumerate identical logical addresses; stale snapshot generation, missing page, tail overrun, and unsupported head mapping reject before launch.

Expected touch points: new lib/ptx/decode-paged-layout.f, focused test, FILEMAP.md.
Smallest check: focused layout test.
Prerequisites: supported geometry contract and immutable KV device snapshot.
Owned result: paged address calculation only.
Claim: unassigned.
