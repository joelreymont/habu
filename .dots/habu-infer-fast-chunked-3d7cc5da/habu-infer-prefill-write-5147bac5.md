---
title: "Infer prefill: write shared KV layout"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.733556+02:00"
blocks:
  - habu-infer-prefill-tiled-1ab7cf32
  - habu-infer-kv-atomic-cdfb00cb
---

Why this exists:
prefill and decode must use the same physical KV page layout without a conversion copy.

Required result:
write prefill K and V outputs directly into admitted pages through one checked page writer.

Done when:
the resulting snapshot is byte-equivalent to token-by-token append for fixed prompts; boundary, tail, cancellation, and failed-kernel cleanup are exact.

Expected touch points: new maki/infer/prefill-kv.f, focused test.
Smallest check: focused layout equivalence and cleanup test.
Prerequisites: tiled attention kernel and atomic KV append.
Owned result: prefill-to-KV writer only.
Claim: unassigned.
