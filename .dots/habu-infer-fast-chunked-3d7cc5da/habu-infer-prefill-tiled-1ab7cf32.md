---
title: "Infer prefill: tiled attention kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.727525+02:00"
blocks:
  - habu-infer-prefill-supported-d3aaf0ce
  - habu-infer-dense-host-4c9152ad
---

Why this exists:
composed decode-style attention is too slow for long prompts.

Required result:
implement the selected contiguous or tiled causal attention path using existing attention machinery under the prefill geometry contract.

Done when:
outputs match the modern host block at representative lengths; unsupported geometry fails before launch; perf-watch registration is complete.

Expected touch points: new or focused lib/ptx prefill emitter, device test, perf-watch/FILEMAP rows.
Smallest check: correctness-only GB10 parity.
Prerequisites: supported prefill geometry and modern host block.
Owned result: fast prefill attention kernel only.
Claim: unassigned.
