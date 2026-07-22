---
title: "Infer dense: grouped-query decode integration"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.451991+02:00"
blocks:
  - habu-infer-dense-host-4c9152ad
  - habu-infer-decode-paged-66b6a16d
---

Why this exists:
the paged decode family must prove n_kv_heads fewer than n_heads on real model tensors, not only synthetic geometry.

Required result:
integrate the pinned model's query-to-KV-head mapping and RoPE positions with the selected paged kernel.

Done when:
multiple decode positions match the host block attention output and greedy next identifier; invalid head mapping rejects before launch.

Expected touch points: focused dense GQA device integration test and minimal glue.
Smallest check: correctness-only GB10 parity run.
Prerequisites: host reference block and paged real-model parity.
Owned result: real-model GQA decode integration only.
Claim: unassigned.
