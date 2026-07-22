---
title: "Infer quant dispatch: end-to-end parity"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:42.937517+02:00"
blocks:
  - habu-infer-quant-dispatch-126a293a
  - habu-infer-dense-full-14833530
---

Why this exists:
Per-kernel parity does not prove that a full continuation preserves the pack's measured quality envelope across changing batch shapes.

Required result:
Run the pinned evaluation prompts through the complete quantized engine and compare logits, perplexity, fixed continuations, and terminal state with the published quality profile.

Done when:
Every prompt stays within its declared limits; repeated runs are identical; a deliberately wrong schedule or pack identity fails before generation.

Expected touch points: end-to-end quantized fixtures and canonical quality records.
Smallest check: the fixed-continuation quantized parity test.
Prerequisites: shape-keyed selection and the complete BF16 dense engine.
Owned result: end-to-end quantized correctness evidence only.
Claim: unassigned.
