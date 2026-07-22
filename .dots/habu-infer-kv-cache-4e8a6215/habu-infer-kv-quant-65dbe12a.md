---
title: "Infer KV quant: long-context quality"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:45.087024+02:00"
blocks:
  - habu-infer-kv-quant-e60a2e42
  - habu-infer-quant-calibration-bf8a0fa0
---

Why this exists:
Short kernel parity cannot justify compressed cache for long contexts where error compounds across many decode steps.

Required result:
Run the pinned long-context corpus through BF16 and each supported cache profile, recording logit error, perplexity delta, fixed continuations, and failure positions.

Done when:
Repeated runs are identical; damaged scales fail; every supported profile stays within its declared limit over the full context range or receives an explicit rejection verdict.

Expected touch points: canonical long-context quality records and focused evaluator fixtures.
Smallest check: quality-record validation and the damaged-scale negative.
Prerequisites: compressed attention read path and calibration corpus.
Owned result: long-context cache-quality evidence only.
Claim: unassigned.
