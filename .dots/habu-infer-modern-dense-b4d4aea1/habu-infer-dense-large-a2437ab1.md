---
title: "Infer dense: large vocabulary head"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.457803+02:00"
blocks:
  - habu-infer-dense-tensor-c037a6fd
---

Why this exists:
the pinned model's vocabulary projection is large enough to require an explicit supported execution and workspace plan.

Required result:
bind the tied or untied head layout, execute it with landed GEMM/GEMV machinery, and expose one typed logit row.

Done when:
selected prompts match trusted logits within declared tolerance; geometry, workspace, and layout failures reject before launch.

Expected touch points: new maki/infer/dense-lm-head.f, focused test.
Smallest check: host and correctness-only device parity.
Prerequisites: tensor and config binding.
Owned result: vocabulary head only.
Claim: unassigned.
