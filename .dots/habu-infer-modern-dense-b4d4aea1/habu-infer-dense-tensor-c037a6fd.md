---
title: "Infer dense: tensor and config binding"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.441007+02:00"
blocks:
  - habu-infer-dense-pin-36c8e45c
  - habu-infer-pack-tensor-93c2e949
---

Why this exists:
normalized configuration and pack layout must bind the pinned checkpoint's embeddings, norms, Q/K/V/O, SwiGLU, and vocabulary head exactly once.

Required result:
define the model-family tensor-role table and validate layer counts, GQA geometry, RoPE parameters, and packed layouts before publication.

Done when:
pinned pack binds exactly; missing, duplicate, wrong-shape, wrong-dtype, and incompatible RoPE/GQA fields reject named.

Expected touch points: new maki/infer/dense-model.f, focused binding test.
Smallest check: focused model binding test.
Prerequisites: pin product checkpoint, model-pack tensor layout catalog.
Owned result: modern-model config and tensor binding only.
Claim: unassigned.
