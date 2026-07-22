---
title: "Infer GPT2: full host logits"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.848634+02:00"
blocks:
  - habu-infer-gpt2-one-1ace1023
---

Why this exists:
the GPT-2 oracle needs embeddings, twelve blocks, final normalization, and the tied vocabulary head joined into a complete forward.

Required result:
compose the checked block repeatedly using bounded reusable workspaces and emit the final logit row.

Done when:
fixed prompts match selected layer checkpoints and final logits against the committed reference within the declared host tolerance; run twice is identical; no per-layer owner leaks.

Expected touch points: maki/infer/gpt2.f, focused full-forward test.
Smallest check: focused GPT-2 full-forward parity test.
Prerequisites: one transformer block.
Owned result: full host forward composition and workspace lifetime only.
Claim: unassigned.
