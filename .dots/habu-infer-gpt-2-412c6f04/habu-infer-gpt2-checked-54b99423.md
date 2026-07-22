---
title: "Infer GPT2: checked model geometry"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.837945+02:00"
blocks:
  - habu-infer-gpt2-tensor-f2ed655d
---

Why this exists:
the forward must not rely on scattered constants for layer count, width, heads, context, and vocabulary.

Required result:
validate the normalized GPT-2 configuration and derive every buffer and tensor extent with checked arithmetic.

Done when:
the pinned 12/768/12/50257 geometry passes; inconsistent head division, position limit, tensor extent, and overflow reject before allocation.

Expected touch points: maki/infer/gpt2.f or new maki/infer/gpt2-config.f, focused test.
Smallest check: bin/hb --load the focused geometry test.
Prerequisites: tensor role binding.
Owned result: GPT-2 geometry and extent derivation only.
Claim: unassigned.
