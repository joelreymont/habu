---
title: "Infer GPT2: reference fixture provenance"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.853809+02:00"
blocks:
  - habu-infer-gpt2-tensor-f2ed655d
---

Why this exists:
parity requires a reproducible external reference tied to the exact checkpoint and prompt inputs, not an opaque blob.

Required result:
commit the smallest logits and internal-checkpoint fixture with generator identity, checkpoint digest, command, shapes, and precision provenance; validate the fixture before tests consume it.

Done when:
regeneration from the pinned environment is byte-identical or numerically equivalent under an explicit serialization rule; altered digest or shape rejects.

Expected touch points: GPT-2 reference fixture, provenance record, Habu fixture validator.
Smallest check: validator plus regeneration comparison.
Prerequisites: tensor role binding.
Owned result: external reference generation record and committed fixture only.
Claim: unassigned.
