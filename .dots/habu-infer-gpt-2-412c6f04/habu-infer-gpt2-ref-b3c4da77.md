---
title: "Infer GPT2: reference fixture provenance"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.853809+02:00"
---

The tensor identity and checkpoint-preparation prerequisites are done. Commits
1746455e and 5756381d introduced the tensor vocabulary now named GPT2TENSOR
and PREPARE, respectively. Both are ancestors of master, so the obsolete
blocker edge to habu-infer-gpt2-tensor-f2ed655d is removed.

Why this exists:
parity requires a reproducible external reference tied to the exact checkpoint and prompt inputs, not an opaque blob.

Required result:
commit the smallest logits and internal-checkpoint fixture with generator identity, checkpoint digest, command, shapes, and precision provenance; validate the fixture before tests consume it.

Done when:
regeneration from the pinned environment is byte-identical or numerically equivalent under an explicit serialization rule; altered digest or shape rejects.

Expected touch points: GPT-2 reference fixture, provenance record, Habu fixture validator.
Smallest check: validator plus regeneration comparison.
Prerequisites: tensor identity and checkpoint preparation.
Owned result: external reference generation record and committed fixture only.
Claim: unassigned.
