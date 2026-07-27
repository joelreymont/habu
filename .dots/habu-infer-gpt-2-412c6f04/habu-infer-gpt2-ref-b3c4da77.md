---
title: "Infer GPT2: reference fixture provenance"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.853809+02:00"
---

The tensor role binding prerequisite is done and its dot is closed. It was
habu-infer-gpt2-tensor-f2ed655d, superseded by 1746455e "Add GPT-2 tensor
vocabulary (GPT2BIND)" and 5756381d "Add GPT2TX bind PREPARE and prep
capability", both ancestors of master, so its blocker edge is removed here.
Where the text below says "Prerequisites: tensor role binding", read those two
commits.

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
