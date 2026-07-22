---
title: "Infer quant: publish pack profile"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:41.087846+02:00"
blocks:
  - habu-infer-quant-quality-d94ce039
  - habu-infer-quant-bounded-1f9c9408
---

Why this exists:
A runtime must never load low-bit weights whose recipe, calibration, quality verdict, or kernel compatibility is missing or stale.

Required result:
Attach the validated recipe, source and output digests, calibration identity, quality results, and compatible kernel keys to one immutable model-pack profile and make pack publication depend on that complete profile.

Done when:
A valid profile reloads canonically; missing or mismatched evidence rejects before pack publication; changing any recipe or quality input changes the profile identity.

Expected touch points: model-pack profile publication and focused tests.
Smallest check: the focused complete-profile and stale-evidence test.
Prerequisites: quantization quality evaluator and bounded NVFP4 transform.
Owned result: quantized pack-profile publication only.
Claim: unassigned.
