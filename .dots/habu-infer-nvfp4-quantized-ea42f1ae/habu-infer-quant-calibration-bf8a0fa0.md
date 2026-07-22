---
title: "Infer quant: calibration corpus pins"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:40.671548+02:00"
blocks:
  - habu-infer-dense-pin-36c8e45c
  - habu-infer-dense-tokenizer-a4453246
---

Why this exists:
Quality measurements are meaningless unless the checkpoint, tokenizer, prompts, token ranges, and reference outputs are immutable inputs.

Required result:
Define the calibration and validation corpus record for the pinned dense checkpoint, including checkpoint and tokenizer digests, prompt identities, token spans, reference continuation identities, and the split between calibration and evaluation data.

Done when:
The corpus validates without network access; any changed asset or overlapping calibration and evaluation row rejects; repeated loading produces identical ordered inputs.

Expected touch points: canonical corpus records, digest validation, and focused tests.
Smallest check: the focused corpus identity test.
Prerequisites: pinned dense checkpoint and tokenizer.
Owned result: calibration and evaluation input identity only.
Claim: unassigned.
