---
title: "Infer KV quant: quality profile"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.510336+02:00"
blocks:
  - habu-infer-dense-full-14833530
  - habu-infer-pack-manifest-27c1030c
---

Why this exists:
Key/value-cache compression needs an explicit user-selected precision and quality contract separate from weight quantization.

Required result:
Define a versioned key/value-cache profile with data type, scale granularity, supported attention kernels, capacity accounting key, model identity, and measured quality limits.

Done when:
Canonical profiles round-trip; unknown precision, incompatible model or kernel, missing quality evidence, and conflicting scale geometry reject before cache creation.

Expected touch points: key/value-cache profile schema and focused tests.
Smallest check: the focused profile round-trip and incompatibility test.
Prerequisites: full BF16 dense-model parity and model-pack manifest.
Owned result: key/value-cache quality profile data only.
Claim: unassigned.
