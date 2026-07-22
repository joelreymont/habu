---
title: "Infer quant: recipe schema"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:40.530555+02:00"
blocks:
  - habu-infer-pack-manifest-27c1030c
  - habu-infer-pack-tensor-93c2e949
---

Why this exists:
A quantized pack needs one versioned contract for block geometry, scale format, rounding, calibration identity, quality limits, and compatible kernels.

Required result:
Define and validate the quantization recipe carried by the model-pack manifest. It describes the source and packed data types, block axes and size, scale encoding, rounding rule, calibration digest, quality limits, and kernel compatibility keys.

Done when:
A canonical recipe round-trips; unknown versions, unsupported block geometry, missing calibration identity, invalid limits, and incompatible kernel keys reject before tensor conversion.

Expected touch points: the quantization recipe module, model-pack manifest integration, and focused tests.
Smallest check: the focused recipe round-trip and rejection test.
Prerequisites: model-pack manifest and tensor-layout catalog.
Owned result: quantization recipe data and validation only.
Claim: unassigned.
