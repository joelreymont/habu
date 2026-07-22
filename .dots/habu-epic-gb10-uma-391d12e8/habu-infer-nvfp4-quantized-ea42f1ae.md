---
title: "Infer: NVFP4 quantized decode"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T15:59:04.567006+02:00"
blocks:
  - habu-infer-quant-publish-1457f90e
---

This is the offline weight-quantization campaign record. Do not dispatch it as implementation work. Its leaves define the recipe, pin calibration and evaluation inputs, convert tensors with bounded memory, measure model quality, and publish one complete quantized pack profile. The campaign closes when the profile is immutable and all quality evidence validates.
