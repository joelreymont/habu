---
title: "Infer quant: bounded NVFP4 tensor transform"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:40.806831+02:00"
blocks:
  - habu-infer-quant-recipe-f2474b3b
  - habu-infer-pack-bounded-7106d353
---

Why this exists:
Packing must produce final NVFP4 tensor bytes and scales without holding source and destination models in memory at once.

Required result:
Convert one bounded source tensor chunk into the declared final layout, emitting NVFP4 values, scale metadata, checksum input, and measured reconstruction-error statistics under a validated recipe.

Done when:
Boundary chunk sizes match an independent oracle; the configured scratch bound is never exceeded; malformed shapes and injected read, transform, or write failures publish no member and release every mapping.

Expected touch points: the model-pack tensor transform and focused fixture tests.
Smallest check: the focused chunk-boundary and failure-atomicity test.
Prerequisites: quantization recipe and bounded model-pack writer.
Owned result: one-tensor NVFP4 conversion and its bounded scratch lifetime only.
Claim: unassigned.
