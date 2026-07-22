---
title: "Infer GEMM: activation quantization"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:42.092602+02:00"
blocks:
  - habu-infer-gemm-supported-538f1a45
---

Why this exists:
The selected NVFP4 tensor-core instruction requires a precisely defined activation conversion and scale lifetime.

Required result:
Quantize one bounded activation tile using the recipe's rounding and scale semantics, returning the packed tile and its owned scales without mutating the source.

Done when:
Boundary values and random tiles match an independent oracle; zero, non-finite, overflow, and partial-tile behavior is explicit; failure releases scratch and publishes no tile.

Expected touch points: activation quantization module and focused host and device tests.
Smallest check: the focused activation-tile parity test.
Prerequisites: supported small-batch geometry.
Owned result: activation-tile conversion and scale ownership only.
Claim: unassigned.
