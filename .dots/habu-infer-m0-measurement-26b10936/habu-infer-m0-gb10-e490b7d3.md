---
title: "Infer M0: GB10 hardware manifest"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.761617+02:00"
blocks:
  - habu-infer-m0-schema-27e8ca5d
---

Why this exists:
benchmark results are meaningless without exact DGX OS, kernel, driver, CUDA, ptxas, GPU, memory, power, and clock identity.

Required result:
collect those facts from the operating system and NVIDIA tooling into the M0 schema with explicit unavailable results.

Done when:
a live manifest validates on this DGX Spark; injected missing commands and malformed outputs fail or record unavailability as specified.

Expected touch points: the hardware-manifest collector under tools/infer-bench/ and focused fixture and live tests.
Smallest check: the focused manifest fixtures and one live manifest validation.
Prerequisites: benchmark record schema.
Owned result: hardware manifest collection only.
Claim: unassigned.
