---
title: "Infer M0: workload matrix"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.766852+02:00"
blocks:
  - habu-infer-m0-schema-27e8ca5d
---

Why this exists:
the six required workload cells need one canonical definition so every engine receives the same prompts, lengths, concurrency, sampling, warmup, and repetition count.

Required result:
define the interactive-short, coding and retrieval-augmented generation 4K, long-prompt 32K, mixed concurrency four, KV-pressure, and soak workload cells as validated data.

Done when:
every cell round-trips canonically; duplicate identifiers, invalid lengths, impossible concurrency, missing prompts, and zero repetitions reject by name; the matrix contains no engine-specific flags.

Expected touch points: the workload matrix under tools/infer-bench/ and its focused test.
Smallest check: the focused workload-matrix test.
Prerequisites: benchmark record schema.
Owned result: canonical workload-cell definitions and validation only.
Claim: unassigned.
