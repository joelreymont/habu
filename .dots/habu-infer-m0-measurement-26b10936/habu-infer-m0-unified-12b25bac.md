---
title: "Infer M0: unified-memory counters"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:40:20.782980+02:00"
blocks:
  - habu-infer-m0-schema-27e8ca5d
---

Why this exists:
the M0 schema names peak unified memory, warmup page faults, and CPU utilization but no checked sampler owns their collection window.

Required result:
sample the relevant Linux and NVIDIA counters before, during, and after one engine run, distinguishing unavailable counters from zero.

Done when:
fixture traces produce exact peak/delta values; counter reset, wrap, malformed input, and permission denial have named outcomes; sampling overhead is measured.

Expected touch points: the system-counter sampler under tools/infer-bench/ and focused fixture tests.
Smallest check: the focused system-counter test.
Prerequisites: benchmark record schema.
Owned result: system counter sampling only.
Claim: unassigned.
