---
title: "Infer M0 schema: workload coordinates"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T10:09:57.776583+02:00\""
---

Why this exists:
Benchmark samples need one engine-independent description of the work that was requested.

Required result:
Define and validate workload coordinates with workload identifier, prompt and output lengths, context, concurrency, sampling mode and parameters, cold or warm state, repetition ordinal, and optional cache-pressure target.

Done when:
Canonical coordinates round-trip; invalid lengths, concurrency, sampling combinations, repetition, and cold or warm state reject; no engine-specific flag enters the record.

Expected touch points: the workload-coordinate record and focused tests.
Smallest check: the focused coordinate boundary test.
Prerequisites: none.
Owned result: benchmark workload coordinates only.
Claim: agent=benchwork workspace=.jj-ws/habu-infer-m0-schema-170f7dc3 machine=spark.
