---
title: Add pinned engine and inference benchmarks
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T18:57:31.174620+02:00"
---

Follow-up to the perf-verdict retirement (Joel ruling: individual sha-pinned benchmarks only, each with its own calibrated budget; totals are never gates). The gate perf coverage after retirement is the six confined JSON benchmarks; this dot restores breadth the right way: one confined benchmark each for the checker (certify a pinned candidate set), the compiler fixpoint slice (compile a pinned module), the engine interpreter (a pinned compute kernel), and - when the forward pass lands - tokens-per-second on the pinned GPT-2 config, each in its own quiescent fork with median-of-3, its own base constant with provenance (value, margin, date, machine, parent commit), following the lib/json-read-perf-test.f + test/json-read-perf-phase.f architecture exactly. No aggregates. Acceptance: each benchmark red under its own slowed-workload mutation, green at calibration, inadmissible under the synthetic-load fixture; registration per convention. Owner: the json-read-perf-phase architecture generalized into shared perf-phase support if a second consumer wants it - the lift-into-shared-support rule applies at the second copy, not before. Dependencies: after the retirement lands. Priority 2.
