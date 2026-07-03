---
title: "CAD 0a: structured report schema v1"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-04T00:20:01.901951+02:00\""
---

docs/model-cad.md Phase 0. maki/report.f + maki/report-test.f wired into maki/test.f. Report object: model name, shape/dtype/layout/target keys, fusion plan, materialized tensors, est bytes before/after, coalescing status, schedule candidates+selection, gate verdicts, profile rows, roofline class, artifact key, warnings/split reasons. Machine-readable render (agent-parseable, no prose scraping). Checked records behind representation-hiding constructor/accessor words (signatures must not leak layout) so internals swap to ADTs in cad-adt-swap. Fail-closed on unknown fields. No new trust rows.
