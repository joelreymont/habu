---
title: "Infer M0 schema: metric payload"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-22T10:09:57.924710+02:00\""
---

Why this exists:
Latency, throughput, memory, page-fault, processor, and cache measurements need fixed units and explicit unavailable values.

Required result:
Define the raw metric payload with time to first token, inter-token latency, request latency, prefill and decode throughput, memory, page faults, processor use, cache bytes and pages, plus availability and measurement-window semantics.

Done when:
Every metric has one unit and type; unavailable is distinct from zero; negative, overflowing, conflicting, and incomplete payloads reject; boundary values round-trip canonically.

Expected touch points: the metric-payload record and focused tests.
Smallest check: the focused unit, availability, and boundary test.
Prerequisites: none.
Owned result: raw benchmark measurements and units only.
Claim: agent=benchmetric workspace=.jj-ws/habu-infer-m0-schema-5abd99f4 machine=spark.
