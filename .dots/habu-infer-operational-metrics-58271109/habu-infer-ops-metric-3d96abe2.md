---
title: "Infer ops: metric schema"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:45.352916+02:00"
blocks:
  - habu-infer-m0-schema-27e8ca5d
---

Why this exists:
Serving and soak evidence need stable names, units, labels, and counter types before code starts recording values.

Required result:
Define the versioned operational metric catalog for queue, active requests, prefill and decode tokens, latency, cache use, reserve, waits and rejects, page faults, kernel and schedule identity, and pack identity.

Done when:
Every metric has one type and unit; duplicate names, unknown labels, invalid counter kinds, and missing identity fields reject; the catalog round-trips canonically.

Expected touch points: operational metric schema and focused tests.
Smallest check: the focused catalog round-trip and conflict test.
Prerequisites: M0 benchmark schema and scheduler wait-reason catalog.
Owned result: metric names, types, units, and labels only.
Claim: unassigned.
