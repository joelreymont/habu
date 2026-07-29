---
title: "Infer ops: scrape snapshot"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:45.601423+02:00"
blocks:
  - habu-infer-ops-metric-fa3ebf5e
---

Why this exists:
Monitoring needs one immutable, bounded view that does not block or mutate the serving engine while it is rendered.

Required result:
Acquire one complete metric generation, render the simple scrape format in canonical order, and retire the snapshot after the client finishes or disconnects.

Done when:
Concurrent updates cannot mix generations; escaping and numeric formats are exact; slow or failed writes release the snapshot; rendering the same generation is byte-identical.

Expected touch points: metric snapshot and renderer with focused tests.
Smallest check: the focused concurrent-update rendering test.
Prerequisites: metric accumulator and client backpressure lease.
Owned result: scrape snapshot lifetime and rendering only.
Claim: unassigned.
