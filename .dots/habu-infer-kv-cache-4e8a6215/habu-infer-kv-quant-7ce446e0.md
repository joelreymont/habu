---
title: "Infer KV quant: append conversion"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.793755+02:00"
blocks:
  - habu-infer-kv-quant-4e01b752
  - habu-infer-kv-atomic-cdfb00cb
---

Why this exists:
New key and value rows must enter compressed pages atomically with their scales and without a transient full-cache copy.

Required result:
Convert one append row into the selected cache profile and commit data, scales, length, and reservation consumption as one transition.

Done when:
Boundary values match an independent oracle; injected conversion or write failure leaves page bytes, scales, length, reservation, and snapshot generation unchanged; cancellation releases all scratch.

Expected touch points: compressed append path and focused transition tests.
Smallest check: the focused append failure-atomicity test.
Prerequisites: compressed capacity accounting and atomic key/value-cache append.
Owned result: one-row cache conversion and append transaction only.
Claim: unassigned.
