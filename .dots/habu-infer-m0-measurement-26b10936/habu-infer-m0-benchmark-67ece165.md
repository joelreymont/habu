---
title: "Infer M0: benchmark reducer"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:00:35.640340+02:00"
blocks:
  - habu-infer-m0-schema-27e8ca5d
---

Why this exists:
Raw repetitions need one deterministic reducer so baseline and Habu results use identical statistics and failed runs remain visible.

Required result:
Reduce a complete set of raw run records into minimum, median, and 95th-percentile metrics while preserving cold and warm groups, run counts, failures, and raw-log identities.

Done when:
Boundary-sized fixtures prove ordering, even and odd medians, the declared percentile rule, cold and warm separation, exact run counts, deterministic output, and rejection of incomplete or mixed-identity inputs.

Expected touch points: the benchmark reducer under tools/infer-bench/ and focused tests.
Smallest check: the focused reducer test.
Prerequisites: benchmark record schema.
Owned result: statistical reduction and aggregate record validation only.
Claim: unassigned.
