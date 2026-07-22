---
title: "Infer M0 schema: canonical record codec"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:10:05.164160+02:00"
blocks:
  - habu-infer-m0-schema-779b18d6
  - habu-infer-m0-schema-170f7dc3
  - habu-infer-m0-schema-5abd99f4
---

Why this exists:
Comparable benchmark data needs one canonical record that joins producer identity, workload coordinates, raw metrics, failures, and aggregate metadata.

Required result:
Compose the three validated record families into the versioned benchmark record and provide deterministic encoding, decoding, and cross-field consistency checks.

Done when:
Complete records encode byte-identically and round-trip; missing, duplicate, ill-typed, unknown-version, mixed-identity, inconsistent length or run-count, and success-with-failure records reject by name.

Expected touch points: the canonical benchmark codec and focused integration tests.
Smallest check: the focused canonical round-trip and cross-field rejection test.
Prerequisites: run identity, workload coordinates, and metric payload.
Owned result: canonical benchmark record composition and codec only.
Claim: unassigned.
