---
title: "Core variants: prove zero census"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:46:13.909391+02:00"
blocks:
  - habu-compiler-lower-unified-5f599080
---

Run a bounded, case-insensitive token-aware census after unified lowering and
prove src/core and src/habu contain no live SUMTYPE or legacy payloadless enum
declarations. If the census identifies a raw-tag family, migrate it to unified
ENUM while preserving package spelling, tag ordinal, layout, serialized
identity, and diagnostics. Otherwise close with the zero-finding artifact.
Parser, scanner, generated-source, tombstone, and rejection cleanup belongs to
the delete/tool dots and must not be duplicated here. Run family, lowering,
engine, snapshot, AOT, and fixpoint gates.
