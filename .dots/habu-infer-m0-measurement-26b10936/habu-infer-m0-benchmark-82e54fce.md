---
title: "Infer M0: benchmark record schema"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.746075+02:00"
blocks:
  - habu-infer-m0-schema-27e8ca5d
---

This is the benchmark-schema campaign record. Do not dispatch it as implementation work. Its leaves separately own producer identity, workload coordinates, raw metric payloads, and the canonical record codec. The campaign closes when the codec composes all three validated record families and rejects inconsistent cross-field combinations.
