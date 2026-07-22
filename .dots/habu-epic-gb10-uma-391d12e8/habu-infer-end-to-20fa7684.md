---
title: "Infer: end-to-end single-sequence engine"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T15:58:37.094041+02:00"
blocks:
  - habu-infer-engine-steady-3fe9348e
---

This is the single-sequence engine campaign record. Do not dispatch it as implementation work. Its leaves own engine state, prefill, one paged decode step, sampling and detokenization, the 64-token GPT-2 oracle, and the steady-state benchmark record. The campaign closes when that benchmark record is reproducible from the complete engine.
