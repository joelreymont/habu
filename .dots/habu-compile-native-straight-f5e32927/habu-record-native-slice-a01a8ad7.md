---
title: Record native slice metrics
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:57:52.876000+02:00"
blocks:
  - habu-compare-native-compiler-e85a3d6b
---

Full context: Wave 2 exits require source maps, pass dumps, coverage, and per-pass/per-artifact metrics against the pinned native baseline. Record time, size, stack traffic, spills, calls, and runtime for the exact straight-line corpus with stage digests. Acceptance: results are reproducible and attributable; regressions name the responsible pass; metrics never replace correctness. Dependency: native shadow comparison.
