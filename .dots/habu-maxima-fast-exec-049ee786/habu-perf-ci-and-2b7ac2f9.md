---
title: Perf CI and docs
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-17T22:23:04.202826+01:00\\\"\""
closed-at: "2026-02-20T16:19:09.172730+01:00"
close-reason: completed
blocks:
  - habu-lock-hoist-api-0d6259d1
---

PLAN.md, LESSONS.md, docs/maxima-loader.md. Cause: regressions recur without automated perf gates. Fix: codify targets, bench scripts, and fail thresholds in docs/CI.
