---
title: Enforce GC parity CI gates
status: open
priority: 1
issue-type: task
created-at: "2026-02-20T08:55:19.504952+01:00"
blocks:
  - habu-automate-gc-self-807cbd79
---

File: bench/check.zig:1, tools/gc-compare:1; cause: regressions can land silently; fix: CI thresholds tied to baseline deltas and hard failure on regressions; why: preserve gains while iterating.
