---
title: Clean up comparison false negatives after canonical runner adoption
status: open
priority: 3
issue-type: task
created-at: "2026-03-07T19:32:55.817624+01:00"
blocks:
  - habu-decompose-remaining-per-0c9e465d
---

Canonical test-batch/run_testsuite path plus Maxima comparison helpers. Root cause: after the real runner is in place, some residual failures may still be comparison drift rather than semantic mismatches. Fix: ensure local triage agrees with canonical comparison semantics. Why: late-stage correctness cleanup only.
