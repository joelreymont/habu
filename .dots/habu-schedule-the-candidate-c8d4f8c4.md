---
title: Schedule the candidate-validation phase visibly
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T15:29:56.799943+02:00"
---

Phase 20 (native engine candidate validation slice) is silently skipped in the default test/run.f profile — it needs --under a candidate engine, and its absence looks identical to a pass. Per the passing-is-not-scheduled rule: the default profile must either run it or print a named SKIPPED line counted in the report, so a gate that did not run can never read as green. Found by the CG-23 lane, which had to invoke the worker directly to see the red it then fixed.
