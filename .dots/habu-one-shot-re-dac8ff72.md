---
title: One-shot re-measure tool for all size ratchets
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-21T16:31:12.721005+02:00\\\"\""
---

Orchestration friction, recurring ~8x on 2026-07-21 alone: every cross-machine src landing obligates this box to re-measure census (STATUS.md), CODELEN + floor-dist (gate-size-attribution), whole-file baseline (gate-build-size), and now the 43 per-region budgets - currently done by hand (fixpoint rebuild, size-report, edit 3-4 files, re-run tests), error-prone (the floor-direction arithmetic was mis-predicted twice; rule: MEASURE then transcribe, never predict). Build tools/re-measure.f: one command that (1) fixpoint-rebuilds with the size map, (2) measures census + code-total + floor + per-region rows + file size, (3) REWRITES all committed rows in place with a dated provenance comment naming the commit being absorbed, (4) re-runs the size/census tests, (5) prints the delta table for the commit message. Fail-closed if the map does not reconcile. Red-first: run against a tree with deliberately stale rows, verify every row lands at measured truth and the tests flip green.

Claim: agent=fable-remeasure workspace=.jj-ws/fable-remeasure machine=spark
