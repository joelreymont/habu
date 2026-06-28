---
title: Batch dictionary diagnostics checks
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T19:06:06.707197+02:00"
---

Problem: test/gate-dictionary.f and test/gate-diagnostics.f contain many GE-HB-RUN-STDIN/GE-CHECK-RUN snippets. Each snippet starts a new Habu/check process even when the invariant is just checker verdict, diagnostic content, or dictionary behavior in one controlled source. Fix: group independent positive snippets into one source per feature group and group negative checker diagnostics through CHECK-CANDIDATE!/check-all-errors JSONL where process isolation is not the invariant. Files: test/gate-dictionary.f, test/gate-diagnostics.f, test/gate-common.f. Acceptance: one process per feature group, stable per-case attribution in output/JSONL, same negative diagnostics asserted, dictionary/checker and diagnostics slices shrink, full native gate passes.

Checkpoint 2026-06-28: diagnostics repair now batches the five repair-class
fixtures into one `tools/check.f --json-errors --all-errors` run and asserts
each word's class through `tools/gate-json-assert-core.f` in-process. The
diagnostic JSON schema/origin/class assertions no longer spawn
`tools/gate-json-assert.f`; focused diagnostics repair passed in 11.82s with 3
helper spawns, and the full hot gate passed at 48.621s internal / 51.61s wall.
Remaining work: dictionary/checker still has 25 boundary stdin runs and 69
helper spawns in focused measurement.
