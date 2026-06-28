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

Checkpoint 2026-06-28: dictionary checker certifications now batch seven
positive semantic sources into one `tools/check.f` run through the checker warm
image. Execution-vector misuse and CASE misuse negatives now use one
`tools/check.f --all-errors` run per group with per-word stderr assertions,
instead of one check process per rejecting definition. A direct dictionary
wrapper passed at 30.52s wall after dead wrapper removal. The first full gate
after the runner key change passed at 75.358s internal / 78.23s wall, and the
hot-cache full gate passed at 48.101s internal / 51.09s wall with
`helper-spawn=113` (down from 123) and `warm-hit=16`.
