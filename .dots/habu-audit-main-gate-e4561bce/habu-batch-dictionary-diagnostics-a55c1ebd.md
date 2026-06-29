---
title: Batch dictionary diagnostics checks
status: closed
priority: 1
issue-type: task
created-at: "2026-06-28T19:06:06.707197+02:00"
closed-at: "2026-06-29T02:27:26.923036+02:00"
close-reason: "completed: diagnostics repair batches repair-class checks and uses gate-json-assert-core in-process; dictionary checker positives and execution-vector/CASE misuse negatives are batched through check/check-all-errors. Current full hot gate passed at 46.183s internal / 49.20s wall with dictionary 15.264s, diagnostics repair 10.376s, undef-primary 11.027s, all-strict 13.825s, and file-unsafe 6.930s. Remaining GE-HB-RUN/GE-RUN sites are runtime/load/CLI boundary contracts, not pure checker assertions."
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

Close checkpoint 2026-06-29: current full hot gate passed at 46.183s internal /
49.20s wall. Dictionary/checker ran in 15.264s; diagnostic slices ran in
10.376s, 11.027s, 13.825s, and 6.930s. Remaining dictionary/diagnostic
`GE-HB-RUN` and `GE-RUN` sites are runtime/load/CLI boundary contracts, not
pure checker assertions, so the checker/diagnostic batching acceptance is met.
