---
title: Batch dictionary diagnostics checks
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T19:06:06.707197+02:00"
---

Problem: test/gate-dictionary.f and test/gate-diagnostics.f contain many GE-HB-RUN-STDIN/GE-CHECK-RUN snippets. Each snippet starts a new Habu/check process even when the invariant is just checker verdict, diagnostic content, or dictionary behavior in one controlled source. Fix: group independent positive snippets into one source per feature group and group negative checker diagnostics through CHECK-CANDIDATE!/check-all-errors JSONL where process isolation is not the invariant. Files: test/gate-dictionary.f, test/gate-diagnostics.f, test/gate-common.f. Acceptance: one process per feature group, stable per-case attribution in output/JSONL, same negative diagnostics asserted, dictionary/checker and diagnostics slices shrink, full native gate passes.
