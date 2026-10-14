---
title: Print a warning once in check.f
status: closed
priority: 3
issue-type: task
created-at: "2026-10-03T13:23:51.476314+02:00"
closed-at: "2026-10-15T02:58:08.240379+03:00"
close-reason: Superseded by habu-print-check-f-5dce644f, which now explicitly retains warning-only one-record/mode acceptance. CHK-RUN-CHECK-SILENT already suppresses the run child repeat; remaining pre-pass JSON formatting belongs to that task.
---

A warning-only source (W-EFFECT-NOT-RECORDED) prints its warning twice under tools/check.f: once from the in-process stage and once from the run child; in prose mode the in-process stage's warning leaks as a raw JSON line ahead of the prose one (review 436, both reproduce on the pre-round-4 engine). Acceptance: one warning line per warning, in the mode's format, seen failing first.
