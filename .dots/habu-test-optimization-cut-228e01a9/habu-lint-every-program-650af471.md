---
title: Lint every program check.f accepts
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T10:16:55.382279+02:00"
---

Problem (r4-chkorphan fix lane, 51f9ee92): check.f accepts a subject up to CHK-SRC-CAP ($100000, tools/check-core.f:55), but a stdin program of 256 KiB dies in the lint read, tools/lint/text.f:98 `lint: file exceeds buffer` (192 KiB passed), and that die leaves a habu-check-* directory under HB_TMP. Two defects: the lint read's capacity disagrees with check.f's, and a die on that path skips check.f's temp cleanup. Acceptance: a failing case first (check-test or check-cli-boundary row) at a size between the two caps, through a file and through stdin; every program check.f accepts is linted (one capacity owned in one place, or the lint read sized from check.f's) or refused before the run with a named diagnostic; no lint or check.f die leaves its root behind (census the die sites reachable from tools/check.f after CHK-MAKE-TEMP and route them through the cleanup).
