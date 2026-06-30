---
title: Fix in-process negative source capture
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T07:59:42.179933+02:00"
closed-at: "2026-06-30T08:17:22.525576+02:00"
close-reason: "completed locally: CHECK-ALL-ERRORS-BUF plus GD-CHECK-BUF-BAD migrated package no-return negatives in-process; duplicate-definition remains boundary sentinel; check-all-errors-test, dictionary phase, full cold/hot native suite green; zed untouched"
---

Problem: moving dictionary/checker negative source probes from child bin/hb to raw GE-EVAL-CAPTURE is unsafe. Reduced evidence on macOS: redirect/restore alone captures and drains; positive GE-EVAL-CAPTURE captures and returns rc=0; rejected source `: GE-PROBE-BAD ( -- n ) 1 2 ;` exits the current process via checker/compiler `die` with rc 70, so `catch` cannot convert it into an in-process outcome or diagnostic. Fix: add checked in-process negative checker coverage through `CHECK-ALL-ERRORS-BUF`/`VERIFY-SOURCE-BUF` candidate scope and diagnostic-buffer rendering, not raw evaluate; migrate only semantic checker-negative probes that do not require argv/env/source-file/process isolation; leave true CLI/compiler-die/process-boundary probes as child bin/hb sentinels. Verify: focused adapter fixtures return expected reject verdict and diagnostic text without process exit; dictionary/checker local suite reduces inner-hb-stdin; full local native suite green. Zed/device timing is out of scope.

2026-06-30 local proof: `tools/check-all-errors-core.f` now exposes `CHECK-ALL-ERRORS-BUF`; `tools/check-all-errors-test.f` proves buffer diagnostics; dictionary package no-return negative checks use `GD-CHECK-BUF-BAD` in-process. Duplicate-definition stayed as a child `check.f` sentinel because the current checker duplicate path exits through fatal rc 78. Focused `check-all-errors-test` passed; direct dictionary/checker phase passed; full local suite passed cold 40075ms internal / 42.230s wall and hot 25637ms internal / 27.849s wall with `inner-hb-stdin=19`, `boundary=50`, `warm-miss=0` on the hot run. Zed/device timing intentionally untouched.
