---
title: Make stdlib-runner-fixtures pass under load
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T18:19:49.287653+02:00"
---

src/lib/test/runner-test.f fails asserts 40 and 41 (expected 0 got 1; expected true got false) with WAIT: progress stdin (14693ms) when the full suite runs beside another suite and a two-generation build (carl-int .out/land/reg.log:1597-1615, 2026-10-31; red under load once before), and passes alone (rc 0, 16 s, same engine). The case is GTT-PROGRESS-STDIN-CAT (runner-test.f:357), /bin/cat under GTT-CMD-TIMEOUT-MS through GT-PROGRESS-STDIN-CAPTURE (src/lib/test/runner.f:451-482). Acceptance: the cause named from artifacts (which deadline expires and why cat is not done), the test fixed so load cannot fail it, or the runner bug fixed.
