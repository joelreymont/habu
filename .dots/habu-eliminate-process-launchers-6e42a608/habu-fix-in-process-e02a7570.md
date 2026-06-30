---
title: Fix in-process negative source capture
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T07:59:42.179933+02:00"
---

Problem: moving dictionary/checker negative source probes from child bin/hb to GE-EVAL-CAPTURE is currently unsafe. Local probe with GE-SRC ': BAD ( -- n ) 1 2 ;' plus GE-EVAL-CAPTURE waited on capture timeout instead of returning a fail-closed rc/diagnostic; stdout/stderr pipe drain likely keeps a write end open on rejection. Fix: build a focused checked regression for GE-EVAL-CAPTURE on rejected source, repair redirect/restore/drain so thrown checker/compiler errors produce a bounded captured outcome, then migrate only semantic negative probes that do not require argv/env/process isolation. Verify: focused probe returns expected rc+err without timeout; dictionary/checker local suite reduces inner-hb-stdin; full local native suite green. Zed/device timing is out of scope.
