---
title: "Walk tools/ in the suite-coverage lint's routing check"
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T20:43:11.833203+02:00"
---

A tools/<name>-test.f that runs T-REPORT and is listed in no gate file runs nowhere, and nothing notices. Wanted: a lint that walks tools/ and derives two facts per file (RUNS = names T-REPORT, NAMED = another file writes the path), so an unrouted tools test file is a finding; the same walk over test/ closes the sibling blind spot (dot habu-schedule-unscheduled-compiler-b36ff91b). Found while scheduling the codegen comparison harness (dot habu-schedule-the-codegen-4e1915bc), whose premise - a finished tools/ test file in no gate - nothing would have caught.
