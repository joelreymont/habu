---
title: "Walk tools/ in the suite-coverage lint's routing check"
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T20:43:11.833203+02:00"
---

tools/suite-coverage-lint-core.f reads the registry from disk in only two places: check (d) walks tools/lint and demands every *-test.f there be a TEST:SUITE member, and check (e) walks test/ and demands every file that runs T-REPORT be routed. Nothing walks the rest of tools/, so a tools/<name>-test.f outside tools/lint that runs T-REPORT and is listed in no gate file runs nowhere and no lint notices - the same blind spot check (e) was built to close for test/ (dot habu-schedule-unscheduled-compiler-b36ff91b). Wanted: check (e)'s walk extended over tools/ with the same two derived facts (RUNS = names T-REPORT, NAMED = another file writes the path), so an unrouted tools test file is a finding. Found while scheduling the codegen comparison harness (dot habu-schedule-the-codegen-4e1915bc), whose premise - a finished tools/ test file in no gate - nothing would have caught.
