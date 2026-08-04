---
title: Wire the unwired coverage
status: open
priority: 2
issue-type: task
created-at: "2026-08-04T20:52:23.009696+02:00"
---

The dead-code sweep (ed132e03) found three unscheduled tests of LIVE modules — tools/json-test.f (361 lines, the only coverage of tools/json.f's strict-error and JSONL matrix), lib/test/subject-test.f (86), tools/stdlib-time-test.f (43) — and one stale doc pointer: docs/forth.md's Errors section names src/config.fs which lives at bootstrap/src/config.fs (the section describes the sealed ENGINE-ERROR ABI; the rewrite is judgment, not mechanical). Register the three tests in the appropriate gate-stdlib suites and confirm they execute in a full run; fix the forth.md pointer while stating what the section actually pins.
