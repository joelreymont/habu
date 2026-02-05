---
title: Add reader/printer repro tests batch1
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-05T22:32:05.474587+01:00\""
closed-at: "2026-02-05T23:09:18.938764+01:00"
close-reason: Added four ANSI ID repro tests for reader/printer nonconformance and validated full test suite
blocks:
  - habu-map-failures-to-e9ce25c5
---

Context: /Users/joel/Work/habu/src/tests/integration.zig:1, /Users/joel/Work/habu/src/runtime/primitives/io.zig:1; cause: failing reader/printer semantics lack direct regressions; fix: add <=5 minimal repro tests from baseline; deps: habu-map-failures-to-e9ce25c5; verification: tests fail before fix and reference source ANSI ids.
