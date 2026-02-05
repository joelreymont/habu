---
title: Add compiler/eval repro tests batch1
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-05T22:32:05.508072+01:00\""
closed-at: "2026-02-05T22:47:29.205024+01:00"
close-reason: Added batch repro tests for compiler/eval failures and removed doCall debug prints.
blocks:
  - habu-map-failures-to-e9ce25c5
---

Context: /Users/joel/Work/habu/src/tests/integration.zig:1, /Users/joel/Work/habu/src/compiler/compile.zig:1; cause: evaluator/compiler conformance failures lack reduced tests; fix: add <=5 mapped repros; deps: habu-map-failures-to-e9ce25c5; verification: tests fail pre-fix and map to ANSI ids.
