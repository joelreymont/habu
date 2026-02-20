---
title: "Major GC: pause-slice validation"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-20T08:58:56.363163+01:00\\\"\""
closed-at: "2026-02-20T14:26:45.443264+01:00"
close-reason: Validated major slice budgets in bench and gates
blocks:
  - habu-major-gc-barrier-ac8038a7
---

tools/gc-compare, src/tests: verify slice scheduling meets pause budgets without liveness regressions.
