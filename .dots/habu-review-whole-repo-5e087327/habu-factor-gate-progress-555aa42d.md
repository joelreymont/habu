---
title: Factor gate progress capture
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.552232+02:00\\\"\""
closed-at: "2026-06-25T17:36:48.715136+02:00"
close-reason: Moved progress-aware capture loops into lib/test-runner.f with non-stdin, flushing, and stdin variants; gate-stdlib and test/run now keep phase setup/policy only and no longer reach into direct poll/PFD machinery. Validated test-runner fixture, source-list check, manifest fixture, stdlib gate, lints, and full native gate.
---

Finding F14. Evidence: docs/factorization-review.md:42; test/gate-stdlib.f:31 and test/run.f:69. Root cause: heartbeat/progress capture logic is duplicated and stdin capture reaches into low-level process internals. Fix: add shared progress-aware capture helpers with stdin and non-stdin variants; keep gate words scenario-level. Why: gates should report progress without duplicating process machinery. Validate with gate-stdlib and full native gate.
