---
title: Make warn/cerror conform and update repro tests
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T23:30:55.659422+01:00"
---

Context: /Users/joel/Work/habu/lib/stdlib.habu:600,2230,4862 and /Users/joel/Work/habu/src/tests/integration.zig:3155-3214; cause: warning/cerror behavior still characterized as nonconforming and tests assert TypeMismatch; fix: consolidate warn/cerror semantics through restart-case + signal path loaded via stdlib and update tests to assert conforming behavior with handler-bind and continue/muffle-warning; deps: Dispatch condition throws via handler-bind; verification: condition/restart batch tests pass and no longer use still-nonconforming assertions.
