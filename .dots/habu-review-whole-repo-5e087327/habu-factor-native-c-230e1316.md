---
title: Factor native C-CALL emitter
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.521456+02:00\\\"\""
closed-at: "2026-06-25T16:14:50.564449+02:00"
close-reason: "completed: split native C-CALL into prologue/plain span helpers, unsafe scan helpers, inline copy, and absolute-call emitter in src/habu/habu2.f; added tools/c-call-emitter-test.f and c-call-emitter-shape gate suite; validated c-call-emitter-test, engine-suite, bootstrap-codegen-test, trust-lint, stale-status-lint, filemap-lint, recovery probe unchanged at a09a95574b1a185a7ec918d33b84fce839fd623441339f006366ac1eac2da7fd, and full native gate PASS"
---

Finding F05. Evidence: docs/factorization-review.md:33; src/habu/habu2.f:25. Root cause: C-CALL mixes prologue recognition, inline-size policy, unsafe instruction scan, inline copy, and absolute-call stencil emission. Fix: split into prologue predicate, inline safety predicate, inline copy helper, and absolute call emitter. Why: call/inline policy must be separately reviewable from code emission. Validate with native fixpoint, hb-build tests, and full native gate.
