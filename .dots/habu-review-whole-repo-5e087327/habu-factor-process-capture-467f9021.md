---
title: Factor process capture lifecycle
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.545407+02:00\\\"\""
closed-at: "2026-06-25T17:21:40.634436+02:00"
close-reason: Factored process capture lifecycle into lib/process.f shared helpers; argv/env/cwd capture variants now delegate setup, stdin drive, drain, timeout, cleanup, and finish. Validated focused process fixtures, source-list checks, manifest/signature fixtures, lints, engine suite, stdlib gate, and full native gate.
---

Finding F12. Evidence: docs/factorization-review.md:40; lib/process.f:235, lib/process-argv.f:140, lib/process-env.f:161. Root cause: plain, argv, stdin, and argv+env capture paths duplicate setup, probe, drain, poll, close, and reap lifecycle. Fix: factor shared capture validation/read/probe/drain/finish helpers while preserving module split. Why: capture behavior must stay consistent across all spawn variants. Validate with process, process-argv, process-env, process-cwd tests and full native gate.
