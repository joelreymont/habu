---
title: Add task race fuzz
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T14:55:43.035498+02:00"
closed-at: "2026-06-30T14:57:33.345031+02:00"
close-reason: superseded by narrow Odin task-state race dot
---

Problem: current task tests are deterministic and short; they will miss races in user variables, task lifecycle, atomics, and mutex release paths. Fix: add a long-running checked race/fuzz fixture with seeded pseudo-random worker actions: atomic increments, FACILITY acquire/release, PAUSE, task-local +USER writes, and cooperative stop. Keep it deterministic by seed and bounded by iteration/time args. Files: lib/task.f, lib/task-test.f, docs/threads.md, test suite wiring if needed. Verify: focused smoke seed in normal suite on macOS and zed; longer manual seed documented.
