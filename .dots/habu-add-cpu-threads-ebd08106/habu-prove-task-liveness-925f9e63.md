---
title: Prove task liveness under PAUSE
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T14:55:35.838924+02:00"
closed-at: "2026-06-30T14:57:33.341783+02:00"
close-reason: superseded by narrow Odin-shaped soak dot
---

Problem: pthread scheduling fairness is owned by the OS, but Habu still needs a tested liveness/yield contract for PAUSE and mutex contention. Fix: add a bounded liveness fixture with multiple workers contending on FACILITY, explicit PAUSE points, per-worker progress counters, and a timeout failure if any worker starves. Do not claim global scheduler fairness. Files: lib/task.f, lib/task-test.f, docs/threads.md. Verify: task fixture passes on macOS and zed; diagnostics name the stalled worker/counter.
