---
title: Task fatal fail-closed proof
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T14:58:37.233531+02:00"
closed-at: "2026-06-30T15:34:04.860290+02:00"
close-reason: "Implemented deterministic fail-closed worker failure: task bodies now run through TASK-RUNNER; worker die preserves explicit code/message, uncaught throw exits process-wide with task: unhandled throw. Subprocess fixtures prove rc 0x62 and E-TASK-STATE low byte. Proof: macOS and zed bin/hb --load lib/task-test.f plus full suites."
---

Problem: Odin accepts process-wide fatal exit for task failure only if deterministic and documented. Fix: add subprocess fixtures where a task calls die and throws without a handler, prove the process exits with the expected code/message, and document that worker fatal paths are process-wide until a result/join model exists. Files: lib/task-test.f, docs/threads.md. Verify: macOS and zed task-test.
