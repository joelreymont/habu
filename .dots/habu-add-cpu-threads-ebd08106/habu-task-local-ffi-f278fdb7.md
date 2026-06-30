---
title: Task-local FFI race regression
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T14:58:37.230295+02:00"
closed-at: "2026-06-30T15:34:04.774378+02:00"
close-reason: "Implemented task-local FFI regression in lib/task-test.f: each worker interleaves TASK:+USER writes with task-local FFI scratch through strlen, validates TASK:HIS state after join, and keeps compile-while-tasks-live fail-closed guard at rc 0x4F. Proof: macOS and zed bin/hb --load lib/task-test.f plus full suites."
---

Problem: tasking must prove task-local state and FFI scratch are isolated under concurrency, and compile-while-tasks-live stays fail-closed. Fix: add a regression that interleaves task-local +USER writes with FFI scratch use and keeps/extends the live compile guard fixture. Files: lib/task-test.f, docs/threads.md. Verify: macOS and zed task-test; guard exits deterministic rc 0x4F.
