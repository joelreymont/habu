---
title: Add task channels
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T14:55:57.021209+02:00"
---

Problem: FACILITY mutexes are enough for exclusion but not for structured task coordination; condition variables/channels/futures are missing if tasks are meant for real pipelines. Fix: add typed Habu synchronization primitives in stages: pthread condvar-backed condition wait/signal/broadcast, bounded channel send/recv/close over mutex+condvar, then futures/promises as a small layer over channels. Keep unchecked FFI shim thin and typed wrappers checked. Files: lib/task.f or new lib/task-channel.f, tests, docs/threads.md, stdlib manifest/docs. Verify: producer/consumer, close, timeout/cancel, and multi-producer/multi-consumer fixtures on macOS and zed.
