---
title: Stress many Habu tasks
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T14:55:27.413542+02:00"
---

Problem: lib/task-test.f proves two pthread tasks only; it does not exercise allocator/user-area/thread-table limits with many simultaneous tasks. Fix: add a checked stress fixture that constructs and activates dozens/hundreds of TASK records, increments per-task and shared counters under FACILITY, joins/kills cleanly, and proves no leaked TASKS-LIVE state. Files: lib/task.f, lib/task-test.f, docs/threads.md. Verify: bin/hb --load lib/task-test.f on macOS and zed; full native test suite stays green.
