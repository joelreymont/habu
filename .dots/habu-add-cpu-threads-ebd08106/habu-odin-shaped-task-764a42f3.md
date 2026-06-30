---
title: Odin-shaped task soak
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T14:58:37.226003+02:00"
closed-at: "2026-06-30T15:34:04.789639+02:00"
close-reason: "Implemented in lib/task-test.f as 4 acquisition tasks plus detector/control task, repeated start/join cycles, atomics, TASK:FACILITY, task-local TASK:+USER/HIS checks, and FFI strlen calls from worker threads. Proof: macOS bin/hb --load lib/task-test.f; macOS full suite 25821ms internal; zed bin/hb --load lib/task-test.f; zed full suite 80879ms internal."
---

Problem: Odin port needs proof for its actual host shape, not generic thread completeness. Fix: add a checked task soak with 4 acquisition tasks plus 1 detector/control task, repeated start/join cycles, atomics, FACILITY mutex, and FFI calls from task threads. Files: lib/task-test.f, docs/threads.md. Verify: bin/hb --load lib/task-test.f on macOS and zed.
