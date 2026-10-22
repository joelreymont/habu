---
title: Read script argv on any task in a native image
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T08:43:00.581979+03:00"
---

Reported by Zen (phil) 2026-10-22, not yet reproduced here. In a native image, SCRIPT-ARGC is 0 and `0 SCRIPT-ARGV$` throws 7180 (E-SCRIPT-ARGV-RANGE) on any task other than main. Cause as reported: src/os/script-argv.f:39-43 SCRIPT-ARG-START reads `data-base APP-ENTRY:XT-CELL + @` to tell an application from the engine, and data-base is task-local, so a spawned task reads its own context's cell (0) and takes the engine branch. Source mode (`hb --load`) takes the ARGV branch and works. Repro: native-build a program whose main task prints SCRIPT-ARGC (1) and spawns a task that prints SCRIPT-ARGC (0) and calls `0 SCRIPT-ARGV$` (throws 7180); the same file from source works on the task. docs/stdlib.md:2731 documents SCRIPT-ARGV without a main-task restriction. Fix at the layer that owns the application/engine fact (the image-wide cell, not a task-local one); add a native E2E case with a spawned task.
