---
title: Define task failure propagation
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T14:55:50.031637+02:00"
---

Problem: fatal errors currently terminate the whole process with exit_group, which is correct for compiler/checker die paths, but Habu lacks a typed task failure/cancellation result model for application workers. Fix: design and implement an explicit task result state: running/done/canceled/failed with captured throw code where recoverable, JOIN/WAIT-style observation, and documented boundary for unrecoverable process-fatal errors. Files: lib/task.f, lib/task-test.f, docs/threads.md. Verify: worker throw/fail/cancel fixtures on macOS and zed; process-fatal paths still exit process-wide.
