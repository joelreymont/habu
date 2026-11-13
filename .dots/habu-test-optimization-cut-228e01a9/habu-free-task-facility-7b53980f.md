---
title: "Free TASK:FACILITY locks held across a fork"
status: closed
priority: 3
issue-type: task
created-at: "2026-10-03T18:10:14.059704+03:00"
closed-at: "2026-11-04T11:48:50.257660+02:00"
close-reason: "Census 2026-11-04: goes with habu-fork-only-to-59d3557e, fork only to exec (docs/design.md \"Machinery being replaced\"). Locks held across fork matter only to a child that keeps running Habu; fork only to exec [i]"
---

Found by lane 449 forklocks: a TASK:FACILITY mutex (e.g. src/lib/aio.f:249 AIO-LOCK) or a Darwin task semaphore held by another task at fork stays held forever in the child, which has only the forking thread; any child path that takes it deadlocks. Lane 449 holds the three engine registry locks across fork and resets abandonable ones through FORK-CHILD; facilities are not covered. Decide per facility class: hold across fork (prepare/parent/child release) or refuse the facility's use in a forked child by name. Acceptance: a hammer case per facility class shows a child hang before and none after; or a named refusal with its test.
