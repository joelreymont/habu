---
title: "Free TASK:FACILITY locks held across a fork"
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T18:10:14.059704+03:00"
---

Found by lane 449 forklocks: a TASK:FACILITY mutex (e.g. lib/aio.f:249 AIO-LOCK) or a Darwin task semaphore held by another task at fork stays held forever in the child, which has only the forking thread; any child path that takes it deadlocks. Lane 449 holds the three engine registry locks across fork and resets abandonable ones through FORK-CHILD; facilities are not covered. Decide per facility class: hold across fork (prepare/parent/child release) or refuse the facility's use in a forked child by name. Acceptance: a hammer case per facility class shows a child hang before and none after; or a named refusal with its test.
