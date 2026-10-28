---
title: Grow AT-EXIT rows with the tasks that use them
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T00:36:23.788652+02:00"
---

TASK-EXIT-MAX (src/lib/task.f:822, $80) caps the whole process at 128 AT-EXIT rows. A task registers one row per library it uses (aio SCRUB aio.f:851, genio.f:52, net/http.f:564 exit hooks, c2-memory/owner-runtime.f:162 DRAIN, net/curl.f:1659), so only about 128 / rows-per-task TASK:NEW tasks can live at once; past that a task's first AIO submission or scope fails with E-TASK-EXIT-TABLE. This blocks growing worker slots (Zen zen-grow-worker-slots, net/http's fixed task count). Fix: row storage that grows (chunked like the TCB pool, or owned per TCB record); raising the constant is a bigger cap, not a fix. Owner: phil, as a follow-on commit after the TASK:NEW lane (wyskoqmq) in phil-tasknew; hand the change id to carl after review. Found by the TASK:NEW lane review; predates that lane.
