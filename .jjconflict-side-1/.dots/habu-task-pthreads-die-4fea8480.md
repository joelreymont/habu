---
title: Task pthreads die inside a gate-pool fork
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T13:43:22.517065+02:00"
---

lib/task-test.f is green in-process and dies when forked through GT-POOL-START-FORK from the SAME image. Differential evidence (2026-08-07, macOS arm64): bin/hb --load lib/errors.f lib/string.f lib/prelude.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test/runner.f test/gate-common.f test/gate-pool.f test/run-shared-stdlib.f lib/task-test.f -> 'test: ok'. Same load list with a trailing probe that only wraps 's" lib/task-test.f" included' in GT-POOL-START-FORK -> fork worker throw rc 75; in the gate's stdlib/tail-process group the same file died kind=signal code=11 (SIGSEGV). lib/task.f creates real pthreads and carves a per-task dictionary region (TCB.DBASE/NDICT/CP) out of DATA, so the suspect is pthread creation and/or those regions in a forked child of the pool worker. Root cause unknown; find it, then decide whether lib/task.f can be made fork-safe or whether gate-pool must refuse to fork a tasking member. WORKAROUND IN PLACE: SUITE tasking-threads is scheduled through the tail slice (SUITE-TAIL-PROCESS? in test/gate-stdlib-lib.f), which spawns a fresh process per suite, so it runs green today; the fork route is still broken. Remove the tail-slice placement note in test/gate-stdlib-cases.f when this lands.
