---
title: Reset every process-wide lock in a forked child
status: closed
priority: 3
issue-type: task
created-at: "2026-10-03T12:49:28.707816+02:00"
closed-at: "2026-10-15T01:26:45.702958+03:00"
close-reason: Completed or superseded on current master. 2e44edf6 provides FORK-CHILD reset registry for task exit, lifecycle, PG config, dynamic-storage registrar and process-tree locks. Fork hammer, PG fork, stripped image and convergence recorded green. Distinct AIO facilities/inherited connections remain separately tracked. Read-only source/history evidence checked in backlog review; no new code or broader runtime claim.
---

fork copies only the calling thread, so a child forked while another task holds a process-wide lock inherits it held with no owner. b999043e (5426578b) resets src/lib/process-tree.f WALKING in src/lib/process-fork.f ENTER-CHILD (PROC-TREE:CHILD-RESET). Still exposed: src/lib/task.f's exit-chain lock (~760) and one-time setup flags (~249, ~596), src/lib/pg.f:638 CONFIG-LOCK (pg.f sits above process-fork, so a direct reset would load libpq into every forking program), src/lib/image-lifecycle.f:21 LOCK (baked). Acceptance: one mechanism by which each module that owns process-wide state declares its child reset (a reset-hook registry ENTER-CHILD runs, or a stated rule that tasks and their locks do not cross a fork, enforced); WALKING moves onto it; a fork under each held lock is shown safe or refused by name, seen failing first.
