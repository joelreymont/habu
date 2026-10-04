---
title: Reset every process-wide lock in a forked child
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T12:49:28.707816+02:00"
---

fork copies only the calling thread, so a child forked while another task holds a process-wide lock inherits it held with no owner. b999043e (5426578b) resets lib/process-tree.f WALKING in lib/process-fork.f ENTER-CHILD (PROC-TREE:CHILD-RESET). Still exposed: lib/task.f's exit-chain lock (~760) and one-time setup flags (~249, ~596), lib/pg.f:638 CONFIG-LOCK (pg.f sits above process-fork, so a direct reset would load libpq into every forking program), lib/image-lifecycle.f:21 LOCK (baked). Acceptance: one mechanism by which each module that owns process-wide state declares its child reset (a reset-hook registry ENTER-CHILD runs, or a stated rule that tasks and their locks do not cross a fork, enforced); WALKING moves onto it; a fork under each held lock is shown safe or refused by name, seen failing first.
