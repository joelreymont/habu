---
title: Roll a declaration back while a task is live
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T07:32:13.019671+01:00"
---

Problem: a rollback that lowers a cursor while a task is live exits $4F on the main thread (src/lib/task.f:7): GENERATED-DECL-DICTIONARY:ROLLBACK (src/core/generated-declaration-dictionary.f ~35), the REPL's FRAME-ROLLBACK-CODE (src/habu/interpreter.f), the linker's FORGET-SPACE (src/habu/linker.f) and the checker overlay's ROLLBACK (src/core/checker.f). So a declaration rejected after it made records while a task runs adds a second record, E-TASK-COMPILE 'ndict! ran while a task is live, in: ROLLBACK', and exits 79 instead of its own status (reproducer /tmp/carl-rk/p/live-enumdup.f, a duplicate enum member with a task live). Acceptance: such a declaration gives only its own record and status under --load and in the REPL, with a task live; its records are retired as evaluate's recovery retires them (src/habu/habu1.f EMIT-DEF-OCC-RETIRE). Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-rk.
