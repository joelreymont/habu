---
title: Roll a declaration back while a task is live
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T07:32:13.019671+01:00"
---

Problem: GENERATED-DECL-DICTIONARY:ROLLBACK (src/core/generated-declaration-dictionary.f ~35) lowers NDICT with ndict!, which exits $4F on the main thread while a task is live (src/lib/task.f:7), so a declaration rejected after it made records while a task runs adds a second record, E-TASK-COMPILE 'ndict! ran while a task is live, in: ROLLBACK', and exits 79 instead of its own status (reproducer /tmp/carl-rp/enum1.f, a replayed ENUM with a task live). Acceptance: such a declaration gives only its own record and status under --load and in the REPL, with a task live; its records are retired as evaluate's recovery retires them (src/habu/habu1.f EMIT-DEF-OCC-RETIRE). Depends: none. Claim: unassigned.
