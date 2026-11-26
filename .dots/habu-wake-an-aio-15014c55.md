---
title: Wake an AIO owner after the loop releases its lock
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T13:49:55.852276+01:00"
---

Problem: src/lib/aio.f SETTLE wakes a record's owner (TASK:WAKE) while the loop holds AIO-LOCK, so the woken owner's first act, TAKE's AIO-LOCK TASK:GET, sleeps until the loop has drained every ring: Maki measured 19-72 us sleeps after a wake (17 of 141 waited queries), once 821 us. Acceptance: no owner is woken under AIO-LOCK on Linux or macOS, a cross-task AIO:CANCEL that settles a record at stage time still wakes its owner, and the aio, task and process-task suites pass on arm64 and x86-64. Depends: none.
