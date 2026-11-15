---
title: Print a breakpoint hit on a task thread instead of dying
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T16:56:41.679793+02:00"
---

Problem: a persistent DEBUG BP* breakpoint hit on a task's thread (src/lib/task.f pthread) kills the image with SIGTRAP (habu-crash regs, sig 5, pc = the word's first instruction) instead of printing habu-bp:/habu-bp-lr:/habu-bp-stack: and continuing, as a main-thread hit does; several tasks hitting one breakpoint at once kill it the same way. Reported by tim from Maki 2026-11-06 on the installed hb (16:14), arm64 macOS. It reproduces on Linux arm64 with the installed hb: run from the tree, `require src/lib/task.f require src/habu/debug.f package PBQ TASK:MIN-STACK TASK:TASK W : KEEP ( n -- n ) ; ' KEEP DEBUG:BP* : BODY ( -- ) 17 KEEP . ; : GO ( -- ) 25 KEEP . ['] BODY W TASK:ACTIVATE W TASK:JOIN MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH ; GO ;package` prints the main-thread hit (habu-bp:, habu-bp-lr:, habu-bp-stack:, then 25), then the task's hit prints habu-crash regs with sig 5 and pc KEEP's first instruction, and the process dies of SIGTRAP (rc 133). Cause not known: inferred only that the trap handler serves the main thread alone, or not concurrent traps. Acceptance: a reduced Habu reproducer (a task that hits a BP* breakpoint, then several tasks at once) prints each hit and continues; the test is that reproducer in the suite. Files: src/lib/debug*.f and the trap handler it installs (to be located by the root-cause step). Verify: the reproducer test. Depends: none. Ownership: unassigned. Maki does not need it now. Claim: unassigned.
