---
title: Print a breakpoint hit on a task thread instead of dying
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T16:56:41.679793+02:00"
---

Problem: a persistent DEBUG BP* breakpoint hit on a task's thread (src/lib/task.f pthread) kills the image with SIGTRAP (habu-crash regs, sig 5, pc = the word's first instruction) instead of printing habu-bp:/habu-bp-lr:/habu-bp-stack: and continuing, as a main-thread hit does; several tasks hitting one breakpoint at once kill it the same way. Reported by tim from Maki 2026-11-06 on the installed hb (16:14), arm64 macOS; evidence, reproducer and log in /tmp/tim-habu-breakpoint.md and /home/joel/Work/maki/.jj-ws.noindex/db-restart/.out/{trace-serve.f,trace-maki.sh,dbr-trace-run.sh,dbr/trace.log}. Cause not known: inferred only that the trap handler serves the main thread alone, or not concurrent traps. Acceptance: a reduced Habu reproducer (a task that hits a BP* breakpoint, then several tasks at once) prints each hit and continues; the test is that reproducer in the suite. Files: src/lib/debug*.f and the trap handler it installs (to be located by the root-cause step). Verify: the reproducer test. Depends: none. Ownership: unassigned. Maki does not need it now. Claim: unassigned.
