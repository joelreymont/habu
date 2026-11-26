---
title: "Read Linux's monotonic clock without a syscall"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T09:06:21.832635+01:00"
---

Problem: src/os/linux/sys.f:114 OS-TIMER-NOW makes a clock_gettime syscall per TIME:MONO-NS reading, 138 ns each on spark; strace counts one syscall per reading (reproducer /tmp/carl-mono/README.md). The vDSO path reads the clock in user space. Acceptance: a MONO-NS reading on Linux arm64 makes no syscall, on the clock Linux's timers expire on. Depends: none. Claim: unassigned.
