---
title: Weigh each profile sample by the time since the last
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T07:24:10.042376+02:00"
---

Problem: the profiler samples on setitimer ITIMER_REAL (src/habu/prof.f:703, EMIT-PROF :621) and counts one per delivered SIGALRM; ticks that expire while the process is blocked in a syscall (fsync, F_FULLFSYNC at src/lib/fs-mutate.f:614) or descheduled merge into one pending signal, so waiting time is undercounted. Acceptance: under prof-on, a phase that blocks (a loop of F_FULLFSYNC on a scratch file) reports total samples times the interval within 10% of its wall time, and the word that blocked carries that weight; CPU-bound phases report as before. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-prof-a.
