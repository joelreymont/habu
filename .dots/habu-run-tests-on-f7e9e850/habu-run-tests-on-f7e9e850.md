---
title: Run tests on the shipped hb without child processes
status: open
priority: 1
issue-type: task
created-at: "2026-10-30T20:40:12.627548+02:00"
---

Problem: Joel 2026-10-30 ~20:45, after the c2-records hang: 'we decided to build everything in a single binary, why are you launching processes? are you trying to prove that the program you build runs?' then 'no pre-existing! fix this and look for similar code that should be fixed too'. Observed: test/c2-records-lifecycle.f HALT-RESULT :39 spins on READY with no end when the worker throws first; three hb-root children spun 33-93 min (pids 6374, 44605, 69534). Many tests spin on a flag the same way (rg 'while TASK:PAUSE repeat'). Plan: census, lead design from it, one lane per fix. Acceptance: every case runs in the suite's loaded process on the shipped bin/hb unless a process boundary is its subject (image save and restart, exec, exit code, signal, tty/stdin, process state fixtures cannot reset), stated beside it; no test wait outlives a worker that ended. Files: per child. Verify: per child; full suite at landing. Depends: none. Ownership: test harness process use. Claim: unassigned.
