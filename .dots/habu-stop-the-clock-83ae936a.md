---
title: Stop the clock in prof-reset and bound prof-rate
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T17:54:38.903401+03:00"
---

Problem: two profiler defects, the same on both targets (`src/habu/prof.f` BPROF-RESET, BPROF-RATE, C-PROF-TIMER-FRAME; `src/habu/prof-x64.f` RESET-BODY, RATE-BODY, TIMER,; mirror `bootstrap/cg/prof.fs`).
(1) prof-reset clears PROF-TOT, PROF-OTHER and PROF-FOREIGN, then each record's counter, then the arena counters, with the clock still running. A tick between two of those stores survives in one field and not the other, so the report's identity (total = attributed + other + foreign) is off by one after the reset. prof-row already stops the clock around its work and restarts it only when PROF-ARMED is set.
(2) prof-rate stores n unchecked, and prof-on writes it into tv_usec of both itimerval halves with tv_sec 0. For n >= 1000000, or n < 0, setitimer refuses with EINVAL; the return is not read, prof-on marks the profiler armed, and the phase samples nothing and reports zeros.
Acceptance: prof-reset stops the clock, clears, and restarts it only when armed, as prof-row does. prof-on arms any positive interval, splitting n into seconds and microseconds. prof-rate refuses n < 0 with a named error; 0 keeps meaning the default. A setitimer that refuses the arm is named on fd 2 and fatal, like a refused arena mapping. Both targets behave the same.
Files: `src/habu/prof.f`, `src/habu/prof-x64.f`, `src/habu/prof-abi.f` (messages, exit code), `bootstrap/cg/prof.fs`, `test/gate-debug-lib.f`, `test/x86-64-kernel-prof.f`, `docs/x86-64.md` (prof table rows).
Verify: ARM64: a phase at 1500000 us over a busy loop of about 4 s samples at least two ticks; prof-reset in a hot loop at a 50 us rate, repeated 1000 times, leaves the identity exact; a negative rate is refused with its message. x86: the same through the booted `hb-x64-kernel-prof` images, plain and under the macOS layout shim. Full native gate on the ARM64 engine, since prof.f is baked.
Route: engine lane (worker-max: signal timing).
Ownership: krait (Intel lane).
