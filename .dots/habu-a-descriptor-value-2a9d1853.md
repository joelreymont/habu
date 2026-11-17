---
title: A descriptor value holds only what libc reads
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T09:55:08.440096+01:00"
---

Problem: TCP4:>CONNECTION, TCP4:>LISTENER, UDP4's socket converter and SERIAL:>HANDLE take any cell, and libc reads only its low 32 bits: $100000002 TCP4:>CONNECTION TCP4:CLOSE answers ok and closes fd 2 (spark probe). Acceptance: a cell wider than an int cannot become a descriptor value unless the program spells the narrowing; no runtime check. Depends: none. Claim: unassigned.
