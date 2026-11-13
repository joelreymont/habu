---
title: Skip the DATA store guard outside DATA
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T06:03:12.121701+02:00"
---

Problem: every !, +! and c! enters (PROT-SPAN) (src/habu/habu1.f:5256), 63.6M times per warm engine build; 99.986% exit at once, below DATA-BANDS:LO or at or above HI, so the guard's cost is the call. Acceptance: a design that elides the guard for stores the compiler proves lie outside DATA, or makes the outside test inline; guarded entries measured before and after. Depends: none. Claim: unassigned.
