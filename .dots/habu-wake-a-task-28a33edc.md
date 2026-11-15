---
title: "Wake a task's AIO wait from one place"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T13:14:37.364178+01:00"
---

Problem: src/lib/net/ws.f (WAKE-R, WAKE-W, POKED, PIPED, POKE, PIPE-OPEN, PIPE-CLOSE), src/lib/net/curl.f and src/lib/net/http.f (the listener's wake for parked connections) each keep their own pipe to wake a task parked in AIO:AWAIT-ANY on its group. Acceptance: aio.f offers one way to wake a group's waiter, the three copies are gone, and http-test.f, ws-test.f and curl's tests pass. Depends: none. Claim: unassigned.
