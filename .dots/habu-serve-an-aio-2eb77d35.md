---
title: "Serve an AIO wait's halt without yielding"
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T10:49:10.130456+01:00"
---

Problem: src/lib/aio.f PARK (:1141-1144) calls TASK:PAUSE, which is sched_yield, after every TASK:WAIT in WAIT-LOOP and GROUP-LOOP, so a task woken by its completion gives the CPU back before it reads the result: on loaded spark a 0.1 ms PG query waits 4-24 ms (tim, /tmp/tim-aio-yield.md); task.f WAIT's comment tells loops to call TASK:PAUSE. Acceptance: PARK serves a pending halt through 0 TASK:HALT-UNWIND and never yields; a halted wait still gives its records back; aio-uring passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-aio.
