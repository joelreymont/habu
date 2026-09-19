---
title: Re-arm PostgreSQL image cleanup after each capture
status: open
priority: 1
issue-type: task
created-at: "2026-09-20T02:02:45.051781+03:00"
---

lib/db/pq.f FORGET-HANDLES retires connection/result slots but does not reset REGISTERED; IMAGE-LIFECYCLE:PREPARE consumes the hook, so a later CONNECT skips registration. Source finding from the FFI lifecycle reduction (9c283543). Claim: alder, .jj-ws/alder-lifecycle-followups; Hazel says fix after the current stripped-lifecycle/FFI/deadline stack. Measure repeated connect/prepare cycles, reset the registration at its owner, and prove subsequent captures retire slots and release arenas without stale process pointers. Keep scope to this lifecycle defect and its fixture.
