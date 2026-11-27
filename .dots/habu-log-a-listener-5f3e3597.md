---
title: Log a listener that ends by a throw
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T20:47:36.203287+01:00"
---

Problem: src/lib/net/http.f LISTENER-BODY rethrows a throw from ACCEPT-UNTIL-STOPPED (a re-arm's REC-CLAIM or H-ROOM growth in src/lib/aio.f, or ENQUEUE's JOB-PUT, failing for want of memory); nothing logs it, ENDED-TASKS does not count it and KILL-TASKS drops the outcome, so a server that stops accepting says nothing and a test waiting on its end hangs. Acceptance: a listener that ends by a throw writes one log line naming the code before it leaves, and a test that forces the throw sees that line. Depends: none. Claim: unassigned.
