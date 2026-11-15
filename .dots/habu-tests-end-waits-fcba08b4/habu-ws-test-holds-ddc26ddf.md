---
title: ws-test holds a stalled accept by a clock
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T20:51:23.647654+01:00"
---

Problem: src/lib/net/ws-test.f STALL (:636-638 on lane rrlllootk) holds the stalled accept with HOLD-MS TASK:SLEEP, a clock, and CLOSES-WITHIN (:664) names a deadline it no longer has. Acceptance: the hold ends when the WS task under test ends, with no sleep; CLOSES-WITHIN is renamed for what it checks; the websocket suite passes. Depends: the waits campaign commit. Claim: unassigned.
