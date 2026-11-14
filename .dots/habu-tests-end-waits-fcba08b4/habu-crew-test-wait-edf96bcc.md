---
title: task-test NEW-WAIT only forwards to POLL
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T20:51:23.723850+01:00"
---

Problem: src/lib/task-test.f:2325 NEW-WAIT only calls TEST-WAIT:POLL. Acceptance: its callers call TEST-WAIT:POLL and it is deleted; the tasking-threads suite passes. Depends: none. Claim: unassigned.
