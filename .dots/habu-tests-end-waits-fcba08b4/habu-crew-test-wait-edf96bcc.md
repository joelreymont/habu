---
title: crew-test WAIT and task-test NEW-WAIT only forward to POLL
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T20:51:23.723850+01:00"
---

Problem: after the waits campaign src/lib/crew-test.f:42 WAIT and src/lib/task-test.f:2325 NEW-WAIT only call TEST-WAIT:POLL. Acceptance: their callers call TEST-WAIT:POLL and both words are deleted; crew and tasking-threads suites pass. Depends: the waits campaign commit. Claim: unassigned.
