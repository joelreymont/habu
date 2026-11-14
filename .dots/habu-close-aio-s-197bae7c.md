---
title: "Close AIO's host when STOP meets a loop error"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:06:36.259027+02:00"
---

Problem: AIO STOP (src/lib/aio.f:1131-1141) rethrows the loop task's err before HOST-CLOSE and '0 RING-LIVE !', so a loop that failed leaves RING-LIVE 1 and the host's descriptors open; a later START sees a live ring. Acceptance: STOP closes the host and clears RING-LIVE on every outcome, then rethrows the loop's error; a test makes the loop fail, calls STOP, sees the error, and starts again. Files: src/lib/aio.f, src/lib/aio-test.f. Verify: src/lib/aio-test.f and the net tests that start AIO (curl-test.f). Depends: none. Ownership: carl, worker lane. Claim: unassigned.
