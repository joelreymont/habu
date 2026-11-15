---
title: Find why DRIBBLE-LOW-SPEED timed out beside a broken RESET
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T12:38:36.061236+02:00"
---

Problem: src/lib/net/curl-test.f's DRIBBLE-LOW-SPEED failed 4 of 10 runs with 'curl low-speed code: 28' (asserts 187-190) while CURL:RESET's header-list step was removed, though that test never calls RESET; 0 of 17 runs failed with the step in place. Either one test's failure leaks into another through the shared server, or the test is timing-sensitive under load. Acceptance: the cause shown by evidence from a reproducer, and a fix brief. Depends: none. Claim: unassigned.
