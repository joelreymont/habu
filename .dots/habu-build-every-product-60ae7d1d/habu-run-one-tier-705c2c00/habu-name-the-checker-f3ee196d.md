---
title: "Name the checker's orphan-closer refusal"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T19:23:10.804987+02:00"
closed-at: "2026-11-10T21:52:35.307754+02:00"
close-reason: "done: landed as 8f08f970: MD-CLOSES-NOTHING (render.f:1160, :1335); an orphan `then` prints \"this word closes nothing open here\""
---

Problem: the checker refuses an orphan closer (THEN with no IF) as generic E-REJECTED 'unknown_rejection' with an empty reason (tools/check.f --json-errors; src/core/checker.f CF-THEN ~:18853). Acceptance: a named code and reason, as the JIT's 'control-flow closer without opener'. Claim: unassigned.
