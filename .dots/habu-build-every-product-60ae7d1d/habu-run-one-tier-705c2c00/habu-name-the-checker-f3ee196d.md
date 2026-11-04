---
title: "Name the checker's orphan-closer refusal"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T19:23:10.804987+02:00"
---

Problem: the checker refuses an orphan closer (THEN with no IF) as generic E-REJECTED 'unknown_rejection' with an empty reason (tools/check.f --json-errors on /tmp/jerry-big/gaps/orphan-THEN-t0.f; src/core/checker.f CF-THEN ~:19812). Acceptance: a named code and reason, as the JIT's 'control-flow closer without opener'. Claim: unassigned.
