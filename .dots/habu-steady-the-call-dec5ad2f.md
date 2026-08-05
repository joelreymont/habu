---
title: Steady the CALL-FAN-BIG timing row
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T20:28:53.244998+02:00"
---

The CSE landing run measured CALL-FAN-BIG's spread at 1242/1000 — noisy enough that its time column is not currently usable for verdicts. Find why (alignment? branch-predictor sensitivity of the five inlined copies?) and either steady the measurement or mark the row's time informational-only. Found 2026-08-05.
