---
title: Fix the proc-pty nested-step race
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T19:23:10.836826+02:00"
closed-at: "2026-11-04T11:48:49.808193+02:00"
close-reason: "Census 2026-11-04, done: 6b969738 (2026-11-01, after the dot) ends every PTY step at its prompt: 0 of 288 failed; inferred same race."
---

Problem: test/proc-pty.f assertion 124 ('99 throw') failed once under load: PTY-STEP-NESTED's PROBE-BARRIER does not wait for its own ' ok', so STEP-LN's drain reads it into the next case. Acceptance: the barrier waits for its own prompt; assertion 124 stable under load. Claim: unassigned.
