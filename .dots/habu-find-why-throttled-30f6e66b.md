---
title: Find why throttled http-test answers status -2
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T13:31:23.215023+03:00"
---

Problem: src/lib/net/http-test.f run under taskpolicy -b failed F6 and F7 with status -2 where 200 is expected, before and after the reset fix (4296ed9a); the throttled process had 1.1 s of CPU in 50 minutes and was stopped, and the cause was not examined. If -2 is a client deadline mapped to a status, it is an assertion standing in for a deadline (docs/gate.md:428-441); otherwise it is a real defect. Acceptance: the cause is reduced and fixed at the responsible layer, or shown to be a deadline and thrown as one; no deadline is raised. Files: decided by the cause. Verify: the reduced case before and after; the http row rc 0. Depends: none. Ownership: carl's http area. Claim: unassigned.
