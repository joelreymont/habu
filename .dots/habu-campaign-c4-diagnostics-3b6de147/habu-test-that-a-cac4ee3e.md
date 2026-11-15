---
title: Test that a caught nested record is not revived
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:16:44.884564+01:00"
closed-at: "2026-11-16T04:07:33.865113+01:00"
---

Problem: no test pins that a nested require's rejection the program catches is not printed for a later uncaught failure of the same code (the throw on, swmyntvq). Acceptance: a suite case where a caught nested require is followed by an uncaught throw of the same code prints only the second's record. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
