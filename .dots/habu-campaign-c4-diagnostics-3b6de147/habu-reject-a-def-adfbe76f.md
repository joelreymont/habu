---
title: Reject a definition whose name is missing before its signature
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:37:06.076409+01:00"
closed-at: "2026-11-16T04:07:34.702279+01:00"
---

Problem: ': ( -- ) ;' prints 'uncaught throw code -8573' (a compiler check) under the loader and check.f. Acceptance: one E-MISSING-NAME record at the colon's operand, rc 70. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
