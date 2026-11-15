---
title: Give each E-PKG-CONTEXT throw its record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.508285+01:00"
closed-at: "2026-11-16T04:07:36.044800+01:00"
---

Problem: E-PKG-CONTEXT (7136, checker.f:1746) is still a bare throw at checker.f:12892, 12956, 12993, 13200, 13290, 13295, 13297, 13314, 13318, 13359, 13360, 13367, 13376, which one catcher turns into a generic record (interpreter.f:1021, 821); CHECKER-PKG-CONTEXT-REJECT (checker.f:1785-1787, callers 1812, 14017, 21612) writes 'hb: no authenticated package context' to fd 2. Acceptance: each site writes its own placed record through REJECTION; the constant, the catcher and the fd 2 line are gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
