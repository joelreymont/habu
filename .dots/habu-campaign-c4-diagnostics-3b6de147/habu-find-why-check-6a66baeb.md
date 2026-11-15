---
title: Find why check.f outlives SIGTERM on a full dictionary
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:19:45.355768+01:00"
closed-at: "2026-11-16T04:07:36.201476+01:00"
---

Problem: timeout 10 bin/hb --load tools/check.f -- FILE on a dictionary-filling file ran 1m45s and exited 124: SIGTERM did not end it for about 95 s. Acceptance: the cause, at file:line, with evidence. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
