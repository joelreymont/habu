---
title: Exit 70 for every interpreter rejection
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:27:31.756411+01:00"
closed-at: "2026-11-16T04:07:36.222060+01:00"
---

Problem: src/habu/interpreter.f stops rejections with private statuses 74, 75, 76 and 78, against the rule that a rejection exits 70. Acceptance: every interpreter rejection exits 70; docs state no other status for them. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
