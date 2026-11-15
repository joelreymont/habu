---
title: "Keep a rethrown code's record in every spelling"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:19:45.383661+01:00"
closed-at: "2026-11-16T04:07:35.742865+01:00"
---

Problem: only a throw of the catch's own local throws on (src/habu/t0.f THROW-ON, CATCH-BOUND?), so 'catch dup 0<> if throw then drop' loses the caught rejection's record for a generic E-THROW. Acceptance: a rethrow of a caught code keeps its record however the code reaches throw. Approach (Joel, 2026-11-15, chose 'Checker tracks code'): the checker gives catch's answer a caught-code type that survives dup, locals and branches, and a throw of it keeps the record; T0's pattern match goes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (commit kmwpswzw, stacks after the third merge, closes at landing).
