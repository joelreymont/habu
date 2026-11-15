---
title: "Find why SERIAL:READ with timeout -1 throws E-AIO-STATE"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:30:07.298890+01:00"
closed-at: "2026-11-16T04:07:36.191001+01:00"
---

Problem: test/serial.py ~:353-364 expects -9110 for length 1, timeout -1, handle 0; SERIAL:READ and SERIAL:WRITE throw -9292 (E-AIO-STATE), so serial.py stops there. Acceptance: the cause found and fixed; serial.py runs green. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
