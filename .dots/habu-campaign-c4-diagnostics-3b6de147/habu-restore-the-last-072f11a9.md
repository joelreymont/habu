---
title: Restore the last token when a buffer ends
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:59:01.221989+01:00"
closed-at: "2026-11-16T04:07:34.179867+01:00"
---

Problem: src/habu/interpreter.f FRAME-OPEN/FRAME-UNLINK save and restore INP/INE/INB but not TKA/TKL, so after an evaluated or loaded buffer is released SRCLOC:TOKEN$ points into freed memory; include.f LOAD-CURRENT patches one release. Acceptance: a rejection after a load or evaluate returns reads the outer token; LOAD-CURRENT's patch is gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
