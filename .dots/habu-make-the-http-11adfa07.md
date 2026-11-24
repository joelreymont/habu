---
title: Make the HTTP stop keep its bound
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T17:16:11.009107+02:00"
---

Problem: src/lib/net/http-test.f PARKED-WORKER-CASE (:2216) and FULL-QUEUE-CASE (:2306) fail in about 1 of 3 runs of the http suite alone on 37e207b0: HTTP:STOP (src/lib/net/http.f:1541) overruns STOP-CEILING-MS, kills nothing and every task, the parked one included, ends itself, so the stop waited out the 10 s park instead of STOP-BOUND-MS (http.f:1100, AWAIT-TASKS :1130). Acceptance: the cause found and fixed; the http suite passes repeated runs. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/bob-httpstop.
