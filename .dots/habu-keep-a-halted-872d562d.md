---
title: "Keep a halted task's own curl transfer in its cleanups"
status: active
priority: 2
issue-type: task
created-at: "2026-11-02T19:06:15.661566+02:00"
---

Problem: src/lib/net/curl.f AWAIT (:1857-1866) and its streaming wait (:1826-1838) read TASK:HALTED? after TASK:WAIT and ABANDON the transfer, but HALTED? stays true for the whole halted run, cleanups included (src/lib/task-test.f:1314; Zen worker.f EXIT-CLEAN reads it in AT-EXIT). So a fetch started inside a halted task's finally, AT-EXIT, DEFER-ENTER section or callback returns CURLE-ABORTED for the cleanup's own transfer; curl.f:1852-1854 still describes that old pattern. Same defect as aio's F1 (dot habu-aio-waits-keep-127bb200, lane carl-aio umzuqtzo, reviewed: a per-task parked cell set around PAUSE, and the wait's finally gives the transfer back only when the halt's throw comes out of PAUSE). Acceptance: the waits stop reading HALTED?; a halt while parked abandons exactly the parked transfer in a finally around the wait; a cleanup that fetches after a halt gets its result; the comment at :1852-1854 states the new rule in at most two lines. Regression: a curl-test case whose finally fetches a local URL after a halt and expects the body, failing before. Files: src/lib/net/curl.f, its test. Verify: the curl test file, tools/check.f -- src/lib/net/curl.f. Depends: habu-aio-waits-keep-127bb200 landed (reuse its pattern). Ownership: carl. Brief .out/briefs/curl-halt-parked-transfer.md. Claim: agent=carl workspace=.jj-ws.noindex/carl-curl
