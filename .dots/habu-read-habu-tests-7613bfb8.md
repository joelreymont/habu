---
title: "Read Habu tests' captured logs whole"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T11:30:52.468274+02:00"
---

Problem: Habu's tests that capture stderr (src/lib/log-test.f, sqlite-test.f, pg-test.f, net/ws-test.f, net/model-test.f, net/mcp-test.f, net/http-test.f, net/test-issuer-test.f, secret/linux-test.f, fs-mutate-test.f, test/stderr-test.f FILE$) read the capture back into fixed buffers with FS:READ-ALL, so a regression that logs a line twice throws E-FS-CAPACITY out of the test and leaves the capture file behind instead of failing one check. Acceptance: each reads its capture through STDERR:CAPTURING (src/lib/test/stderr.f); no capture is read into a fixed buffer. Depends: none. Claim: unassigned.
