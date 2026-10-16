---
title: Wait for the request retire in http-test
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T13:31:23.176102+03:00"
---

Problem: src/lib/net/http-test.f:1327 (LARGE-CONTENT-CASE), with the same pattern at :1356 and :1390, asserts that the old request handle is retired (HTTP:BODY$ on OLD-REQUEST throws HTTP:E-HANDLE) as soon as the client holds the response. The server worker sends the response before it retires the request, so under load the client can call BODY$ before the retire and the TTHROWSQ fails. Gate run on 26a32b98 plus the stack at load 200-340: F143, assertion 143 is that TTHROWSQ (mapped with a traced copy, ~/.cache/tmp/jerry-http143/); it passed 6/6 on master and 3/3 on the stack at load ~155 (~/.cache/tmp/jerry-repro/). Acceptance: fix the layer that is wrong, either the server retires before the response that ends the request is observable, or the test waits for the retire before it asserts; no deadline is raised. Files: decided by the root cause (src/lib/net/http-test.f, or src/lib/net/http.f with carl). Verify: the http row passes; a copy that delays the retire shows the assertion holds. Depends: none. Ownership: carl's http area (he has been told). Claim: unassigned.
