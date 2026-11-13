---
title: Hold the http flood case under load
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T23:01:38.737827+01:00"
---

Problem: the suite http (src/lib/net/http-test.f) failed once in a full suite run on spark, at FLOOD-CASE's LATE-AWAIT: 'http-test LATE-AWAIT: a deadline expired', uncaught throw -2502, exit 67; alone it passes 10 of 10 on the installed hb and on a candidate. Acceptance: a hunt names the cause as path:line (the test's deadline or the server under load); then the http suite passes in every full suite run, its waits keyed to the events they wait for. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-http (read-only hunt).
