---
title: "Raise http-test's flood stop on a condition"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T20:54:36.536346+02:00"
---

Problem: src/lib/net/http-test.f FLOOD-CASE (~2126-2149) raises HTTP:STOP a fixed FLOOD-LATE-MS (20 ms) past the second flood's deadline, then asserts the peer was reset after the stop (assert 301, FLOOD-ENDED @ raised >). When the sleep oversleeps or the worker's late budget (src/lib/net/http-request.f FILL ~205-213: the bytes queued at the deadline) parses sooner, the worker closes before the stop and 301 fails: 2 of 3 runs in the lane 6 fold-in's defect runs, 2026-10-31. Acceptance: the case raises its stop on an observed condition that proves the worker is reading late, with no fixed offset; 20 consecutive http-test runs under parallel load pass 301. Files: src/lib/net/http-test.f. Ownership: carl. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-flood.
