---
title: A timed-out AWAIT-END in curl-test cannot name its case
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:30:37.363394+02:00"
---

Problem (curl re-review 2026-11-11, finding 7): src/lib/net/curl-test.f AWAIT-END (about line 1692) passes the fixed name 'curl-test AWAIT-END' to TEST-WAIT:POLL from 11 call sites, and POLL reads no test label by design (src/lib/test/wait.f EXPIRED), so a run that times out there does not say which case waited. Acceptance: AWAIT-END takes its caller's name for POLL; a timeout names the case. Files: src/lib/net/curl-test.f. Verify: curl-test rc 0. Depends: none. Ownership: unassigned. Claim: unassigned.
