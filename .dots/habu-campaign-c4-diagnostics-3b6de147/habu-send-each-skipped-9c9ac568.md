---
title: Send each skipped-arm stop with its place and status
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T23:46:57.175812+01:00"
closed-at: "2026-11-16T04:07:34.094691+01:00"
---

Problem: under --all-errors a walk stop kept for a skipped arm is lost when the composition ends non-70 (tools/check-core.f CHK-LINT-ALL rethrows before CHK-SEND-UNREAD), is judged at DISCOVER:LAST-TOKEN rather than where its record stands (tools/check-verify-core.f CHK-DISCOVER vs source-discovery.f SD-GROUP-STOP), and always ends rc 70 even for a 74 stop (re-review 9b findings 1, 2, 4, 6). Acceptance: each such stop's record is sent once with the status plain check gives, pinned in test/diag-position-test.f. Depends: none. Claim: agent=W160 workspace=.jj-ws.noindex/jerry-w160
