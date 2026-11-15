---
title: Read PARSE-COMPLETE before the use handlers run
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:41:12.184613+01:00"
closed-at: "2026-11-16T04:07:34.827181+01:00"
---

Problem: CHECK-UNJUDGED! (src/core/check-walk.f ~:1935) reads PARSE-COMPLETE after CHECK! ran PUBLISH-EXIT, and a use handler that starts a check sets it to -1 (:1342). Acceptance: a reproducer shows what that changes, and the read takes the definition's own value. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w49parse.
