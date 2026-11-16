---
title: Reject an opaque execute under --verify-only
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T16:26:06.254389+01:00"
---

Problem: the top-level line `s" 1 drop" ' evaluate-closed execute` is rejected E-EXEC-OPAQUE-XT at 1:30 by --load and tools/check.f (rc 70), but tools/check.f --verify-only exits rc 0 with no record. Acceptance: the three readers agree on it; a test pins the --verify-only outcome. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-o8.
