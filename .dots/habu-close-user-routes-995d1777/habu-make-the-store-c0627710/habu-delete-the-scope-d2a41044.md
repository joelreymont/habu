---
title: Delete the scope words nobody calls
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:18:55.084043+01:00"
---

Problem: once tests reach checker scopes through CHECK:SCOPED, CHECKER-SCOPE-START and CHECKER-SCOPE-FINALIZE (src/core/check-walk.f:1623, 1637) have no caller in src/ or tools/. Acceptance: both are deleted, after a case-insensitive caller search. Depends: habu-restate-tests-off-5cb50894. Claim: agent=carl workspace=.jj-ws.noindex/carl-wrap2.
