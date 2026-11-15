---
title: Pin every nameless declaration record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T23:45:20.633814+01:00"
closed-at: "2026-11-16T04:07:35.173649+01:00"
---

Problem: only the load's nameless NEWTYPE is pinned (test/type-decl-suite.f); the nameless SUMTYPE and VARIANT records and the tools/check.f paths (verify-source.f RECORD-NEWTYPE, RECORD-SUMTYPE-BODY) are not, and name-length-test.f checks the caret as a substring (re-review 9a finding 4b). Acceptance: each keyword's record pinned on the load and check paths with its place, caret and help. Depends: none. Claim: agent=W145 workspace=.jj-ws.noindex/jerry-w145
