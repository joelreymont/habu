---
title: Retire PART-GUARD and PART-PATH
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T15:48:07.355821+01:00"
closed-at: "2026-11-14T16:06:18.056411+01:00"
---

Problem: CHECKER-RESOLVE:PART-GUARD and PART-PATH (src/core/checker.f) are public: a program calls them, rc 0, from a body and at top level. Their last caller is src/habu/verify-source.f. Acceptance: each is E-UNDEFINED to programs on the product; test/name-length-test.f reaches the path refusals through package. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-guard.
