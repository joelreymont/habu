---
title: Reject a reserved name in CHECK-CANDIDATE!
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T13:12:30.486275+01:00"
---

Problem: src/core/check-walk.f:1966 CHECK-CANDIDATE! accepts the reserved `I` as a definition name, while --load rejects the same definition with E-RESERVED-DEFINITION (src/core/checker.f:14172), so the check and the load disagree. Acceptance: tools/check.f --verify-only on `: I ( -- ) ;` rejects it with the load's E-RESERVED-DEFINITION diagnostic. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-rsvd.
