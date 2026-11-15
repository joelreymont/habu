---
title: Reject an unreadable source once, where it is named
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.996594+01:00"
---

Problem: an unreadable required file prints check.f lines or an old packet; the loader's record names E-MISSING-SOURCE at the resolved path; the child throws E-SOURCE-READ bare. Acceptance: one E-UNREADABLE-SOURCE record at the path token, rc 74, in every mode. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-unread.
