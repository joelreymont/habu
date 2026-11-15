---
title: Reject an unreadable source once, where it is named
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.996594+01:00"
closed-at: "2026-11-16T04:07:34.564070+01:00"
---

Problem: an unreadable required file prints check.f lines or an old packet; the loader's record names E-MISSING-SOURCE at the resolved path; the child throws E-SOURCE-READ bare. Acceptance: one E-UNREADABLE-SOURCE record at the path token, rc 74, in every mode. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
