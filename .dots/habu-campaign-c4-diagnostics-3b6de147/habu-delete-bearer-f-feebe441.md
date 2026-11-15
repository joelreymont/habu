---
title: "Delete bearer.f's UNANSWERED if nothing needs it"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T13:16:44.873384+01:00"
closed-at: "2026-11-16T04:07:36.463761+01:00"
---

Problem: src/lib/net/bearer.f ~:707 says a bare OPTION:NONE in place of UNANSWERED is rejected at the 2drop (rc 70); copies with OPTION:NONE at :734, :739, :750, :753 all check clean, so the comment is false and UNANSWERED (:708) may carry no reason. Acceptance: UNANSWERED and its comment deleted, or the comment states the reason it is kept, true on this tree. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
