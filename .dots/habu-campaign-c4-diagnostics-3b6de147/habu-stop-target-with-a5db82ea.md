---
title: Stop ?TARGET with 74, placed only at its token
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T17:19:45.297088+01:00"
closed-at: "2026-11-16T04:07:33.947970+01:00"
---

Problem: src/core/include.f:2622-2631 ?TARGET stops with 70, not INCLUDE-IO-RC (74) the card states, and places its record at the caller's token when a definition calls it. Acceptance: ?TARGET stops with 74; called from a definition its record has no place; load-reject-diag pins both. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
