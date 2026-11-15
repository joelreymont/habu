---
title: "Delete render's generic suggestion"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.438570+01:00"
closed-at: "2026-11-16T04:07:36.417819+01:00"
---

Problem: render.f:1856 keeps the generic suggestion C4 deleted, reached by the fall-throughs DCODE E-REJECTED (:1372), 'rejected at' (:1547) and 'here' (:1583); MD-REASON$ (:1162) and MD-LABEL$ (:1264) have unreachable defaults. Acceptance: each fall-through and default that no record reaches is `76 die`; no record prints the generic suggestion. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
