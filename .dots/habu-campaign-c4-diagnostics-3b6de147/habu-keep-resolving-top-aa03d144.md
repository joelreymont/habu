---
title: Keep resolving top-level names after a rejected one
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.964766+01:00"
closed-at: "2026-11-16T04:07:35.704720+01:00"
---

Problem: verify-source.f TOP-RESOLVE opens a stretch on a refused token, so --all-errors reports only the first of adjacent undefined names. Acceptance: every undefined top-level name gets its record, in order. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
