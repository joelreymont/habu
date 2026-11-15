---
title: Keep resolving top-level names after a rejected one
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.964766+01:00"
---

Problem: verify-source.f TOP-RESOLVE opens a stretch on a refused token, so --all-errors reports only the first of adjacent undefined names. Acceptance: every undefined top-level name gets its record, in order. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-stretch.
