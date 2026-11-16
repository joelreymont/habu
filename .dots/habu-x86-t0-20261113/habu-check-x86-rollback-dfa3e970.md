---
title: Check x86 rollback against the checker grid
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.765848+01:00"
---

Problem: arm64 EMIT-ROLLBACK calls the checker's GRID-REVOKE-OFF when it lowers DP; no x86 rollback does. Acceptance: a reproducer shows whether x86 reaches a forgotten text or cell, and any gap is fixed. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-xg1 (hunt).
