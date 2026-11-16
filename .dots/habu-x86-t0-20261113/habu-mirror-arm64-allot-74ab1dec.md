---
title: Mirror arm64 allot and comma on x86
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.605104+01:00"
---

Problem: x86 ALLOT-BODY skips the checker's GRID-REVOKE-OFF and DP-LOWER, on a negative count, and x86 comma keeps no COMMA-RUN/END cells (address-cell-rollback red on x86). Acceptance: address-cell-rollback passes on x86. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
