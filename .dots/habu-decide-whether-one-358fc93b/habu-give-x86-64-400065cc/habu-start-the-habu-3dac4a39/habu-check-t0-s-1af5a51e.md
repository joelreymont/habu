---
title: "Check T0's bind count against the certificate"
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:04:30.080122+02:00"
---

T0 counts {: binds in COMPILE-TEXT but never compares with NFETCH:BINDS as tier 1 BINDS-CK does (elaborate.f:1764); a shortfall or stale capture passes silently. Workspace: jerry-t0-kw.
