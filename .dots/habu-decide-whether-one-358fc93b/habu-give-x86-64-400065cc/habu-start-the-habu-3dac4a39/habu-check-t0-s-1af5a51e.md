---
title: "Check T0's bind count against the certificate"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T14:04:30.080122+02:00"
closed-at: "2026-11-02T11:34:09.124891+02:00"
close-reason: "Landed 4419bc533d 'Read T0's widths and binds from the checker': src/habu/t0.f:566-567 E-T0-BINDS matches tier 1's BINDS-CK."
---

T0 counts {: binds in COMPILE-TEXT but never compares with NFETCH:BINDS as tier 1 BINDS-CK does (elaborate.f:1764); a shortfall or stale capture passes silently. Workspace: jerry-t0-kw.
