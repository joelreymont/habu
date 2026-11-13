---
title: "Turn the checker's load-order defers into plain calls"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T10:37:10.488676+02:00"
---

Problem: src/core/checker.f declares defers that later core files install only because checker.f loads first: 53 from type-family.f (TFAM-HOOK-INSTALL, :5975), 13 from render.f, 3 from xref.f and one each from layout-valid.f, cell-effects.f, prefix-boundary.f, layout-buffer.f and sumtype.f. docs/design.md 'Machinery being replaced' replaces a hook with data the owner records or a plain call. Acceptance: no defer in src/core exists only because its implementation loads later; each such call is a plain call, and both engine generations build. Depends: none. Claim: unassigned.
