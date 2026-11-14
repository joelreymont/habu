---
title: "Turn the checker's load-order defers into plain calls"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T10:37:10.488676+02:00"
---

Problem: src/core holds defers whose only reason is that their implementation loads later: 33 of checker.f's 57 (most filled from type-family.f and render.f, 3 from xref.f, check-walk.f's FACTS-READ-XT and NAMED-OPERAND-XT, cell-effects.f's CWIN-STATE, prefix-boundary.f's FIRST-RECORD-XT), 10 in sumtype.f and layout-buffer.f's STORAGE-CLEAR-XT. docs/design.md 'Machinery being replaced' replaces a hook with data the owner records or a plain call. Acceptance: no defer in src/core exists only because its implementation loads later; each such call is a plain call, and both engine generations build. Depends: none. Claim: unassigned.
