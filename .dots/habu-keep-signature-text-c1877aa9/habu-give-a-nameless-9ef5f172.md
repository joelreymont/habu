---
title: Give a nameless datum its own extent
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T23:28:05.983263+01:00"
---

Problem: a row laid at top level with no name (src/core/rollback-cells.f MARK, src/core/generated-declaration.f EXPLAIN, src/lib/error.f ROW,) has no extent of its own: it is glued to the extent before it, which keeps it live. Acceptance: a new word (name Joel's, asked) aligns and starts a datum, an extent the linker learns from a scratch-cell chain under one engine cell beside COMMA-BASE-CELL (EXTENTS! adds kind-4 anchors); MARK, EXPLAIN and ROW, call it; the card's section 4 states it; the reproducer holds no EXPLAIN text. Depends: habu-keep-a-stripped-d3015a90. Claim: unassigned.
