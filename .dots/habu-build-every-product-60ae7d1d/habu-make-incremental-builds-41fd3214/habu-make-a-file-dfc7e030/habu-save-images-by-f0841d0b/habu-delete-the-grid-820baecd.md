---
title: Delete the grid machinery and CELL-BODY
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:05.999975+02:00"
closed-at: "2026-11-04T11:48:47.885217+02:00"
close-reason: "Census 2026-11-04: goes with habu-make-a-file-dfc7e030, the cross-build whose product is data (docs/design.md \"Machinery being replaced\"). Grid machinery, CELL-BODY (src/core/cell-body.f:35 verified) and unit-format grid tags serve the byte scan and cache."
---

T3 (plan section T3): BCOMMA becomes plain ',', checker STORAGE-GRID/RECORD-GRID-*/grid unit-format tags, checker-owner-abi GRID offsets, include.f:1968-2003, src/core/cell-body.f and its tests and docs (Joel: 'Remove all of it'). Verify: rebuild, full suite, multi-generation convergence. Depends: T2. Claim: unassigned.
