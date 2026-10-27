---
title: Scan only unchecked-window bytes for raw cells
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T12:39:37.291483+02:00"
---

Capture scans every undeclared create byte for code and mapping addresses (tools/native-unit-object.f ADMIT-SPAN; src/habu/aot-data-cell.f NONCELL-WINDOW? has grids only for declared spans), so ffs.f refuses raw-cell owner=FFS-S offset=4 value=0x300000000 for an integer that looks like an address. Once top-level stores are type-checked (087ee3fa), only bytes written inside an unchecked window can hold an unrecorded address: narrow the scan to those bytes. Rejected: changing ffs.f values, a high VA band, recording every , and !. Acceptance: ffs.f frames F144x2/F145/F340/F341 pass; a pointer stored inside an unchecked window is still refused. Workspace: carl lane after 087ee3fa lands.
