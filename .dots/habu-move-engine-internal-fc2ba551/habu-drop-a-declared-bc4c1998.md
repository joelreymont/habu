---
title: "Drop a declared cell's row with a negative allot"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T15:06:42.358821+01:00"
---

Problem: `PERSISTED-PTR-VARIABLE X  -8 allot` leaves here at X's cell with its address row live (probe on the installed hb prints 0), so data laid there later is relocated as an address. Found by the cell design (.out/designs/design-cell-private.md). Acceptance: after a negative allot over a declared cell, no row covers what is laid there next: the build rejects it or the row goes with the cell; a test shows it. Needs a design. Depends: none. Claim: unassigned.
