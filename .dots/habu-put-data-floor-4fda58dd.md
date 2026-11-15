---
title: "Put DATA-FLOOR-CELL and hb's strings on one table"
status: open
priority: 3
issue-type: task
created-at: "2026-11-09T17:59:25.292408+02:00"
---

Problem: Joel ruled 2026-11-05 that when the arm64 cross-build writes a working hb, the old hb builder, its string windows and DATA-FLOOR-CELL go; the linker-built hb still uses DATA-FLOOR-CELL and keeps hb's strings apart (linker.f:2931-3407, interpret.f:326, generated-declaration-dictionary.f:28, intern.f:145, habu2.f:7744). Acceptance: a design (Plan agent form) that puts hb's strings on the linker's one table and drops DATA-FLOOR-CELL, brought to Joel before a worker starts if it adds a public API or changes a saved format. Files: those named. Verify: the design cites each site; after it lands, hb rebuilds itself byte for byte. Depends: none (lands after arm64). Ownership: linker.f and the sites named. Claim: unassigned.
