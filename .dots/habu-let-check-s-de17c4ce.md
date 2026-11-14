---
title: "Let CHECK's store and cell rows grow"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T19:53:36.739987+01:00"
---

Problem: src/core/checker.f ROWS and CELL-ROWS are fixed 64-row arrays (ROW-CAP :2473, CELL-ROW-CAP :2542); ROW+ :2513 and CELL-ROW+ :2562 die 'checker: store rows full' / 'cell rows full' past them, a limit Habu set itself. The engine registers 35 store rows and 30 cell rows (checker.f and type-family.f); CHECK:STORE+, TABLE+ and CELL-ROW+ are public. SAVED-PTRS (:2479) is sized by ROW-CAP too. Acceptance: any number of rows register; the two caps and their dies are gone; a warm engine build still equals a cold one when checker.f registers one more row (the test/warm-engine.f procedure with such a pad). Depends: none. Claim: unassigned.
