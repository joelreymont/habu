---
title: "Take SPAN's bounds tests out of default code"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:35:45.446724+01:00"
---

Problem: src/lib/span.f throws E-SPAN-RANGE on every call for a narrowing outside the span (:35), a byte index (:84) and CELL-AT's cell index (:97), E-SPAN-LENGTH for a negative length (:42, :108) and E-SPAN-CAPACITY for a copy longer than its destination (:110): checks against program errors that default code must not run (AGENTS.md Changes). Callers whose index is in range by construction pay it on every access (fmath-basis.f's work cells, 43 CELL-AT lines). Acceptance: as VEC:AT's (dot d265dedb): no bounds test in default code; each caller whose index comes from input states and tests its own range. Depends: none. Claim: unassigned.
