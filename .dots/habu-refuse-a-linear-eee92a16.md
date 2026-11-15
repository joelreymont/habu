---
title: Refuse a linear token top-level constant takes
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T18:40:07.849551+02:00"
---

Problem: at top level `5 LT:NEW constant K K .` prints 5, rc 0: `constant` takes a linear token, because TOP-TAKE-STEP (src/core/check-walk.f, in royxvwoy "Carry multi-cell values at top level") judges only the item's width. Acceptance: `constant` over a linear item at top level is refused, rc 70, naming `constant`, with a test in test/export-package.f. Depends: royxvwoy landed. Claim: unassigned.
