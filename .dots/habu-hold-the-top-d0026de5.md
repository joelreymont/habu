---
title: Hold the top-level row live for word steps
status: open
priority: 2
issue-type: task
created-at: "2026-10-26T19:23:06.912134+02:00"
---

Problem: each top-level word step loads the whole saved interpret row (the checker's type row over the top-level stack) and saves it back: src/core/checker.f:31963 CHECKER-TOP-LOAD instantiates the saved row and :31994 CHECKER-TOP-SAVE copies it again, so a step costs time in proportion to the live top-level depth. Literal events already skip the step and write only their change (the comment above TOP-READ-NODE, :32060). No measured case shows word steps costing a real load; literals were the measured case. A design exists only in outline: a held row, a step window, rerun, splice, flush at NEW and USIGS-GROW, compaction; its Plan report and summary file are gone.

Acceptance: a measurement on the real loader: time per top-level word step against top-level depth (for example 10, 100 and 1000 live cells, each with 10 000 word steps), and whether any tree or consumer load reaches a depth where the cost shows. With such a case attached, this dot becomes the design work; without one, it closes as not needed.

Files: none for the measurement; src/core/checker.f for a design.

Verify: the timing table and the program that produced it, recorded in the dot.

Depends: none.

Ownership: unassigned. Claim: unassigned.
