---
title: Remove TRANSFER-CHECKED if nothing reaches it
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T20:29:52.587339+02:00"
---

Problem: src/core/checker.f TRANSFER-CHECKED (~28824 on ykrswwlx) has no caller by name in src/, tools/ or test/ (its definition and two comments only; review of the dead-reset commit, 2026-11-09), so TRANSFER-ROWS, the ORDER-P mapping and the order note above it may be as dead as CHECKER-RESET-SOURCE was. Acceptance: show on the linked hb whether anything reaches it (no BL/B to it in the engine text or live region, no cell holding its xt, as the reset's RCA did); if nothing does, delete it and every word only it calls. Files: src/core/checker.f. Verify: the grep and the image scan, then hb rebuilds and runs. Depends: the arm64 landing. Ownership: unassigned. Claim: unassigned.
