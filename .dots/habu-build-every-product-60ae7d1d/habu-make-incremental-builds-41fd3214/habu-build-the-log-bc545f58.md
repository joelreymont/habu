---
title: Build the log and path programs in turn in test/file-objects.f
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:43:40.350581+02:00"
closed-at: "2026-11-10T12:52:22.266106+02:00"
close-reason: "disproved by trace, no change: with LOG-CASE as L, P, L2, P2 the test passes under an engine without c1c72c21's cache marks and under one without c1efdd1a's six marks, while the six-build order fails at P2 (prints the builder's path) and at L2 (rc 74, app-end.f threw 7122); a bare build must come right before the second build. Why a program with requires does not stand in for it is not traced. Times: 7.7 s wall as is, 6.5 s without the bare builds."
---

Problem: test/file-objects.f LOG-CASE :524-533 builds bare-main.f twice (M, M2) only so the next build compiles app-end.f again; the test went from 4.4 s to 8.6 s when P, M2, P2 were added (c1c72c21). Any other program's build rewrites app-end.f's object, so L, P, L2, P2 should need no bare build (inferred). Acceptance: LOG-CASE builds L, P, L2, P2; with c1c72c21's cache marks removed the P2 label fails, with c1efdd1a's six marks removed a label under L2 fails; bare-main.f removed if unused. Files: test/file-objects.f, test/fixtures/file-objects/. Verify: the two traces' failing labels; test: ok on the restored tree; wall and CPU time before and after. Depends: c1c72c21 landed or stacked under it. Ownership: LOG-CASE and its header lines. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-order.
