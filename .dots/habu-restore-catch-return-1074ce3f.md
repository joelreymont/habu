---
title: Restore catch return-stack frames
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-14T18:58:34.030428+02:00\""
blocks:
  - habu-owner-seal-persist-1f23e205
  - habu-honor-set-curr-fbd17193
---

Context: src/habu/habu1.f BCATCH/BTHROW do not preserve RSP-CELL or LOOPSP-CELL across nested throw/evaluate recovery; tools/build-fixpoint-test.f F170 reproduces the resulting capture-state corruption before the AOT-owner gate. Fix: restore exact return-stack and loop-stack state on normal and thrown exits, including src/habu/habu2.f evaluation recovery interactions and bootstrap/cg/forth.fs parity. Acceptance: minimal negative checked regression proves nested BFT-STEP/TTHROWSQ restoration, native and bootstrap implementations agree, process capture reports the real child outcome, and the dependent owner persistence gate is green.
