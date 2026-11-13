---
title: "Drop what layout-valid.f's removal left"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T17:57:04.334760+01:00"
---

Problem: test/layout-valid-hook-forge.f, layout-valid-walk-forge.f and layout-valid-desc-forge.f name words defined nowhere, so test/program-diagnostics-test.f's rows for them pass whatever the engine does; TYPE-FIELD's TX-DEPTH, FIND, EACH, NAME$, CELLS@ and ALIGN@ have no production caller. Acceptance: a forge test whose subject is gone is deleted with its rows, one that still tests live behaviour names live words and fails without it, the uncalled TYPE-FIELD words go after rg finds no production caller, and program-diagnostics-test.f passes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-lvleft.
