---
title: Check a saved image by declared type
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:05.988865+02:00"
closed-at: "2026-11-04T11:48:47.866905+02:00"
close-reason: "Census 2026-11-04: goes with habu-make-a-file-dfc7e030, the cross-build whose product is data (docs/design.md \"Machinery being replaced\"). Save check over captured memory (aot-closure.f, AOT-DATA-TEXTPTR-CHECK still present, verified) goes with the window build."
---

T2 (plan section T2): aot-closure.f XTD-ROW deterministic; delete CELL-LITERAL?, LITERAL-BYTE?, CELL-MAPPED?, AOT-DATA-TEXTPTR-CHECK, SCAN-DATA-CELL, AOT-CARRIED-CELLS-CHECK; classify each span producer cell (ffi-abi.f, ffi-callback.f, task.f, compiler/native/string.f, include.f, aot-capture.f keep-row) as a scalar or an address already in the registry; tests per plan. Verify: those tests, full suite, Tender's server image build. Depends: T1b. Claim: unassigned.
