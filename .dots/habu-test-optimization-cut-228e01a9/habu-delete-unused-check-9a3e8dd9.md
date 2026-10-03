---
title: Delete unused check-run and multi-error origin words
status: open
priority: 4
issue-type: task
created-at: "2026-10-02T09:39:52.702781+02:00"
---

Problem (r4-usingloc lane, ab7fcef3, dot d3150b2e): tools/check-core.f CHK-RUN-N (~:1720) has no caller. src/core/checker.f MULTI-ERR-ORIGIN! (~:15281) and its MEO-* state (MEO-ON, MEO-BASE, MEO-NAMEC, MEO-BL/BC/BB, MEO-NAMEC@, MEO-APPLY) serve a whole-buffer multi-error driver that no production path uses: its only caller is test/engine-suite.f:1467 TR-ME-ORIGIN! and the MEO section after it (~:1473-1500); tools/check.f gets file positions from tools/diag-origin-core.f markers instead. Acceptance: census every reader of each word and cell (rg src lib tools test docs); delete CHK-RUN-N, and delete the MEO machinery with its whitebox section unless a production reader exists (then say which and keep it); no behaviour change: check-test, check-all-errors-test, gate-diagnostics, engine-suite (whitebox) rc 0; baked (checker.f): rebuild, g1 == g2, two-generation build. Base: after the master merge.
