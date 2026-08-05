---
title: "Pin the new chain's byte counts once its coverage settles"
status: open
priority: 3
issue-type: task
created-at: "2026-07-31T18:58:51.987777+02:00"
---

test/compiler/codegen-compare-baseline.txt still carries only the old emitter's rows; the new chain's byte counts are recomputed live on every run and checked against the old column for equal results, not against a committed number. That is the right call today - the chain gains capabilities often and every gain moves the numbers, so a pinned new column would red on progress. It is the wrong call once the subset stops moving: a byte count is exact and deterministic, a silent regression from 12 bytes to 16 for ADD3 would pass unnoticed today, and 'fewer bytes' is the goal's own claim. Wanted, when the straight-line subset stops changing week to week: write the new rows into the committed table (CODEGEN-REPORT:BASELINE$ already renders a path per row and CODEGEN-BASELINE already parses the word new), compare their sizes exactly and their costs with the same tolerance band, and keep tools/codegen-compare-test.f's live result check as well. Do not pin the costs before dot habu-measure-the-new-dbaf82dc replaces the FFI trampoline: the new cost column is dominated by the harness's own call path until then.
