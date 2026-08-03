---
title: "Retire the flaky cost assertion on the third corpus's T-SUM row"
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T12:37:57.119045+02:00"
---

PRE-EXISTING GATE FLAKE, found while building the fourth codegen corpus and reproduced on the file as committed. tools/codegen-compare-test.f:747 asserts `s" CODEGEN-CORPUS3:T-SUM" COSTLIER? TFALSE`, which is assertion 105 of that suite. T-SUM's two columns are old 22.5 ns and new 20.6 ns with the entry taken off - a margin of nine per cent - so a transient that lands on the new column and not the old one flips it. Measured on an idle 12-core Apple Silicon host: the file AS COMMITTED (extracted with jj file show -r @-, no edit of mine) failed 1 run in 10 alone and 3 runs in 10 while a maki pass had the cores, always at case 105 with the label of line 747, always 'expected false got true'. The same rate appears with the fourth corpus's cases added, at the same case number, so the fourth corpus neither causes it nor makes it worse. This is exactly what the head of tools/codegen-compare-test.f says must not be scheduled: 'a scheduled run that can fail for host load is worse than no scheduled run at all.' The other four TFALSE rows beside it have the same shape and margins between 1.1x and 1.4x, so they are candidates too; T-SGD!'s TTRUE at 1.2x is the same question the other way up. FIX: either drop the assertions whose margin is under about 2x and say in the prose which rows are draws (what tools/codegen-compare-test.f does for the fourth corpus's CALL-FAN and LADDER), or give COSTLIER? a margin so the assertion is 'costlier by more than X' rather than 'costlier at all'. Owner: the codegen-compare harness.
