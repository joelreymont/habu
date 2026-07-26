---
title: Recalibrate cold gate budget
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:01:14.718775+02:00"
---

Problem: the cold-cache budget in test/run-lib.f lines 84-87 (SPARK-COLD-MS and MACOS-COLD-MS both 25000) is wrong for this box, so every cold run of test/run.f reds spuriously while correctness stays green. Four measurements: 30840, 31612 on the unmodified parent, 33571, and roughly 33000 recorded in LESSONS - all against the 25000 cold budget, while warm runs pass consistently against 35000. Required result: recalibrate the cold budget base for the affected profiles or derive the cold budget from the warm one with a measured cold factor, keeping the provenance lines (base, pct, cold flag) intact so a halved budget stays visible. Acceptance: a fresh-HB_TMP cold run of bin/hb --load test/run.f passes the performance verdict on this box on an unmodified tree; the budget calibration test pins the new values; warm budgets unchanged. Files: test/run-lib.f, test/run-budget-cal-test.f. Verify: cold and warm runs of test/run.f plus the budget calibration test. Depends: none. Ownership: cold budget constants and their derivation only. Claim: unassigned.

Amended 2026-07-26 (MODELPROV lane evidence): the maki core slice grew from 14.8 s at calibration time to 21.9-22.6 s today, measured at 22590 ms on the unmodified base 4383174718d4, so the 25000 stop-line cannot fit for ANY tree (forced cold: base 38066, candidate 39945, both hard-fail, correctness green). Record the derivation (measured value, margin, date, slice inventory) beside the constant, add the rule that any change adding a suite to the measured slice re-derives it in the same commit, and coordinate with habu-refuse-perf-measurement-150e80b6 so the recalibration measurement happens in a provably quiet window.
