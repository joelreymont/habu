---
title: process-test standalone -2502 flake under concurrent lanes
status: open
priority: 3
issue-type: task
created-at: "2026-07-11T09:21:14.533987+02:00"
---

lib/process-test.f intermittently dies 'hb: uncaught throw code -2502' (E-PROC-TIMEOUT) when run standalone while other jj-workspace lanes run gates (5-7 concurrent bin/hb). Observed across multiple trees (before AND after the outcome-sum + capture-state slices; not a regression: 3x serial green, new signal test 9/9 green in isolation, run.f gate green because run-lib exports calibrated HB_LOAD_PCT to pool children). Even HB_LOAD_PCT=300 standalone runs still flaked once in a batch, so either some capture budget does not scale (audit PT-*-TIMEOUT-MS coverage: POLL-IN-OR-TIMEOUT sites, PT-CMD paths) or the 3x clamp is insufficient under 7-lane contention. Evidence tool needed first: the uncaught throw names no test - add a T-LABEL progress marker per PT test (seal.f style) or a WHY-THREW include-under-catch harness so the flaking test is identified before any budget change. Suspects: TEST-RUN-CAPTURE-* with PT-HB budgets spawning bin/hb children under contention.

A/B evidence (2026-07-11): pristine master (4fbd3081) and the capture-state
tree both 4/4 green standalone (HB_LOAD_PCT=300) in the same time window,
minutes after the capture-state tree failed 2/2 during another lane's gate
burst. The flake tracks ambient load (time), not tree content. run.f is
immune (calibrated per-pool budgets). Localization tool (per above) still
needed before any budget change.
