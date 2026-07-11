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

Interleaved A/B (2026-07-11, post outcome-sweep): 4 alternating pairs
(sweep tip vs pristine derive-S2 master) all green in a quiet window,
minutes after 3x consecutive reds on the tip during a lane gate burst
(and, earlier, 3x reds vs 3x greens split the OTHER way). Reds track
gate-burst windows regardless of tree; run.f with calibrated budgets
never reds. Marker instrumentation (stderr PT-MARK trace) is the
localization tool for the next red window: it pinpoints the throwing
test; 6/6 instrumented runs stayed green this window.

Heisenbug note: on the Derive-S3-rebased sweep tip, a red window (2x
-2502) was followed IMMEDIATELY by 6/6 green instrumented runs (PT-MARK
stderr markers) in the same 5-lane window; the marker build never
reproduced. Either the fd2 writes perturb timing or bursts are shorter
than a 6-run span. Next step stays: catch a red WITH markers to name
the test. run.f passed on this exact tip in the same period.
