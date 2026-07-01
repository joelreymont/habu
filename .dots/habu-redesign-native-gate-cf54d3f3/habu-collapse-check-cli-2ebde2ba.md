---
title: Collapse check-cli suite bottleneck
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-30T23:24:33.441392+02:00\""
---

Problem: Mac hot test suite reports slowest-test=native stdlib check-cli slice at ~10.2s. This slice still spends time in process-boundary CLI checks and repeated helper setup. Fix: profile the check-cli test entries, split semantic checks into resident in-process tests, keep only public CLI contract sentinels as child processes, and batch source programs where exact candidate execution is required. Acceptance: check-cli slice under 5s on Mac hot profile, no loss of CLI stdout/stderr/rc coverage, stats show fewer inner-hb/helper-spawn events.

Progress: macos-arm64-12x2 hot proof on 2026-06-30 reduced check-cli to 5083ms
with inner-hb=11 and helper-spawn=54. Keep open: acceptance is under 5s and the
remaining margin is not proven.

2026-07-01 post-warm-launcher removal proof: macos-arm64-12x2 hot full suite
passed at 30016ms internal / 32.23s wall with check-cli 9273ms, inner-hb=6,
inner-hb-stdin=4, helper-spawn=38, boundary=10. Regression vs prior check-cli
means this dot stays open; next fix must profile check-test CLI sentinels and
keep only public argv/stdin/stderr/rc contracts as child processes.

2026-07-01 resident setup proof: after replacing duplicate per-worker stdlib
tool-base compiles with explicit parent suite setup, macos-arm64-12x2 hot full
suite passed at 26311ms internal / 28.66s wall. Check-cli is 11249ms in the
final run, so this dot stays open; the bottleneck is now inside
`tools/check-test.f` public CLI sentinels, not warm runner setup.

2026-07-01 close: replaced the remaining slow `tools/check.f` child smoke in
`tools/check-test.f` with the existing in-process `CHECK-ALL-ERRORS-FILE`
capture path for file-label JSON diagnostics. Focused check-cli passed at
781ms; full direct Mac hot suite passed at 24878ms internal / 27.22s wall with
check-cli 2397ms. Acceptance met: check-cli is below 5s with the same file-label,
JSON, duplicate-definition, source-list, require-facade, usage, and die checks.
