---
title: Collapse check-cli suite bottleneck
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-30T23:24:33.441392+02:00\""
---

Problem: Mac hot test suite reports slowest-test=native stdlib check-cli slice at ~10.2s. This slice still spends time in process-boundary CLI checks and repeated helper setup. Fix: profile the check-cli test entries, split semantic checks into resident in-process tests, keep only public CLI contract sentinels as child processes, and batch source programs where exact candidate execution is required. Acceptance: check-cli slice under 5s on Mac hot profile, no loss of CLI stdout/stderr/rc coverage, stats show fewer inner-hb/helper-spawn events.

Progress: macos-arm64-12x2 hot proof on 2026-06-30 reduced check-cli to 5083ms
with inner-hb=11 and helper-spawn=54. Keep open: acceptance is under 5s and the
remaining margin is not proven.
