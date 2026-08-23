---
title: runner budgets uncalibrated on this host
status: active
priority: 1
issue-type: task
created-at: "2026-08-22T22:47:07.018740+02:00"
---

Problem: DETECT-PROFILE (test/run-lib.f:279-290) picks linux-arm64-4x2 here; CAL-REF-MS (:310-317) returns TR-CAL-REF-LINUX-MS = 0 (:74) so TR-CAL-PCT is pinned at 100 and CAL-SCALED/HB_CAL_PCT are no-ops; lib/test/budget.f:43-45 likewise off macOS; TR-PROFILE-APPLY (:350-353) sets 4 slots/2 nested on a 12-core box so TR-POOL-PRESSURE-PCT (:549-551) floors HB_LOAD_PCT at 300 and every T-BUDGET-MS hang detector runs 3x slower; every PERF-MS ratchet is a macOS/Spark nominal (gate-engine-lib.f:2428 NOMINAL-MS 16000 from a 10-slot macOS pool; tail-ratchet.f:28-38 'measured on macos-arm64'; match-factor-pin.f:317 6000; json-read ratio basis macOS 8P+4E). The full table is in the parent's audit. Acceptance: TR-CAL-REF for this host measured with test/cal-spin.f (the documented procedure, :8-10) and committed with a host profile for Apple-silicon Linux (see habu-host-profile-skill-f1fb1c41); an uncalibrated profile refuses ratchet phases instead of running them at 100%; each ratchet's nominal re-measured here or marked host-bound. Files: test/run-lib.f, lib/test/budget.f, test/cal-spin.f, the ratchet files. Verify: --timings run on this host with the new profile; ratchets within budget. Depends: an engine on this host. Ownership: gate runner. Claim: agent=host-timing workspace=.jj-ws/habu-runner-budgets-uncalibrated-cb11c328
