---
title: One declaration surface for test routing
status: open
priority: 3
issue-type: task
created-at: "2026-07-18T23:21:32.756185+02:00"
---

Four routing idioms decide how a test runs (TEST:SUITE in maki/test.f, GSI-FORK-INCLUDE in test/gate-stdlib-inline-lib.f, RUN-CASE manifests in test/candidate-validation.f, raw PROC-ARGV+ lists in test/run-lib.f) and a new test must pick correctly or silently run nowhere - which happened to both DDC tests found unrouted 2026-07-18. The execution mechanisms are genuinely different (in-process, forked child, spawned engine); the DECLARATION should not be. Design direction: one routing table (file -> mechanism + expectations) that run-lib and the manifest tests consume, plus a completeness lint: every test-shaped file (test/*-test.f, tools/*-test.f, maki/*-test.f) must appear in the table or in an explicit standalone list with a reason. The completeness lint alone would have caught the DDC gap and is worth landing first, independent of the bigger consolidation. PARKED until after the nanoGPT flagship push; the completeness lint may be pulled forward if unrouted tests recur.
