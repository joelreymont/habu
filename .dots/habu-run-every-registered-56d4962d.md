---
title: Run every registered suite in test/run.f
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T06:03:38.938630+03:00"
---

Problem: test/run.f stops at the first drain that sees a red, so on the root only 151 of the 304 registered suites run and the known ten reds are the prefix's; the engine suite (two pre-existing reds, habu-fix-the-two-284ac502), cast-suite, pre-trust-defer, snapshot-xt-cell-decl, p2-map-rewind and addrmap-inline never reach the pool in a normal run and were found red only by running them directly (tier lane, 2026-09-12). A gate that reports a prefix is not a gate. Acceptance: the pool runs every registered suite regardless of earlier reds, reports the complete red set once at the end with each suite's exit code, exits nonzero when any is red, and the known red set for the root is re-established from a complete run and recorded in LESSONS.md; the per-suite child budget stays load-aware or is calibrated (habu-runner-budgets-uncalibrated-cb11c328). Files: test/run.f, test/gate-stdlib-cases.f, LESSONS.md. Verify: bin/hb --load test/run.f runs 304 suites and prints the complete red set. Depends: none. Ownership: hazel (test harness) with rowan's sign-off on the recorded red set. Claim: unassigned
