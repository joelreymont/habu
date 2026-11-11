---
title: A refused evaluate at top level writes no JSON under --all-errors --json-errors
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:27:27.644922+02:00"
---

Problem: a file whose top level runs s" NOSUCHWORD" evaluate, checked with tools/check.f -- --all-errors --json-errors, prints only the prose line E-UNDEFINED: NOSUCHWORD at <file> and no JSON packet, so diag-contract fails with no JSON diagnostics; diag-code.f states a valid line even when the input is rejected. Reproducer: ~/.cache/tmp/rv-arena-refix/probe/u1.f (found by the arena F2/F3 re-review, .out/review-arena-refix-report.md). Acceptance: the run writes one JSON record for the refusal and diag-contract accepts it; a case in test/diag-position-test.f. Files: the checker's evaluate refusal path, test/diag-position-test.f. Verify: the probe and the test. Depends: none. Ownership: unassigned. Claim: unassigned. Related: habu-all-errors-reports-09344102.
