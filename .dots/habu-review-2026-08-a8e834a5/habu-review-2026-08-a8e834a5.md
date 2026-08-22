---
title: "Review 2026-08-22: tests and gate runner"
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:47:07.005612+02:00"
---

Problem: the test-integrity audit (master 5ee6beb2, read-only) found the gate unable to be green on Linux by construction, a schedule lint that certifies 27 never-run files as covered, uncalibrated budgets on this host, skip-logic sites, substring tests, and duplicated harness layers. Acceptance: every child closed or refuted. Files: test/, lib/test/, tools/lint/schedule-lint.f. Verify: per child. Depends: none. Ownership: gate runner. Claim: unassigned.
