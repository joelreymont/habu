---
title: Name thrown codes from the error rows
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T15:05:56.815869+02:00"
---

Problem: src/lib/verify-diagnostics.f:66-77 THROW-CODE$ names two codes by hand and answers E-STATEMENT-THROW for every other, and MODEL keeps its own CODE$ table (src/lib/net/model.f CODE$ :2888, used once at REPORT+ :3809). Joel 2026-11-06 chose 'Real name (Recommended)' for tools/check.f's records of a thrown code. Acceptance: THROW-CODE$ takes name and message from ERROR:NAME$/TEXT$, keeps repair class and suggestion for its two codes, and answers E-STATEMENT-THROW only for a code with no row; check.f's thrown-code records carry the real name (the tests naming E-STATEMENT-THROW follow); MODEL:CODE$ goes and REPORT+ uses LOG:CODE+. Files: src/lib/verify-diagnostics.f, tools/check-all-errors-core.f:452, src/lib/net/model.f, their tests. Verify: tools/check-all-errors-test.f, verify-diagnostics' tests, src/lib/net/model-test.f. Depends: the ERROR package (converted codes give real names; the sweep widens them). Ownership: carl, worker lane. Claim: unassigned.
