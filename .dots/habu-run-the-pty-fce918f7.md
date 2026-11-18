---
title: Run the PTY suites in the test pool
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T19:26:17.942220+01:00"
---

Problem: test/gate-stdlib-cases.f:1417-1440 runs repl-address-cell-rollback, repl-open-definition and dictionary-zero in the only sequential group, for a fixed 20 s child-reap budget; src/lib/pty-harness.f REAP (:218-222) has no deadline, so the group's reason is gone and the suite idles while it drains. Acceptance: the three suites run in the pool, the sequential group and whatever only it uses in src/lib/test/suite.f go, and the full suite passes. Depends: none. Claim: unassigned.
