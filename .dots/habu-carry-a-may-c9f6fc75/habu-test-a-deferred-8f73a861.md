---
title: Test a deferred immediate body or delete its path
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T07:38:49.512918+01:00"
---

Problem: the kept-quotation stack made 'compile,' keep the xt it compiles (src/habu/prims.f PE-QANY), so test/diag-position-test.f's fixture no longer reaches an immediate whose body the check deferred (E-UNMODELED-IMMEDIATE through a deferred body, a parses: row left unjudged); no test covers that path now. Acceptance: a test reaches the path through a public program, or the path is shown unreachable and its code deleted. Depends: none. Claim: unassigned.
