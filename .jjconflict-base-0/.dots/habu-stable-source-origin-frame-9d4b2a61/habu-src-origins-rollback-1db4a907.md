---
title: "Source origins: rollback stale records"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:22:52.087566+02:00"
blocks:
  - habu-src-origins-intern-1dd60d2e
  - habu-src-frames-unwind-446e760c
---

Problem: candidate rejection or parser failure can leave provisional origin rows visible or let retired handles alias later records. Acceptance: enroll origin allocation in existing candidate/evaluator rollback; retire provisional rows in reverse order, preserve already-published origins, use generation checks for every reference, and ensure frame unwind plus origin rollback restores both owners without leaks or stale aliases. Cleanup failure is supplemental and never replaces the primary failure. Files: source-origin rollback adapter and focused failure tests. Verify: rejection at every capture boundary, nested parser throw, failure then success, stale generation, no leaked rows, and published-record stability. Depends: Source origins: intern declaration spans and Source frames: unwind failures. Ownership: origin rollback, generation retirement, and failure cleanup only. Claim: unassigned.
