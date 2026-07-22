---
title: "Source frames: unwind failures"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T12:22:25.454984+02:00"
blocks:
  - habu-src-frames-restore-869b0245
---

Problem: parser throw and candidate rollback can strand child frames, leak owned bytes, or restore the wrong parent. Acceptance: enroll the nested frame stack in the existing evaluator and checker rollback boundaries; unwind live children in reverse order, restore the exact pre-entry parent and cursor, release each retired frame once, preserve the primary throw, and publish typed internal lifecycle notifications for later provenance consumers. Injected cleanup failure remains supplemental and cannot mask the primary failure. Files: source-frame rollback adapter and focused failure-lifecycle tests. Verify: parser throw at every depth, candidate reject, failure after each mutation boundary, nested cleanup failure, stale reuse, leak and double-release accounting. Depends: Source frames: restore nested parents. Ownership: exceptional unwind, rollback notifications, and failure cleanup only. Claim: unassigned.
