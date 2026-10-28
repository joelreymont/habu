---
title: Report a duplicate name CHECK-UNJUDGED scans
status: open
priority: 3
issue-type: task
created-at: "2026-10-28T16:57:47.891481+02:00"
---

Problem: CHECKER-OWNER:CHECK-UNJUDGED on a body whose name an earlier scan already recorded reaches CHECKER-DUP-DEFINITION (src/core/checker.f), which throws $4E (78) with no message. Positive throw codes are exit statuses the loader assumes were reported, so a --load tool that scans a name twice exits 78 silently (found by the T0 harness, jerry-t0-l1 .out/t0-dis.f placed pass; minimal: two CHECK-UNJUDGED calls on s" L06 ( n -- n ) abs 1+ "). An ordinary duplicate `:` reports 'duplicate definition: A at file:line'. Acceptance: the scan path names the duplicate the way the definer path does, or throws a negative error the top level prints; a test row through CHECK-UNJUDGED shows the message. Files: src/core/checker.f, the checker-owner scan entry. Claim: unassigned.
