---
title: A negative allot frees storage library heads still name
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T16:44:37.196307+01:00"
---

Problem: src/habu/habu1.f BALLOT (:3165) lowers DP for a negative count and revokes checker grids above it, but a library head or count below the new DP (task.f SLOT-LAST, TASK-CHAIN, error.f HEAD) keeps naming the freed storage, which the next definition reuses. Acceptance: after a negative allot no marked rollback cell names storage at or above the new DP, or the allot is refused, decided by what checked callers of a negative allot need; a test holds the case. Depends: none. Claim: unassigned.
