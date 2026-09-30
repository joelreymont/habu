---
title: "Exit the booted harness's failed checks group-wide"
status: open
priority: 3
issue-type: task
created-at: "2026-09-30T14:48:39.770045+03:00"
---

Problem: the booted harness fails a check through `NR-EXIT` (`test/x86-64-peer-harness.f` failure path), which ends only the calling thread. K11c (habu-add-the-x86-efc81b26) measured on kernel 7.2.5 that a main-thread `exit(21)` followed by a task thread's `exit(0)` made the process exit 0 (strace), so a failed check in a threaded image can pass; K11c worked around it by checking only after the join.
Acceptance: a failed check ends the whole process with `exit_group` (231) and its status; the status contract (21-30) is unchanged; K11c's pthread image may then check before the join.
Files: `test/x86-64-peer-harness.f` (the failure exit), `test/x86-64-kernel-task.f` (drop the after-join restriction note if it no longer applies), `docs/x86-64.md` where the harness exit is described.
Verify: ThinkPad: an image whose task thread fails a check while the main thread later exits 0 exits with the check's status (it exits 0 before the change); every existing suite and image keeps its status; the x64-routines manifest loop bad=0.
Route: direct (test-only).
Ownership: krait (Intel lane).
Claim: unassigned.
