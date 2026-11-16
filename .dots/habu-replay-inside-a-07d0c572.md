---
title: Replay inside a definition while a task runs
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T08:42:14.166997+01:00"
---

Problem: a CHECK:SCOPED or verifier replay that an immediate runs inside a pending definition is rejected E-VERIFY-WINDOW by --load and check.f when a task is live at its open (src/core/checker.f PENDING-REJECT, from CHECKER-OVERLAY OPEN), and a task started inside it is rejected E-TASK-STATE (src/lib/task.f ACTIVATE); src/habu/habu2.f REPLAY-OPEN and REPLAY-CLOSE reject both states. --verify-only, which runs no task, admits the file: the readers disagree. Cause: with a task live, replay-close keeps the overlay's code and records, which would split the parked definition's body, and the definition is named by its slot NDICT and its entry (PEND-CELL readers in src/habu/interpreter.f: ENTRY-PEND, DEF-SOURCE-CLOSE, GROUP-SOURCE-END, the EVAL-FRAME PEND save). Acceptance: such a replay with a task live, and a task started inside it, complete the definition as with none, under all three readers; test/load-reject-diag-test.f TEST-REPLAY-TASK's case a loads and case b's task starts. Depends: none. Claim: unassigned.
