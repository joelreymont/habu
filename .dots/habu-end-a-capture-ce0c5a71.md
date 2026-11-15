---
title: "End a capture on the child's exit (timer lane 1)"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:34:19.434107+02:00"
---

Problem: a capture ends on a deadline instead of on its child's exit, and a capture child can outlive the gate pool that started it. Lane 1 of the test-timer plan (habu-remove-the-test-6fc078db) is coded in the parked change vrppzrrz (commit 789de1df "End a capture on the child's exit"), which conflicts with master; master has no PROC-EXIT-FD. Its re-review said NOT YET: fix E puts a forked capture child in its own process group, so the pool parent's death no longer reaches it, because the child inherits the write end of the worker-alive pipe. The re-review's other findings: the HOLDER-KILLED labels, one-line SET-PGID on both sides with LEAD removed, the PAUSED comment and the END header text. Also, capture children of exec'd rows lead their own process groups (src/lib/process.f:136-139) and no reaper watches them, so a SIGKILL of the pool leaves them running (test/gate-pool.f:25-26). Acceptance: a capture ends on its child's exit with a tree kill only when a pipe is still held after the exit, and a SIGKILL of the pool ends every capture child, forked or exec'd. Files: src/lib/process.f, src/lib/process-fork.f, test/gate-pool.f, test/gate-common-lib.f, test/gate-common-test.f, test/capture-tree-test.f, test/gate-pool-orphan-test.f. Verify: the exit-ends-capture case fails on master (after 30 s) and passes on the lane; hb --load for src/lib/process-test.f, test/gate-common-test.f, test/capture-tree-test.f and test/gate-pool-orphan-test.f each rc 0. Depends: none. Ownership: unassigned. Claim: unassigned.
