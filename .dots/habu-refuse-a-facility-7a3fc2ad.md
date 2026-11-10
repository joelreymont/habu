---
title: Refuse a facility held across a fork (timer lane 5)
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:34:19.467774+02:00"
---

Problem: a forked child that inherits a facility (a task lock) another thread held at the fork can hang on it forever. Lane 5 of the test-timer plan is coded in the parked change ttquwzyw (commit 4deac742 "Refuse a facility held across a fork"), proven on its rebuilt engine and not reviewed; master has no E-TASK-FORK-HELD. Its design: TASK:CHILD-RESET walks a chain of every facility in the child; the forking thread's facilities stay its own, owner 0 gets a fresh mutex, any other owner is marked refused and GET throws E-TASK-FORK-HELD; no generation counter (an earlier generation design missed forks landing between GET's lock and its owner store, 26 of 64 children hung); font.f's unreset spin lock becomes a facility; new case FORK-INTO-FACILITY: children forked into a GET/RELEASE loop answer granted or refused, never hang. Four spin locks keep their own resets. Acceptance: the change reviewed and landed on master; FORK-INTO-FACILITY passes. Files (the parked commit's): src/lib/task.f, src/lib/font.f, src/lib/fork-child.f, src/lib/pg.f, src/lib/errors.f, src/lib/test/fork-hold.f, src/lib/fork-child-test.f, src/lib/pg-fork-test.f; src/lib/errors.f, src/lib/test/fork-hold.f and both tests are gone from master, so the rebase must follow where their code went and keep both test cases. Verify: rebase 4deac742 onto master; on an engine rebuilt from it, FORK-INTO-FACILITY and the pg fork case pass. Depends: none. Ownership: unassigned. Claim: unassigned.
