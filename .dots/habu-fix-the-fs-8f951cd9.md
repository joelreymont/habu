---
title: Fix the FS-TEST-PARALLEL kill race in the full suite
status: open
priority: 2
issue-type: task
created-at: "2026-09-21T20:24:38.520847+03:00"
---

Problem: tail-pure-fixtures went red once in about six full-suite runs on the same tree with 'hb: uncaught throw code -3604' (E-TASK-STATE), exit 67, right after layout-box-test, i.e. inside lib/fs-test.f FS-TEST-PARALLEL: it spins until DONE reaches 2 and then TASK:KILLs the two workers, which store DONE before their bodies end, so under pool load a worker can have ended on its own before the KILL reaches it. Not reproduced by replaying the suite's eleven files alone (four runs, rc 0); the tree's baked code was byte-identical to the host's, so the compiler is not the variable. Evidence: /tmp/hazel-x64-spill/full-suite.log line 604 (pool-1668492-248). Acceptance: decide whether TASK:KILL of a task that already ended is E-TASK-STATE by contract (then the test joins or checks TASK:DONE? before it kills) or a race the runtime must tolerate (then KILL answers idempotently and lib/task-test.f pins it); the fix is in the responsible layer with a regression, and tail-pure-fixtures is green under pool load. Files: lib/fs-test.f, lib/task.f, lib/task-test.f. Verify: lib/task-test.f, lib/fs-test.f, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
