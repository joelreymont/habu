---
title: Throw window-child deadlines in engine-writers
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T13:31:23.188673+03:00"
---

Problem: test/native-window-owner-child.f:222 RUN catches LOAD-WINDOW and prints the code, so an E-PROC-TIMEOUT from a forked case inside the window prints window: -2502 and test/engine-writers.f:34 RESULT fails an assertion on window: 0. docs/gate.md:428-441 says a deadline is no assertion result and must reach the pool as TIMEOUT-UNDER-LOAD. Seen in the census run and in the gate on 26a32b98 plus the stack (~/.cache/tmp/jerry-gate2/full.log:3336, :3357); the row passed alone on master and on the stack. Same defect class as 9d6b39a9. Acceptance: a deadline inside the window load reaches the parent as a deadline (the child reports E-PROC-TIMEOUT the way docs/gate.md:418 describes and the parent throws E-PROC-TIMEOUT), every other code still prints and asserts as now. Files: test/native-window-owner-child.f, test/engine-writers.f, and the other parents of that child if they compare the line. Verify: a copy whose window case forces a deadline makes the row end TIMEOUT-UNDER-LOAD; bin/hb --load test/engine-writers.f rc 0. Depends: none. Ownership: those files. Claim: unassigned.
