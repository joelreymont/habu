---
title: Label the whitebox capture deadline a timeout
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T06:57:00.135522+02:00\""
closed-at: "2026-10-01T13:07:37.238371+02:00"
close-reason: Fixed by yutvvxum 54008a1d (reviews ACCEPT)
---

Problem: test/whitebox-engine.f kills its capture child at its own deadline, 60 s shorter than the pool's, and the kill reads as exit 137 (128 + SIGKILL) instead of a timeout, so the whitebox-engine-build image row reports an ordinary failure, the class habu-report-a-grandchild-079b684a fixed for build tools (found by the r4-deadline lane on 4863ad9c). Acceptance: the capture deadline's expiry throws E-PROC-TIMEOUT through the row, so the pool labels it TIMEOUT-UNDER-LOAD with the step named; a forced 1 ms deadline in a scratch copy goes from kind=exit to TIMEOUT-UNDER-LOAD through test/gate-pool.f; a real non-timeout failure stays kind=exit. Files: test/whitebox-engine.f, lib/process.f only if the wait must say why it ended. Verify: forced runs through test/gate-pool.f, test/gate-pool-test.f, the whitebox-engine-build row. Depends: habu-report-a-grandchild-079b684a. Ownership: whitebox capture deadline label.
