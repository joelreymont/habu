---
title: Report a child deadline in test assertions as a timeout
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T07:18:47.470517+02:00\""
closed-at: "2026-10-01T13:07:37.383317+02:00"
close-reason: Fixed by yvouuyls c1882430 (reviews ACCEPT)
---

Problem: test code that reads a child's outcome through PROC-OUTCOME>RC into an assertion (e.g. test/whitebox-engine-suite.f:75, test/native-build-entry.f:36, test/prim-owner-scope.f:115, test/exit-hook-test.f:93, test/aot-chain-row-checks.f:83, test/fixture-cache-test.f:97), the STORE!/MATCH recorders (test/tier.f, test/seal.f, test/hb-cli-contracts-test.f, test/snapshot-writer.f and others) and SUBJECT:RUN forks (test/code-window.f, test/compiler/*) see an expired deadline as rc 137 (128 + SIGKILL), so the row fails as an ordinary assertion instead of reaching the pool as E-PROC-TIMEOUT and TIMEOUT-UNDER-LOAD (found by the r4-wblabel census; full table in ~/.cache/tmp/kestrel-r4-wblabel/HANDOFF.md). Acceptance: one rule at the shared layer (lib/process.f outcome readers, lib/test/subject.f) so a deadline expiry in any gate row's child throws E-PROC-TIMEOUT through the row, while a test that deliberately kills a child and asserts the signal still sees it; census table with a verdict per site; a forced 1 ms deadline through test/gate-pool.f gives TIMEOUT-UNDER-LOAD for a SUBJECT:RUN row and an assertion row, kind=exit for a real failure. Files: lib/process.f, lib/test/subject.f, the sites the census names. Verify: forced runs, test/gate-pool-test.f, lib/process tests, the rows touched. Depends: habu-label-the-whitebox-226c638b. Ownership: child deadline labels in test assertions.
