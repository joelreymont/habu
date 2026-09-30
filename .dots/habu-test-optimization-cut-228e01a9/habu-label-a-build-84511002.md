---
title: Label a build-fixpoint child deadline as load
status: active
priority: 2
issue-type: task
created-at: "\"2026-10-01T00:02:08.287376+02:00\""
---

Problem: tools/build-fixpoint-test-lib.f BFT-STEP (:308-313) catches every subtest throw and dies with 'subtest threw', so a child build that outlives BFT-TIMEOUT-MS (E-PROC-TIMEOUT from lib/process) reaches the pool as a plain exit failure; test/gate-pool.f GT-POOL-INNER-TIMEOUT? (:1137) relabels an inner deadline as TIMEOUT-UNDER-LOAD, with its saturation suffix, only when the child exits UNCAUGHT-RC with the engine's E-PROC-TIMEOUT report. Seen 2026-09-30 in the r4-pathcap lane's full suites at load about 75, where an engine build took 407 s: build-fixpoint-fixtures went red as an ordinary failure while build-fixpoint-snapshot was labelled TIMEOUT-UNDER-LOAD. Acceptance: an inner deadline that expires in any build-fixpoint row reaches the pool labelled TIMEOUT-UNDER-LOAD; every other subtest throw keeps its present report; other rows whose step wrappers catch and die the same way around a child deadline are found (method stated) and fixed the same way; the row stays red either way (no deadline is raised or lowered). Shown by forcing a short deadline in a scratch copy (not committed) and reading the pool's label. Files: tools/build-fixpoint-test-lib.f, the rows the audit names. Verify: the build-fixpoint rows, test/gate-pool-test.f. Depends: none. Ownership: the step wrappers' error path. Claim: agent=kestrel workspace=.jj-ws/r4-rows-eng.
