---
title: Cut the gate wall clock to minutes
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:24:03.755131+03:00"
---

Problem: bin/hb --load test/run.f takes 14.5 min for about 406 suites on the guard-free engine (869 s, gate-k5). Thirteen suites rebuild a cold engine each (habu-build-the-cold-cd0090f3) and the fixture-heavy suites fork hundreds of children that each recompile the libraries they require (lib/task-test.f allows 60 s per child for compiling lib/task.f alone). Acceptance: a per-suite timing table from test/run.f recorded in the dot, the top offenders fixed (one shared preloaded child engine per suite, or the libraries in the prefix), gate under 5 min on this machine. Files: test/run.f, test/gate-common-lib.f, test/gate-stdlib-cases.f. Verify: time bin/hb --load test/run.f. Depends: habu-build-the-cold-cd0090f3. Ownership: hazel line. Claim: unassigned.
