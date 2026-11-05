---
title: "Run chosen tests by name in the suite's pool"
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T12:02:25.376968+02:00"
---

Problem: bin/hb --load test/run.f refuses any argument (test/gate-stdlib-lib.f SUITE-CHECK-ARGS), so a lane runs its tests one at a time (21 tests took 8.3 min). Joel 2026-11-05 approved `bin/hb --load test/run.f -- <test names>` running just those tests in the suite's parallel pool (DECISIONS.md, chosen tests). Acceptance: TEST:ONLY in src/lib/test/suite.f marks every test of a name; once any is marked, the run, the load-file visit and the whitebox question skip unmarked tests; ITEMS-REGISTERED stays the total; an unknown name is refused with exit 64 naming it. Tests: src/lib/test/suite-test.f cases, test/run-cli-test.f end to end. Files: src/lib/test/suite.f, src/lib/test/suite-test.f, test/gate-stdlib-lib.f, test/run-cli-test.f, docs/gate.md. Verify: bin/hb --load src/lib/test/suite-test.f; bin/hb --load test/run-cli-test.f. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-only
