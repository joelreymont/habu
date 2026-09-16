---
title: Cut the gate wall clock to minutes
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:24:03.755131+03:00"
---

Problem: bin/hb --load test/run.f takes 14.5 min for about 406 suites on the guard-free engine (869 s, gate-k5). Thirteen suites rebuild a cold engine each (habu-build-the-cold-cd0090f3) and the fixture-heavy suites fork hundreds of children that each recompile the libraries they require (lib/task-test.f allows 60 s per child for compiling lib/task.f alone). Acceptance: a per-suite timing table from test/run.f recorded in the dot, the top offenders fixed (one shared preloaded child engine per suite, or the libraries in the prefix), gate under 5 min on this machine. Files: test/run.f, test/gate-common-lib.f, test/gate-stdlib-cases.f. Verify: time bin/hb --load test/run.f. Depends: habu-build-the-cold-cd0090f3. Ownership: hazel line. Claim: unassigned.


Parked 2026-09-16 (release first): commit 1 (shared content-keyed cold engine, 44323336) landed on the line; commits 2-4 in .jj-ws/hazel-gate-clock (split native-window-owner into nine suites, aot-wide-format into four, longest suites first: 0a0ba873, cc6a8495, 5f58fb7c) are unverified scheduling changes awaiting a quiet-machine A/B gate; the per-suite timing table is in LESSONS.md. Next CPU win: the optimizing writer is recompiled for every image write (about 260 s per gate).
