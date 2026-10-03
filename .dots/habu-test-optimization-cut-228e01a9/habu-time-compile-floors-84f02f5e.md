---
title: Time compile floors in thread CPU time
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T11:07:52.693965+02:00\""
closed-at: "2026-10-01T17:39:37.088054+02:00"
close-reason: Fixed by owqvomxx 40a79709 (review 126 REVISE, fold review 170 ACCEPT)
---

Problem: test/compile-floor-gate.f compares microsecond budgets with times tools/compile-floor.f (and the tier-0 probes) take with mono-ns, a wall clock (compile-floor.f:57, :177). Host load stretches them: at load 50-80 the gate row fails kind=exit on the unchanged base tree (r4-wblabel commit 2 report), and the registry comment (test/gate-stdlib-cases.f:2291-2292) accepts that. A gate row must not go red for its neighbours. Acceptance: every timed window in the floor tools reads the calling thread's CPU time (clock_gettime CLOCK_THREAD_CPUTIME_ID, or the host's equivalent), named once; budgets re-derived from ten runs and stated with their range and load; the row passes at load 50+ and still fails for a doubled compile cost (show with a forced 2x body in a scratch copy); the registry comment and the tool headers say what is measured. Files: tools/compile-floor.f, test/compiler/compile-floor.f, test/compile-floor-gate.f, any lib word the clock needs, test/gate-stdlib-cases.f. Verify: test/compile-floor-gate.f standalone and under a loaded pool.
