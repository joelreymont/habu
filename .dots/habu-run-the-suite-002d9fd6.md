---
title: Run the suite on the engine that runs it
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T15:11:41.360423+02:00"
---

Problem: test/run.f's pool starts every suite file with the relative bin/hb (test/gate-stdlib-lib.f:34-35 PRODUCT-SPAWN$, :59, :63), while HABU_UNDER_TEST names the engine running run.f (:37-38, :44); so `<engine> --load test/run.f` with another engine compiles and runs every suite file on bin/hb and only the suites' children on that engine, and its PASS does not cover the engine that ran it. Acceptance: one suite run tests one engine, the one that runs test/run.f: every suite file and every child it spawns run on it. Depends: none. Claim: unassigned.
