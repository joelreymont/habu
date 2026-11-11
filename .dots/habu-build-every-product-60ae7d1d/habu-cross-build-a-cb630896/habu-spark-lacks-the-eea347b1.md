---
title: Spark lacks the tools four suite tests need
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:58:57.283103+02:00"
---

Problem: on spark the suite's zip, pg, fs-mutate and svg-pdf tests end rc 67 because the machine lacks what they call: libzip.so.5 is not on the loader's path (E-FFI-DLOPEN; a copy sits in ~/stuart-runs/lib), initdb is not on PATH (pg-cluster: required executable missing; pg-kill-test then fails 5 asserts), socat is missing (fs-mutate-test: required executable missing: socat), and svg-pdf-e2e throws -2506 before naming what it lacks. Logs: spark:~/carl-runs/xos4/o/red/. Acceptance: svg-pdf-e2e names the executable it misses, as the others do; spark has libzip, PostgreSQL's server tools and socat installed, or the suite's Linux run states which tests need them; the four tests pass on spark. Files: test/svg-pdf-e2e.f. Verify: the four tests on spark. Depends: installing packages on spark is Joel's machine: ask before installing. Ownership: carl. Claim: unassigned.
