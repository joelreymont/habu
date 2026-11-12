---
title: Spark lacks the tools four suite tests need
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T08:58:57.283103+02:00"
closed-at: "2026-11-11T10:50:55.521016+02:00"
close-reason: "landed 3fa9f034b66a in master 5c2ebfc1: svg-pdf-e2e names the executable it misses and docs/bootstrap.md says where Debian keeps initdb; spark's suite 273 of 273 green with /usr/lib/postgresql/16/bin on PATH"
---

Problem: on spark the suite's zip, pg, fs-mutate and svg-pdf tests end rc 67 and file-objects case Q ends rc 1 (its build loads libzip: 'Q: the build exits 0' got 74) because the machine lacks what they call: libzip.so.5 is not on the loader's path (E-FFI-DLOPEN; a copy sits in ~/stuart-runs/lib), initdb is not on PATH (pg-cluster: required executable missing; pg-kill-test then fails 5 asserts), socat is missing (fs-mutate-test: required executable missing: socat), and svg-pdf-e2e throws -2506 before naming what it lacks. Logs: spark:~/carl-runs/xos4/o/red/. Acceptance: svg-pdf-e2e names the executable it misses, as the others do; spark has libzip, PostgreSQL's server tools and socat installed, or the suite's Linux run states which tests need them; the four tests pass on spark. Files: test/svg-pdf-e2e.f. Verify: the four tests on spark. Status 2026-11-11: Joel installed postgresql, socat, qpdf and libzip4t64 on spark. With /usr/lib/postgresql/16/bin on PATH (Ubuntu keeps initdb off PATH) pg-kill-test, fs-mutate-test, fs-rename-noreplace-test and svg-pdf-e2e are rc 0 there (spark:~/carl-runs/xos4/o/r2/). The zip tests and file-objects Q wait for dot habu-zip-asks-linux-88bcf59f. Left here: svg-pdf-e2e names the executable it misses, and test/AGENTS.md says a Linux suite run needs PostgreSQL's bin directory on PATH. Depends: none. Ownership: carl. Claim: unassigned.
