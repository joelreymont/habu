---
title: Schedule unscheduled compiler and seal tests
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T22:37:26.988790+02:00"
---

Full context: twelve test files landed with the compiler IR substrate, the Rocq identity binding, and the raw-storage seal but are listed in NO suite in test/gate-stdlib-cases.f and in no other gate file, so they never run in the test suite: test/compiler/ir-{arena,attr,context,source,symbol,type}.f, test/compiler/ir-id-{obligations,proof,replay,source}.f, and test/raw-storage-load-seal-test.f. Verified by search: zero scheduling references for each; test/compiler/ir-arena.f is referenced nowhere in the repository outside itself. They pass when invoked by hand (that is how each lane and the orchestrator verified them), so this is not a red — it is worse: the work is unprotected, and a future regression in the compiler substrate or the raw-storage seal would not be caught. tools/suite-coverage-lint misses it because it only checks that suite members exist, never that every test file is scheduled. Work: register all twelve in the appropriate suites (test/compiler/ir-id.f is already scheduled as SUITE compiler-ir-id at test/gate-stdlib-cases.f:219, follow that pattern), confirm each actually executes in a full run, and extend suite-coverage-lint so an unscheduled test file under test/ is a FINDING rather than silence. Acceptance: a full bin/hb --load test/run.f shows each of the twelve executing; deleting a scheduling entry reds suite-coverage-lint; the lint's new direction is falsified by adding a throwaway unscheduled test file and confirming it is reported.
