---
title: Schedule unscheduled compiler and seal tests
status: active
priority: 2
issue-type: task
created-at: "2026-07-28T22:37:26.988790+02:00"
---

Full context: twelve test files landed with the compiler IR substrate, the Rocq identity binding, and the raw-storage seal but are listed in NO suite in test/gate-stdlib-cases.f and in no other gate file, so they never run in the test suite: test/compiler/ir-{arena,attr,context,source,symbol,type}.f, test/compiler/ir-id-{obligations,proof,replay,source}.f, and test/raw-storage-load-seal-test.f. Verified by search: zero scheduling references for each; test/compiler/ir-arena.f is referenced nowhere in the repository outside itself. They pass when invoked by hand (that is how each lane and the orchestrator verified them), so this is not a red — it is worse: the work is unprotected, and a future regression in the compiler substrate or the raw-storage seal would not be caught. Work: register all twelve in the appropriate suites (test/compiler/ir-id.f is already scheduled as SUITE compiler-ir-id in test/gate-stdlib-cases.f, follow that pattern) and confirm each actually executes in a full run. Acceptance: a full bin/hb --load test/run.f shows each of the twelve executing. (Original acceptance also asked to extend suite-coverage-lint; that tool was retired with the governance cleanup mirror, so scheduling itself is the whole fix.)

Claim: agent=sched workspace=.jj-ws/habu-schedule-unscheduled-compiler-b36ff91b
