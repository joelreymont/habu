---
title: Green the build fixture suites
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T10:57:56.509951+03:00"
---

Problem: on the complete run of 2026-09-12 (engine 04701ef9, LESSONS.md red set) the build-fixture suites are red, exit code and first failure: build-fixpoint-fixtures (1: build-fixpoint-test: subtest threw, F12); stdlib-standalone-load (1: F46); diagnose-hb (1: F3); hb-open-failure (1: F1). Acceptance: each suite green on the root engine, or its case retired with the reason in the commit and the LESSONS.md red set updated in the same commit. Files: tools/build-fixpoint-test.f, test/stdlib-standalone-load.f, tools/diagnose-hb-test.f, test/hb-open-failure.f (as named in test/gate-stdlib-cases.f). Verify: each suite standalone on the root engine, then test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
