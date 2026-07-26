---
title: Lift family reflection helpers into shared test support
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T12:42:17.662473+02:00"
---

Three migration lanes (C13, D1, and the C7/C1 registry-pin blocks) hand-rolled the same family-lookup-plus-field-slot reflection helpers because the shared file was write-locked by concurrent lanes. The shapes are now stable across two family kinds and four suites. Behavior: lift one canonical helper set into test/checker-assert.f (family-by-tail-plus-ctor-package lookup with uniqueness assertion and -1 sentinel refusal; field name-to-slot reader; case name/order reader; kind reader), convert the existing hand-rolled copies in maki/evidence/policy-test.f, maki/evidence/schema-test.f, maki/competitive-report-test.f, maki/db/diff-runner-test.f, maki/db/action-test.f to the shared set with byte-identical assertion outcomes, and every future migration lane uses it. Acceptance: all five converted suites green with unchanged assertion counts; the helper set has its own focused test with a hostile fixture (ambiguous tail must fail the uniqueness assertion, wrong ctor package must not resolve); maki/test.f green. Owner: the checker-assert test-support package. Dependencies: dispatch only when no migration lane is in flight (whole-file conversions across five suites).
