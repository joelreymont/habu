---
title: Compare canonical paths in build-fixpoint-test
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T19:36:44.721428+03:00"
---

Problem: `tools/build-fixpoint-test.f` (`TEST-CLOSURE-KEY`, lines 800-801) compares paths built from `HB_TMP` with the closure's canonical paths. The test fails when `HB_TMP` is not canonical, for example when it contains `..` or a symlink. Astra found this in the i7c2 review, batch 13.
Acceptance: the comparison sees canonical paths on both sides, and build-fixpoint-test passes both with `HB_TMP=<dir>/../<base>` and with the usual value.
Files: `tools/build-fixpoint-test.f`.
Verify: build-fixpoint-test under both `HB_TMP` values.
Depends: none.
Ownership: krait.
Claim: unassigned.
