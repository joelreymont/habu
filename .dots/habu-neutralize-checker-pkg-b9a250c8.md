---
title: Neutralize checker package scope at scope start
status: open
priority: 1
issue-type: task
created-at: "2026-07-25T14:40:51.881718+02:00"
---

Why this is needed: the checker word that opens a rollback scope does not put the replayed code in neutral package state, so any tool that replays standalone source inside a scope still runs with its caller's package active. In src/core/checker.f, CHECKER-SCOPE-START (line 9685) is RBF-PUSH (line 9633): it copies CHECKER-PACKAGE-MODE, CHECKER-PACKAGE-U and CHECKER-PACKAGE-NAME into the new frame and leaves the live values untouched. A package-owned caller therefore makes a plain top-level EXPORT in the replayed source look like an in-package re-export, and the run fails with exit 78.

What has already happened: commit 79c50e5a9dbf repaired exactly one call site. CA-CHECK-FULL-SCOPE in tools/check-all-errors-core.f (line 397) now calls CHECKER-END-PACKAGE immediately after CHECKER-SCOPE-START. That call-site rule is not written down anywhere the checker can enforce, and three more sites in tools/check-core.f have the identical shape with no neutralization: CHK-RUN-PREVERIFY (line 1254, scope opened at 1257), CHK-RUN-NOMINAL-LINTS (line 1293, scope opened at 1294) and CHK-RUN-SCOPED (line 1319, scope opened at 1320). All three replay or verify standalone source. They are latent only because their current callers are not package-owned; the same fault appears the moment a packaged caller drives them.

Owned result: the checker owns neutrality, not the call sites. In src/core/checker.f add one scope opener that pushes the rollback frame and then leaves the live checker package state at neutral top level, published as a primitive next to CHECKER-SCOPE-START (suggested name CHECKER-SCOPE-START-NEUTRAL, PRIM: block near line 4906). Leave CHECKER-SCOPE-START unchanged for scopes that must inherit the caller's package; do not change the default behaviour of RBF-PUSH, because CHECK-CANDIDATE-START and the other scope users depend on it. CHECKER-SCOPE-DONE must keep restoring the caller's exact package mode, length and name bytes on both the clean and the throwing path, unchanged.

Callers to migrate: the four standalone-replay sites named above use the new
opener. Remove the now-redundant `CHECKER-END-PACKAGE` line from
`CA-CHECK-FULL-SCOPE` so exactly one authority decides neutrality. The new
primitive also needs a checker effect row: `tools/check-core.f` declares the
relevant axioms with the `s" NAME" s" effect" TRUST` form, so add the matching
row beside `CHECKER-SCOPE-START`. Keep its rationale, retirement owner, and
focused production-path test source-local.

Forbidden: repeating CHECKER-END-PACKAGE at each call site as the permanent answer, a wrapper in tools/ that hides the rule, weakening EXPORT, special-casing a caller, or any behaviour change to CHECKER-SCOPE-START's existing users.

Acceptance and smallest owning check: before the change, one negative regression per site must fail. For each of the three tools/check-core.f sites, drive that site's real production entry point from a package-owned caller with source that performs a top-level EXPORT, and record the pre-change exit 78. After the change all three pass, the caller's package mode, length and name bytes are proved restored after both a clean and a throwing replay, and the existing packaged all-errors suite (bin/hb --load tools/check-all-errors-test.f, all 23 cases including the top-level EXPORT case) stays green. A mutation that removes the neutralization from the new opener must red every one of those regressions.

Verify: `tools/check-test.f`, `tools/check-all-errors-test.f`, the checker suite
covering scope push and pop, typed-local and package diff gates on the exact
diff.

Files: `src/core/checker.f`, `tools/check-core.f`,
`tools/check-all-errors-core.f`, and the owning test files for the three new
regressions. Claim: unassigned.
