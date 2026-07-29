---
title: Neutralize checker package scope at scope start
status: active
priority: 1
issue-type: task
created-at: "2026-07-25T14:40:51.881718+02:00"
---

Why this is needed: the checker word that opens a rollback scope does not put the replayed code in neutral package state, so any tool that replays standalone source inside a scope still runs with its caller's package active. In src/core/checker.f, CHECKER-SCOPE-START (line 9685) is RBF-PUSH (line 9633): it copies CHECKER-PACKAGE-MODE, CHECKER-PACKAGE-U and CHECKER-PACKAGE-NAME into the new frame and leaves the live values untouched. A package-owned caller therefore makes a plain top-level EXPORT in the replayed source look like an in-package re-export, and the run fails with exit 78.

What has already happened: commit 79c50e5a9dbf repaired exactly one call site. CA-CHECK-FULL-SCOPE in tools/check-all-errors-core.f (line 397) now calls CHECKER-END-PACKAGE immediately after CHECKER-SCOPE-START. That call-site rule is not written down anywhere the checker can enforce, and three more sites in tools/check-core.f have the identical shape with no neutralization: CHK-RUN-PREVERIFY (line 1254, scope opened at 1257), CHK-RUN-NOMINAL-LINTS (line 1293, scope opened at 1294) and CHK-RUN-SCOPED (line 1319, scope opened at 1320). All three replay or verify standalone source. They are latent only because their current callers are not package-owned; the same fault appears the moment a packaged caller drives them.

Owned result: the checker owns neutrality, not the call sites. In src/core/checker.f add one scope opener that pushes the rollback frame and then leaves the live checker package state at neutral top level, published as a primitive next to CHECKER-SCOPE-START (suggested name CHECKER-SCOPE-START-NEUTRAL, PRIM: block near line 4906). Leave CHECKER-SCOPE-START unchanged for scopes that must inherit the caller's package; do not change the default behaviour of RBF-PUSH, because CHECK-CANDIDATE-START and the other scope users depend on it. CHECKER-SCOPE-DONE must keep restoring the caller's exact package mode, length and name bytes on both the clean and the throwing path, unchanged.

Callers to migrate: the four standalone-replay sites named above use the new opener. Remove the now-redundant CHECKER-END-PACKAGE line from CA-CHECK-FULL-SCOPE so exactly one authority decides neutrality. The new primitive also needs a trusted-effect row: tools/check-core.f declares its checker axioms at lines 20-25 with the `s" NAME" s" effect" TRUST` form, so add the matching row there beside CHECKER-SCOPE-START, and add the corresponding TRUSTED.md row with its class, tests and owning dot.

Forbidden: repeating CHECKER-END-PACKAGE at each call site as the permanent answer, a wrapper in tools/ that hides the rule, weakening EXPORT, special-casing a caller, or any behaviour change to CHECKER-SCOPE-START's existing users.

Acceptance and smallest owning check: before the change, one negative regression per site must fail. For each of the three tools/check-core.f sites, drive that site's real production entry point from a package-owned caller with source that performs a top-level EXPORT, and record the pre-change exit 78. After the change all three pass, the caller's package mode, length and name bytes are proved restored after both a clean and a throwing replay, and the existing packaged all-errors suite (bin/hb --load tools/check-all-errors-test.f, all 23 cases including the top-level EXPORT case) stays green. A mutation that removes the neutralization from the new opener must red every one of those regressions.

Verify: tools/check-test.f, tools/check-all-errors-test.f, the checker suite covering scope push and pop, trust-lint, typed-local-diff-lint and package-diff-lint on the exact diff, host-lint and filemap-lint.

Files: src/core/checker.f, tools/check-core.f, tools/check-all-errors-core.f, TRUSTED.md, and the owning test files for the three new regressions.

Claim: agent=pkg-neutral workspace=.jj-ws/habu-neutralize-checker-pkg-b9a250c8

Update 2026-07-28 (orchestrator, suite-red mapping): after the CHECKER-AUTH-PACKAGE fail-closed plumbing landed, the observed failure code at these sites is now an uncaught/asserted 7136 E-PKG-CONTEXT (src/core/checker.f:527), not exit 78. Confirmed live on the proofs branch in four suite phases sharing this root: the engine repair slice (tools/check-repair-hints-test.f case repair-batch expects 70, gets 7136), stdlib/tool-doc (tools/repair-schema-doc-test.f + tools/examples-test.f, assert 90 then 92-118 cascade), stdlib/tool-repair (tools/check-all-errors-test.f case package-caller-export, asserts 65/67), and test/xt-cell-test.f (fork-worker throw 7136). Same invariant as written above: the checker owns package-neutral replay scope. Re-derive the pre-change reds against 7136.

CORRECTIONS from the implementing lane (2026-07-29) — this record has now been
wrong twice, so treat its analysis with suspicion:
1. The three tools/check-core.f sites are NOT independently latent. They NEST,
   and the neutral declaration propagates inward, so neutrality at any
   enclosing scope covers the inner ones. Only CHK-RUN-NOMINAL-LINTS (red 70,
   a top-level NEWTYPE filed under the caller's package) and CHK-RUN-PREVERIFY
   (red 78, a top-level EXPORT read as an in-package re-export) have isolating
   regressions. CHK-RUN-SCOPED has none that can exist today; the lane refused
   to claim one.
2. test/xt-cell-test.f does NOT share this root — only the SYMPTOM (7136).
   Its actual cause was an out-of-bounds write: VPKG-SAVE/VPKG-RESTORE kept
   the loop index on the data stack and read it back with `over` AFTER the
   destination address was pushed, so `over` reached the FETCHED BYTE and every
   byte was written at an offset equal to its own character code. Minimal
   reproducer: any SECOND VERIFY:SOURCE-BUF-IN-SCOPE call from inside an open
   package throws 7136; the first looks healthy only because the mirror
   already holds the right name. Fixed with a named VPKG-COPY over an explicit
   cursor and pinned by test/checker-verify-pkg-scope.f.
3. Line numbers throughout this record are stale: the PRIM: block is near 5019
   not 4906, RBF-PUSH near 9912 not 9633, and the three call sites near
   1327/1370/1399.

