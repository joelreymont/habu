---
title: Fix package-diff-lint on generated mirror diffs
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T15:49:37.112645+02:00"
---

Full context: two pre-existing package-diff-lint conditions found while landing the loop-closer fix, both reproduced independently of that change. (1) tools/package-diff-lint.f throws a bare -7400 on ANY diff touching bootstrap/cg/forth.fs - proven with a 9-line diff appending one comment to the otherwise-untouched base file. A lint that cannot process a legitimate diff of a tracked source is a lint defect; it must either lint the file properly or skip it by documented policy with a named reason, not throw. (2) The lint reports E-PACKAGE-OWNERSHIP for every changed definition in src/habu/habu2.f, which is entirely global by design - the J-* code-generator family (J-IF, J-THEN, J-DO, J-LOOPEND, ...) predates the package system. Putting one new helper (J-LVREQUIRE) in a namespace-of-one would fragment a cohesive family and be undone by any real packaging of the engine, so the right lint behaviour for engine-prefix globals needs a policy decision: either an explicit documented engine-prefix boundary in the lint, or a dot that packages the whole J-* family at once. Decide the policy, implement it, and add hostile fixtures per the test-integrity rule so the boundary cannot silently widen. Acceptance: a diff touching forth.fs lints cleanly or is skipped with a named documented reason; the habu2.f policy is explicit in the lint with a fixture proving the boundary is exact; existing findings elsewhere unchanged.
