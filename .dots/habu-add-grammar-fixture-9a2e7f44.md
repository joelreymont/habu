---
title: Add grammar-fixture category to package lint
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T17:21:34.511553+02:00"
---

Prerequisite for the NEWTYPE rename merge, and a principled lint category rather than an exception: nine test files' global declarations are LOAD-BEARING FIXTURES - test/type-decl-suite.f's own header states its declarations exist to prove user declarations register families through the baked grammar words WITHOUT opening any reserved package; packaging them would destroy the proof. The same class covers the flagged declarations in test/internal-word-gate.f, extent-substrate-probe.f, extent-product-test.f, typed-storage-test.f, cast-suite.f, cast-negative-suite.f, layout-buffer.f, layout-defer.f, engine-suite.f. Behavior: tools/package-diff-lint-core.f gains a second principled list beside GLOBAL-IMPLEMENTATION? - declaration-grammar fixture files - each entry carrying a one-line justification citing the suite's proof obligation (the src/core/util.f boot-necessity precedent: a named reasoned list pinned by the lint's own unit test, not an ad hoc waiver); entries are exact paths; a global added to any OTHER test file still rejects; the lint's unit test pins the list and proves a non-listed test file still flags. lib/process-pty-handle.f is explicitly NOT on this list (it is real debt with its own packaging dot). Acceptance: the lint unit test green with the new pins including the hostile non-listed-file fixture; the NEWTYPE rename diff passes package-diff-lint; full lint-tools slice green. Owner: the package-diff lint package. Claim: agent=newtype workspace=.jj-ws/habu-newtype-rename (commit 2 of the restacked rename lane).
