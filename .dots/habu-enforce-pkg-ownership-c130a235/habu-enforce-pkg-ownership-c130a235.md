---
title: Enforce package ownership in Forth diffs
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T05:04:12.099422+02:00\""
blocks:
  - habu-lint-diff-share-486c2d86
  - habu-consume-registry-events-efe7fe5e
---

Full context: AGENTS.md:60-71 and docs/forth.md:109-123 require every new library, tool, test-support, and subsystem module to use a real package, but enforcement is manual and the referenced habu-enforce-pkg-first-c28d1dec dot does not exist. Cause: tools/namespace-lint-core.f enforces package ownership only for Maki source, while tools/typed-local-diff-lint.f checks changed Forth syntax but not package scope. Fix: add a checked Habu exact-diff lint that tracks package scope across added lines and rejects new or changed module definitions outside a real package, with narrow documented core/prelude path exemptions; reject redundant owner-package/file-stem prefixes on newly added public and private tails when mechanically attributable; share tokenizer/diff parsing instead of duplicating it; wire the gate into the Forth commit workflow and gate manifests. Acceptance: negative fixtures cover global library/tool/test definitions, case variants, package close/reopen, redundant LRD-* inside package LRD, and forged comment/string/diff-header text; positive fixtures cover short tails, cross-package qualification, reopened multi-file packages, and documented core/prelude globals; the live dirty diff is clean; no host glue. Files: checked lint modules/tests/entrypoint, AGENTS.md, docs/forth.md only for command, and manifests. Verify: focused lint tests, lint-tools owning gate, typed-local diff lint, dot-dep-lint, and full native gates.

Dependency: habu-lint-diff-share-486c2d86 must land first; this lane consumes its checked DIFF package and must not copy or fork the parser.

Claim: agent=package_diff_lint workspace=.jj-ws/habu-package-diff-lint.
