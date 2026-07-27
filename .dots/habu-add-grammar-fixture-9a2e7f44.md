---
title: Add grammar-fixture category to package lint
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T17:21:34.511553+02:00"
---

Why: nine suites must declare selected type and storage fixtures at top level to
test the real user grammar. Packaging those declarations would remove the
behavior under test. This is a narrow grammar category, not a file exemption.

Owner: `PACKAGE-DIFF` in `tools/package-diff-lint-core.f`.

Behavior: admit only a declaration whose exact path and opener match one row in
this table:

- `test/type-decl-suite.f`: `SUMTYPE`, `PRODUCT`, `ENUM`, `VALUE-RECORD`,
  `NEWTYPE`, `DEFLINEAR`, `LAYOUT-BUFFER`
- `test/extent-substrate-probe.f`: `NEWTYPE`
- `test/extent-product-test.f`: `NEWTYPE`
- `test/typed-storage-test.f`: `TYPED-VARIABLE`, `TYPED-BUFFER`, `SUMTYPE`,
  `NEWTYPE`, `PTR-VARIABLE`, `LAYOUT-BUFFER`, `DEFLINEAR`
- `test/cast-suite.f`: `CAST:`, `NEWTYPE`
- `test/cast-negative-suite.f`: `NEWTYPE`
- `test/layout-buffer.f`: `SUMTYPE`, `PRODUCT`, `ENUM`, `NEWTYPE`,
  `DEFLINEAR`, `LAYOUT-BUFFER`, `PTR-VARIABLE`
- `test/layout-defer.f`: `DEFER-LAYOUT-BUFFER`, `SUMTYPE`, `NEWTYPE`,
  `DEFLINEAR`
- `test/engine-suite.f`: `VALUE-RECORD`, `NEWTYPE`, `DEFTYPE`, `DEFLINEAR`,
  `LAYOUT-BUFFER`

The path comparison is exact and centralized. The opener comes from the
structural scan, not text search. A different opener in a listed file, any
global in an unlisted test file, and any package-scope change still reject.
`test/internal-word-gate.f` is excluded: it defines no declaration opener; its
declarations are strings executed by child processes, while its real globals
belong to the packaging dot `habu-pkg-internal-word-da4149d9`.

Acceptance: the package-lint unit suite pins every row and opener, generates a
hostile near-path case for all nine rows, proves comments and strings cannot
forge an opener, and proves an unlisted test file still rejects. The exact
NEWTYPE rename artifact passes both diff lints and the lint-tools gate.

Accepted implementation evidence: commit
`d10dc8fea911d22abe9786a6998b07a469b4d655` was independently reviewed and
accepted. The dot remains active until that commit lands and its owning gates
pass on the integration tree.

Claim: agent=gramfix workspace=.jj-ws/habu-add-grammar-fixture-9a2e7f44
