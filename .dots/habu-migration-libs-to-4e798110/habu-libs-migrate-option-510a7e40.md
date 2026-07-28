---
title: Migrate OPTION to unified ENUM
status: active
priority: 1
issue-type: task
created-at: "2026-07-13T17:16:48.654673+02:00"
---

Replace the shared global arity-one legacy `SUMTYPE option` with:

`ENUM option 1 VARIANT none ;VARIANT VARIANT some FIELD value a ;VARIANT ;ENUM`.

Preserve the exact public `OPTION:NONE ( -- option<a> )` and
`OPTION:SOME ( a -- option<a> )` constructors, tags zero and one, generic
schema, `MATCH` order, and physical width. The measured source universe is 125
files because it includes `lib/adt/option.f`; all 124 actual consumer files stay
byte-identical and that declaration is the sole changed member. The future
lowering hash records the frozen field name
`value`, but it does not block this source migration.

The family remains a standard global declaration owned at the exact path
`lib/adt/option.f`; packaging it would rename its public constructors. Add only
that path to the documented production package-diff allowlist. Production-path
fixtures must drive `PACKAGE-DIFF-LINT` over canonical synthetic diff and source
artifacts, proving the exact path passes while the same declaration at a
neighboring path fails. A copied validator or broad class waiver is forbidden.

Create `lib/adt/option-test.f` under package `OPTION-TEST`, with short private
tails and one public runner invoked after package close. Prove that the OPTION
public wordlist contains exactly `NONE` and `SOME` with the unchanged effects;
both-arm construction, `MATCH`, payload round trip, tags, reflection kind
`TK-SUM`, arity one, and field name `value`; and nominal payload rejection.

Exact files: `lib/adt/option.f`, `lib/adt/option-test.f`, `docs/forth.md`,
`tools/package-diff-lint-core.f`, `tools/package-diff-lint-test.f`,
`test/gate-stdlib-cases.f`, `tools/suite-coverage-lint-core.f`, and `FILEMAP.md`.
Do not edit `lib/std.manifest`, enum-census source or baseline, RESULT, or any
consumer. Acceptance: focused OPTION suite, exact owning standard-library
suite, typed-local and package exact-diff lints, suite-coverage and file-map
lints, an exact census proving 124 unchanged consumers and only the declaration
changed in the 125-file universe, and scratch mutation kills for arm
order, field name, compact mode, and generator-domain drift. Smallest owning
path: the standard-library gate executes the new suite through the real OPTION
constructors and `MATCH` implementation.

Claim: agent=option-enum workspace=.jj-ws/habu-libs-migrate-option-510a7e40
