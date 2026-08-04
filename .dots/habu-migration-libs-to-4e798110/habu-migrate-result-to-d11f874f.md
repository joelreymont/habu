---
title: Migrate RESULT to unified ENUM
status: closed
priority: 1
issue-type: task
created-at: "2026-07-28T05:04:26.432169+02:00"
closed-at: "2026-08-02T16:54:45.205890+02:00"
close-reason: landed at 80b8be9206a840fcbe69848db2e0c88b4dabfd85; current lib/adt/result.f is the exact requested unified ENUM declaration
blocks:
  - habu-libs-migrate-option-510a7e40
---

Replace the shared global arity-two legacy `SUMTYPE result` with:

`ENUM result 2 VARIANT ok FIELD value a ;VARIANT VARIANT err FIELD error b ;VARIANT ;ENUM`.

Preserve the exact public `RESULT:OK ( a -- result<a,b> )` and
`RESULT:ERR ( b -- result<a,b> )` constructors, tags zero and one, parameter
order, `MATCH` order, physical width, and all 174 consumer files
byte-identically.

The family remains a standard global declaration owned at the exact path
`lib/adt/result.f`; packaging it would rename its public constructors. Add only
that path to the documented production package-diff allowlist. Production-path
fixtures must drive `PACKAGE-DIFF-LINT` over canonical synthetic diff and source
artifacts, proving the exact path passes while the same declaration at a
neighboring path fails. A copied validator or broad class waiver is forbidden.

Package `lib/adt/result-test.f` as `RESULT-TEST`, with short private tails and
one public runner invoked after package close. Prove that the RESULT public
wordlist contains exactly `OK` and `ERR` with the unchanged effects; both-arm
round trips, tags, reflection kind `TK-SUM`, arity two, field names `value` and
`error`, distinct payload roles, and swapped-role rejection.

Dependency: `habu-libs-migrate-option-510a7e40` lands first because both leaves edit the exact-path global-family lint. Exact files:
`lib/adt/result.f`, `lib/adt/result-test.f`, `docs/forth.md`, `tools/package-diff-lint-core.f`, `tools/package-diff-lint-test.f`. Do not
edit consumers, enum census, gate schedule, or suite-coverage table. Acceptance: focused `adt-result` production suite, typed-local and
package exact-diff lints, exact 174-file unchanged census, exact public wordlist enumeration, and scratch mutation kills for arm order,
field names, compact mode, and parameter swap. Smallest owning path: the existing standard-library suite executes the migrated declaration
through the real RESULT constructors and `MATCH` implementation.
