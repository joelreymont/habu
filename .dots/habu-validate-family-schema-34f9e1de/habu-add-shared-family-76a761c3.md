---
title: Add shared family pointer query
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:07.304435+02:00"
---

Why: both unified declarers need one answer after the complete provisional
family exists: the first member that holds a pointer whose final destination
owns linear state.

Owner and interface: package `FAMILY-SCHEMA` in
`src/core/type-family.f` owns a private query with effect
`( fam -- ptr u8 n bool )`. It is reached by both compiled front ends through
the short ephemeral vector `TFAM-PTR-XT`, then the vector name is undefined.
The package is sealed and exports nothing.

Behavior: walk the valid declaration graph only. Scan the provisional
`TYPE-FIELD` range by exact family id; recurse through pointer nodes,
application arguments, nested structures, unified sums, and legacy positional
sum payloads; use the final resolved family and existing linearity authority;
return the first owning field or payload name with true, or an empty span with
false. The query defines no mutable storage and calls no allocator. This is a
code-review invariant, not a new parser, scanner, or lint prerequisite. Direct
self-containment already rejects with 7127 or 7133, and a mutual family cycle
cannot be expressed because its forward reference rejects with 7109, so no
visited arena or forged-registry fixture is permitted.

`TFAM-CONCRETE-LINEAR?` must read provisional unified sum fields by exact owner,
as its product arm does, while retaining the legacy positional-sum route.
Freeze the old kind, arity, variant count, field count, throw result, and linear
verdict for direct, nested, cross-package, compact, parametric, empty, pointer,
self-pointer, product-through-sum, and sum-through-product families. The
payload-metadata owner must still reject malformed metadata; this query does
not inherit or replace that integrity check.

Proof contracts:

- Confinement uses child processes to exercise both the raw vector spelling and
  qualified package spelling through interpreted, checked compiled,
  `TRUSTED:`, and `0 set-check` calls, plus package-reopen attempts. Each route
  asserts its exact exit and diagnostic. A production declaration close is the
  positive control.
- Rollback proof stays with each production owner. The family suite compares
  bytes for exactly its eight arenas: family records, family strings, package
  ids, variant rows, field rows, layout rows, schema nodes, and schema roots.
  It has a specific layout-row mutation control; an aggregate `IMG-MOVED` is
  not evidence that layout was observed. The existing `DECL-EVENT` suite keeps
  sole proof of its event bytes, published cursor, field and variant ordinals,
  current-variant cursor, frames, and field transaction. Candidate validation
  pins both suites, so neither owner can disappear from the production route.
- Candidate validation contains these literal positive rows:
  `s" test/family-schema-suite.f" construct case-kind positive 0 s" " s" " RUN-CASE`;
  `s" test/family-schema-confine.f" construct case-kind positive 0 s" " s" " RUN-CASE`;
  and
  `s" test/linear-authority-matrix.f" construct case-kind positive 0 s" " s" " RUN-CASE`.
  Each requires exit 0, empty stderr, and stdout ending in `ok` plus LF,
  matching `POSITIVE`. `test/candidate-validation-test.f` must `DIRECT-PIN`
  each literal row and include mutation kills for row deletion and kind
  change. It also fails if `N-POSITIVE` changes from 37 to 36, `N-DIAGNOSTIC`
  changes from 5 to 6, or any row changes from `positive` to `diagnostic`.

Rejected reference evidence, not merge candidates:

- `331311f31677e77909af40b757b4a221b3edc92e` introduced the right traversal
  shape but added the verbose `TFAM-BAD-PTR-XT` global and comments that cited
  tests not present in that commit.
- `d64e51b863f4d489b4b6b79017bd6400908b7427` used aggregate `IMG-MOVED` as
  layout evidence, omitted checked compiled raw and qualified calls plus the
  unchecked compiled qualified call from confinement, and copied a whitespace
  scanner.
- `8223af9290b987e3c4c1b241cf91923412ca827f` added needed traversal kills but
  still used the four-part test helper `TWX-TFAM-CONCRETE-LINEAR?`; use the
  shorter `TWX-TFAM-LINEAR?` and satisfy every mutation twin above.

Acceptance: all production declaration, family-schema, `DECL-EVENT`,
confinement, linearity, candidate-validation, diff-lint, and trusted-inventory
gates pass. Mutating each recursion edge, owner filter, legacy route,
confinement route, or owner-local rollback proof makes its exact test fail.
