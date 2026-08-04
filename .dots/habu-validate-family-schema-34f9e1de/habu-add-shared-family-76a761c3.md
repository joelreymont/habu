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
- Rollback proof stays with each production owner. The family suite proves all
  eight logical high-water marks restore, every pre-existing live prefix stays
  unchanged, the exact rejection survives, and an immediate declaration with
  the same name succeeds. The arenas are family records, family strings,
  parameter kinds, variant rows, field rows, layout rows, schema nodes, and
  schema roots. It must not call a restored live prefix a full-capacity byte
  image. Retired tails, arena base/capacity restoration, and deterministic
  snapshot persistence belong to `habu-own-type-registry-e8f77b18`; that
  pre-existing registry-transaction defect does not regrow this close-time
  query leaf. The existing `DECL-EVENT` suite keeps sole proof of its event
  bytes, published cursor, field and variant ordinals, current-variant cursor,
  frames, and field transaction.
- Candidate validation enrolls the family schema, confinement, and linear
  authority suites through the real shared runner. Before `SHARED-CASES`, reset
  one named seen flag per exact path. `RUN-CASE` records each path only when its
  actual `case-kind` is `positive`, rejects a duplicate or wrong kind, and the
  runner requires all three after execution. Each case still requires exit 0,
  empty stderr, and stdout ending in `ok` plus LF. Comments, strings, dormant
  definitions, deletion, duplication, path changes, and kind changes cannot
  satisfy this executable enrollment. Remove the raw `DIRECT-PIN` family rows;
  substring counting is not structural evidence.

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
confinement route, logical rollback mark, or executable enrollment makes its
exact test fail.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `famschema5` and workspace `.jj-ws/habu-validate-family-schema` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - there is no `package FAMILY-SCHEMA` and `rg TFAM-PTR-XT` returns nothing. The reference stack named in the old claim is also gone: only base `5acf8157cb3c` "Freeze repair control contracts" still exists, while `331311f3`, `d64e51b8` and `8223af92` all report "Revision doesn't exist", so the next owner starts from the frozen contracts rather than from that evidence. The dot stays active and is free to claim.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim.
