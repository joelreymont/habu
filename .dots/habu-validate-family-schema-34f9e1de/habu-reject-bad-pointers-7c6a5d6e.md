---
title: Reject bad pointers at ENUM close
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:07.316830+02:00"
blocks:
  - habu-add-shared-family-76a761c3
---

Owner: `ENUM-DECL` in `src/core/enum-decl.f`.

Behavior: delete the parse-time pointee guard. In `ED-CLOSE`, after binding the
final variant range, field range, and payload width, but before layout
publication and constructor generation, call the shared private family query.
On true, arm `DECL-REJECT` with the returned payload field name and reject with
code 7109 and the exact payload diagnostic. Do not reorder transaction
participants and do not add another predicate.

Production proof: the real
`ENUM elaunder 0 VARIANT ptrarm FIELD p ptr elaunder ;VARIANT VARIANT ownarm
FIELD t FSCX:tok ;VARIANT ;ENUM` path rejects at `p`; the variant names remain
multi-character because one-character names are reserved. Nested pointers,
applications, legacy positional sums, package identity, first-offender order,
and safe non-linear self-pointers use the same production entry. A rejected
declaration restores all eight logical registry marks, preserves every
pre-existing live prefix, and a clean successor with the same name succeeds.
Full retired-tail, base/capacity, and snapshot byte atomicity belongs to
`habu-own-type-registry-e8f77b18`. The existing `DECL-EVENT` suite separately
owns event bytes, cursors, frames, and the field transaction. Executable
candidate enrollment pins both.

Forbidden: a runtime guard, an ENUM-only query, retained early authority,
participant reordering, a copied rollback model, or synthetic declarer.

Acceptance: the enum declaration suite, shared family suite, `DECL-EVENT`
suite, linearity matrix, candidate validation, both diff lints, and the native
gate pass. Land with the STRUCTURE close leaf.

Claim: RELEASED 2026-07-29 by the stale-claim audit. Agent `famschema5` and workspace `.jj-ws/habu-validate-family-schema` are both gone: the directory does not exist and `jj workspace list` has no record of it. The work has not landed - `src/core/enum-decl.f:291` still holds the parse-time `REQUIRE-POINTEE` guard called from `RESOLVE-TYPE`, and `ED-CLOSE` calls no shared family query. The reference stack named in the old claim is also gone: only base `5acf8157cb3c` "Freeze repair control contracts" still exists, while `331311f3`, `d64e51b8` and `8223af92` all report "Revision doesn't exist". The dot stays active and is free to claim.
