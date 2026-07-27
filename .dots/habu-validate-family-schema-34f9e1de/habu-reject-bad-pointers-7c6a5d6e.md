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
declaration restores the family suite's eight owned arenas and a clean
successor with the same name succeeds. The family suite has a layout-specific
mutation control; the existing `DECL-EVENT` suite separately owns event bytes,
cursors, frames, and the field transaction. Candidate validation pins both.

Forbidden: a runtime guard, an ENUM-only query, retained early authority,
participant reordering, a copied rollback model, or synthetic declarer.

Acceptance: the enum declaration suite, shared family suite, `DECL-EVENT`
suite, linearity matrix, candidate validation, both diff lints, and the native
gate pass. Land with the STRUCTURE close leaf.

Claim: agent=famschema5 workspace=.jj-ws/habu-validate-family-schema (three-commit revision on 5acf8157 per the landed contracts; prior stack 331311f3/d64e51b8/8223af92 retained as reference evidence only)
