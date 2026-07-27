---
title: Reject bad pointers at STRUCTURE close
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:07.311515+02:00"
blocks:
  - habu-add-shared-family-76a761c3
---

Owner: `STRUCTURE-DECL` in `src/core/structure-decl.f`.

Behavior: delete the parse-time pointee guard. In `SD-CLOSE`, after binding the
final field range and width and before constructor generation, call the shared
private family query. On true, arm `DECL-REJECT` with the returned field name
and reject with code 7109 and the exact field diagnostic. There is no second
predicate and no early partial-family authority.

Production proof: the real
`STRUCTURE slaunder 0 FIELD p ptr slaunder FIELD t FSCX:tok ;STRUCTURE`
path rejects at `p`; nested pointers, applications, package identity, first
offender order, and safe non-linear self-pointers use the same production
entry. A rejected declaration restores all eight logical registry marks,
preserves every pre-existing live prefix, and an accepted declaration with the
same family name then succeeds. Full retired-tail, base/capacity, and snapshot
byte atomicity belongs to `habu-own-type-registry-e8f77b18`. The existing
`DECL-EVENT` suite separately proves its event bytes, cursors, frames, and field
transaction; executable candidate enrollment pins both suites.

Forbidden: a runtime guard, a STRUCTURE-only query, an early guard retained for
diagnostics, copied rollback inventory, or synthetic declarer.

Acceptance: the structure declaration suite, shared family suite,
`DECL-EVENT` suite, candidate validation, both diff lints, and the native gate
pass. Land with the ENUM close leaf so master never carries two authorities.

Claim: agent=famschema5 workspace=.jj-ws/habu-validate-family-schema (three-commit revision on 5acf8157 per the landed contracts; prior stack 331311f3/d64e51b8/8223af92 retained as reference evidence only)
