---
title: Finalize ENUM kind before publish
status: open
priority: 1
issue-type: task
created-at: "2026-07-24T02:57:13.493656+02:00"
blocks:
  - habu-guard-declaration-event-2b0f3e79
---

Why: a declaration event frame sees the complete variant and field stream, but
type-family registration currently fixes the family kind before that stream
exists. The event owner needs one atomic, token-scoped operation that derives
the kind without a second registry.

Dependency: `habu-guard-declaration-event-2b0f3e79` must land first so reset,
nested publication, and same-savepoint reuse cannot erase or duplicate the
evidence used here.

Type-family owner: extend protected package `TYPE-FAMILY-OWNER` with
`ENUM-LIKE? ( family -- bool )` and
`FINALIZE-ENUM-KIND ( family has-payload -- kind )`.
`FINALIZE-ENUM-KIND` requires the family to lie in the exact top rollback
frame, accepts only current `TK-SUM` or `TK-ENUM`, writes `TK-SUM` when
`has-payload` is true and `TK-ENUM` otherwise through the private family
record, and returns the stored kind. A non-enum-like kind rejects
`E-TFAM-KIND` 7105. These operations own no list, count, reset, restore, or
persistence state.

Declaration-event owner: capture those operations through exactly
`DEV-FAM-ENUM-LIKE? ( n -- bool )` and
`DEV-FAM-FINALIZE-ENUM-KIND ( n bool -- n )`. Each bridge has a source-local
rationale, names this dot as retirement owner, and is exercised through the
focused production-path test. Widen only the transient `DEVTX` frame with one
false finalization latch. Add `E-DEV-ENUM-FINAL` 7176 and
`E-DEV-ENUM-FORM` 7177. Public `FINALIZE-ENUM ( token family -- kind )`
requires the exact open top token, its bound family, and a false latch.

The grammar scan considers only rows owned by that frame; foreign-owner rows
from valid nested declarations are ignored. The owned stream must contain one
leading `DECL`, header rows only before variants, at least one non-nested
`VARIANT`/`END-VARIANT` pair, fields only inside their matching open variant,
one family throughout, and no open variant. Any owned `FIELD` means payload;
width, syntax mode, arity, slots, and first-variant shape are not kind
authority. After the type owner mutates the kind, set the latch with no later
fallible operation.

Repeated finalization and every later event mutation on that frame reject 7176
before state changes. `PUBLISH`, `PREPARE`, and `ROLLBACK` are unchanged in
this leaf: rollback must remain legal after finalization, and mandatory
publication enforcement belongs to
`habu-enforce-enum-finalization-6ae2cd7b`. Generated-declaration protection
undefines both temporary owner capabilities before user source.

Checkpoint: on the exact dependency tree, `FINALIZE-ENUM` is absent, a public
payloadless enum-shaped frame retains provisional `TK-SUM`, and standalone
publication succeeds without finalization. Prove the package gate on the first
representative owner and bridge definitions before continuing.

Acceptance: public event fixtures finalize a payloadless stream to `TK-ENUM`
and any one-cell or multi-cell field stream to `TK-SUM`. Repeated finalization,
wrong token, wrong family, older family, wrong kind, empty stream, missing or
nested variants, unmatched end, and open selector reject with the exact codes
before family, latch, event, field, identity, or rollback state changes.
Private suite corruption fixtures cover same-owner mixed-family and
field-outside-variant rows that the public API correctly cannot construct; no
production mutation hook is added. Nested production frames ignore foreign
rows, cannot finalize an ancestor family, and finalize independently. Rollback
after finalization restores the family and reuses the retired frame with a
false latch. Removing the top-savepoint check, owner filter, grammar check,
field classification, mutation freeze, either chosen kind, or capability
retirement makes an owning-path regression fail.

Exact files: `src/core/type-family.f`, `src/core/decl-event.f`,
`src/core/generated-declaration-protection.f`, and `test/decl-event-suite.f`.

Forbidden: `DEV-PREPARE` enforcement, an ENUM frontend edit, persisted latch,
second family list or count, restore hook, public raw setter, event-record
change, legacy definer edit, constructor work, reflection migration, or caller
migration.

Smallest owning check: `bin/hb < test/decl-event-suite.f`.
Claim: unassigned.
