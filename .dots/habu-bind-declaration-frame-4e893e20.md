---
title: Bind declaration frame family
status: active
priority: 2
issue-type: task
created-at: "2026-07-23T22:43:03.343284+02:00"
---

Why: a live declaration-event token proves LIFO frame ownership but, before
this correction, ARITY, POLICY, DERIVE, VARIANT, END-VARIANT, and FIELD
accepted any family id. A caller could therefore append foreign-family events
or field and variant registry rows inside another declaration frame. This is a
separate invariant from provisional payload ownership and reserved-name policy.

Owner and interface: package DECL-EVENT privately owns DEV-NO-FAMILY,
DEV-FAMILY-USE, DEV-FAMILY-BIND, and DEV-FAMILY-REQUIRE
(token family bind-mode --). OPEN initializes DEVTX.FAM to DEV-NO-FAMILY. The
first DECL with a non-sentinel family binds the frame exactly once. A second
DECL rejects with existing E-DEV-STATE. Every other family-scoped mutator
requires that exact bound family and rejects an unbound sentinel or foreign
family with existing E-DEV-FAMILY-SCOPE before any other validation or
mutation. No public word, error code, trusted boundary, event column, snapshot
field, or registry representation is added. Valid declaration event rows and
identities remain byte-identical.

Dependencies and scope: reuse the DEVTX.FAM frame field and
E-DEV-FAMILY-SCOPE already owned by the atomic declaration transaction. This
leaf owns only commit a51f6d3316b2e5a1fc70aad2a842e65eac98ac0b:
src/core/decl-event.f family binding and check calls, plus the focused
family-scope regression in test/decl-event-suite.f. DEV.OWNER, owner-filtered
scans, payload readers, and the event identity fold belong to the
provisional-payload implementation and are explicitly outside this leaf.

Production-path failure before the change: through public DECL-EVENT
operations, open and bind one family, then call each family-scoped operation
with another valid family or DEV-NO-FAMILY. The call reached mutation or later
validation instead of rejecting at the frame-family boundary.

Acceptance: the exact public path rejects every foreign and unbound call before
mutation. The complete event arena, published watermark, event and field
ordinals, current variant, transaction token, depth, serial, frame including
FAM, provisional and committed field state, and type-family, variant, schema,
layout, and package registries remain identical. Nested frames bind
independently; rollback restores the parent binding; correct-family calls
preserve existing behavior and deterministic identity. Mutations removing or
moving any guard, allowing DECL to rebind, accepting DEV-NO-FAMILY, or
comparing against event rows instead of the frame fail.

Forbidden: new public adapters, new errors, aliases, compatibility behavior,
runtime heuristics, duplicated family state, changes to DEV.OWNER or
DEV-IDENTITY, payload-query changes, or legacy SUMTYPE or PRODUCT edits.

Verify test/decl-event-suite.f, test/enum-decl-suite.f,
test/generated-declaration-transaction-suite.f, exact typed-local and package
diff lints for a51f6d33, strict trust inventory, native fixpoint, and full
gates. Smallest owning path: bin/hb --load test/decl-event-suite.f.

Recovery note: this contract was written after the implementation commit was
discovered unowned during integration review. The sequence violation is not
precedent; a discussion-blind destruction review is mandatory before merge.

Claim: agent=enum-family-review workspace=.jj-ws/integrate-critical-wave.
