---
title: "ENUM: generate named constructors"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T17:14:05.104373+02:00\\\"\""
closed-at: "2026-07-26T08:59:10.768176+02:00"
close-reason: "Outcome landed, mechanism amended in the dot text: constructor generation became the fifth sealed participant (ORDER 820) in the generated-declaration transaction, armed by ED-CLOSE, publishing through the committed payload provider. Landed across aedcff552503 (participant), 3979b8fad73f (explicit family), 8146e7b4faf1 (committed provider), and 32182617b6e8 (;ENUM through the unified front end); all ancestors of master@origin. All-or-nothing publication and residue-free rollback hold via the dictionary savepoint preceding all commits; suites cover nullary, payload, duplicate-name, injected mid-set failure, rollback, snapshot, and AOT."
blocks:
  - habu-fields-expose-provisional-96533716
---

Own ENUM constructor generation and focused constructor tests after the checker,
field-provenance, and shared publication transaction prerequisites land. Generate
sealed `FAMILY:VARIANT` checked constructors from declaration-order named field
schemas while preserving package spelling, tag ordinal, generic substitution,
and transitive linearity. Build the complete constructor set in the shared
generated-declaration transaction and publish it only after every name, effect,
seal, registry, and dictionary mutation has been validated.

Acceptance: `;ENUM` invokes the generator through the normal declaration path;
nullary, payload, nested, generic, arity/type, duplicate-name, reserved-name,
package visibility, injected mid-set failure, rollback, snapshot, and
ahead-of-time regressions prove all-or-nothing publication. Re-evaluating a
failed declaration leaves no callable constructor, signature, seal, dictionary
row, or registry residue. The implementation uses the stable field provenance
for exact diagnostics and adds no new unchecked boundary beyond the shared
transaction owner.

Claim: RELEASED 2026-07-21. The `enumnamed` workspace is preserved as evidence,
but its direct-generator implementation is obsolete and must never merge: it
was not wired into `;ENUM`, checked only the first constructor, and could leave
a partially published set.

Amended at closure (2026-07-26): the outcome landed, with one mechanism
deviation from this text. The 2026-07-25 constructor re-freeze replaced
"generate in the declaration body from live field provenance" with a FIFTH
sealed participant in the generated-declaration transaction (ORDER 820): the
front end's ;ENUM path arms it via ED-CLOSE, its COMMIT renders, evaluates,
and publishes constructors through the landed committed payload provider, and
the dictionary savepoint preceding all commits gives the demanded
all-or-nothing publication and residue-free rollback. The
habu-fields-expose-provisional-96533716 blocker listed above was superseded by
that ruling (certification reads committed rows; no live provider is needed)
and did not land as a prerequisite.
