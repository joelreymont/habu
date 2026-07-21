---
title: "ENUM: generate named constructors"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-13T17:14:05.104373+02:00\""
blocks:
  - habu-enum-parse-full-39c0dc1b
  - habu-checker-type-enum-9569edb6
  - habu-record-field-visibility-7bb1f8a7
  - habu-atomic-generated-declaration-4c1e8b7a
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
