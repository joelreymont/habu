---
title: "ENUM: bind canonical kind"
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T14:54:06.510933+02:00"
blocks:
  - habu-finalize-enum-kind-0fda746b
---

Why: the event owner can classify a provisional family only after the ENUM
front end has closed every variant and field range. The production frontend
must call that owner before generated-declaration publication.

Dependency: `habu-finalize-enum-kind-0fda746b` supplies the sole
`DECL-EVENT:FINALIZE-ENUM` authority. Publication is not yet mandatory; that
is the next leaf.

Owned result: package `ENUM-DECL` registers both compact and full modes
provisionally as `TK-SUM`. `ED-CLOSE` first validates at least one variant and
binds the exact variant range, field range, and widest payload slots, then
calls `DECL-EVENT:FINALIZE-ENUM` with its current token and family. Remove the
mode-based family-kind rationale and the now-unused `TK-ENUM-K` trusted
bridge. No decision may use syntax mode, arity, width,
policy, maximum slots, or the first variant; the event owner's field evidence
is sole authority.

Checkpoint: on the exact core-finalizer parent, a real full payloadless
declaration still publishes `TK-SUM`, compact registration still chooses
`TK-ENUM` before parsing, and neither mode calls the finalizer. The existing
ENUM package and focused suite remain the production seam.

Acceptance: compact payloadless, full arity-zero payloadless, and full
parametric payloadless declarations publish `TK-ENUM`. A field in any variant,
including a mixed empty/nonempty declaration, publishes `TK-SUM`. Tags, field
order, slots, width, policy, DERIVE flags, rollback, family hashes, and
deterministic snapshot identity remain exact. An injected failure after
finalization but before generated-declaration publication leaves no family,
variant, field, event, or latch residue. Mutations that select kind by mode,
arity, slots, width, or first variant fail through the production frontend.

Exact files: `src/core/enum-decl.f` and `test/enum-decl-suite.f`.

Forbidden: `DEV-PREPARE` enforcement, a new trust site, raw mutator, legacy
definer edit, constructor generation, reflection change, global `ENUM`
binding, caller migration, or duplicate kind authority.

Smallest owning check: `bin/hb --load test/enum-decl-suite.f`.
Claim: unassigned.
