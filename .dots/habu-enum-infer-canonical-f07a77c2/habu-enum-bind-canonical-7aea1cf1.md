---
title: "ENUM: bind canonical kind"
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T14:54:06.510933+02:00"
blocks:
  - habu-enum-finalize-family-4f4333b0
---

Why: the event owner can classify a provisional family only when the ENUM front end invokes it after all variant and field ranges are closed and before constructor generation or transaction publication.

Owned result: in package ENUM-DECL, ED-CLOSE first validates at least one variant and binds the exact variant range, field range, and widest payload slots. It then calls DECL-EVENT:FINALIZE-ENUM with the current declaration token and family. Remove the stale mode-based family-kind rationale: registration may use provisional TK-SUM for both full and compact modes, but only the token-scoped finalizer establishes the published kind. No kind decision may depend on syntax mode, arity, width, policy, or maximum slots; only the presence of a declared field event distinguishes TK-SUM from TK-ENUM.

Acceptance: production ENUM-DECL tests prove compact payloadless, full arity-zero payloadless, and full parametric payloadless declarations publish TK-ENUM; one field in any variant publishes TK-SUM; mixed empty/nonempty variants remain TK-SUM; tags, field order, slots, width, policy, DERIVE flags, rollback, snapshot identity, and family hashes remain exact. An injected failure after finalization but before publication leaves no family or event residue. Mutations selecting by mode, arity, slots, or first variant fail. No new trust site, raw mutator, legacy edit, constructor generation, reflection, global ENUM binding, or caller migration. Files: src/core/enum-decl.f and its focused production suite only, plus inventory prose if required. Smallest check: bin/hb --load test/enum-decl-suite.f. Run enum-decl, decl-event, type-family, generated-declaration transaction, snapshot, exact diff lints, strict trust/inventory, candidate validation, and native fixpoint.
