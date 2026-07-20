---
title: "Type declarations: shared syntax events"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-15T06:39:01.661679+02:00\""
blocks:
  - habu-fields-add-shared-6b063c62
---

Full context: MODEL-CAD-V2-PLAN.md:148-157 and .dots/habu-epic-one-structure-04f9804f/habu-type-dsl-unify-b65d46c1.md require STRUCTURE and ENUM to consume one transactional declaration-event stream, but master has no leaf owning that parser transaction and no src/core/type-field.f, structure-decl.f, or enum-decl.f. Cause: the parent unification dot names the shared event transaction without assigning a disjoint implementation write set. Fix: after habu-fields-add-shared-6b063c62, implement the shared typed declaration syntax-event transaction, rollback watermarks, publication boundary, and read-only event reflection used by both post-hook declarers; do not implement either front end, duplicate parser state, cold descriptors, or legacy syntax. Acceptance: malformed and nested event streams roll back every watermark; STRUCTURE and ENUM consumers observe identical field events; publication is atomic; snapshot identity is deterministic. Files: one new focused src/core declaration-event module, its focused tests, post-hook native/recovery load rows, FILEMAP.md. Verify: exact focused load, rollback/snapshot fixtures, typed-local diff lint, bootstrap parity, host/filemap/trust gates.

Claim: agent=declevents workspace=.jj-ws/fable-declevents machine=spark (owns the NEW src/core declaration-event module + its tests + FILEMAP/load rows; builds against the field-record CONTRACT specified in habu-fields-add-shared-6b063c62 - that record is in flight on the Mac (agent=habu-fields-schema-v2); orchestrator reconciles at merge)
