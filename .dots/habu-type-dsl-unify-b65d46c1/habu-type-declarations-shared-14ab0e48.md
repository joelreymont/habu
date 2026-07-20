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

Scope expansion 2026-07-20 (orchestrator decision after the STRUCTURE front-end lane stopped on the underspecified seam; recorded in docs/type-families.md §2.5): this module ALSO owns the header-clause transaction events — arity, POLICY, DERIVE — with the same rollback/publish discipline as the field event, red-first negatives included. Front ends hold no declaration state. Duplicate/reserved name rejects stay with the field record and pass through the field-event path unchanged. This module's landed word surface is the frozen contract both front ends (habu-structure-parse-typed-c5a01e1f, habu-enum-parse-full-39c0dc1b, both re-blocked on this dot) bind to; its landing report must enumerate every public word with stack effect, phase constraints, and throw codes.

Scope expansion #2, 2026-07-20 (after the ENUM lane's seam analysis; details in habu-enum-parse-full-39c0dc1b): this module also owns (a) the sum-declaration field transaction — PF-BEGIN/COMMIT/ROLLBACK bracketing per the PRODUCT precedent plus the PF marks in TDECL-MARK/RESTORE, updating the sumtype.f:61-68 invariant comment; (b) variant-open/close events including SUMV-ADD registration with its dup/canon rejects, surfacing the variant id as a CURRENT-VARIANT selector carried by field events (NO-VARIANT sentinel outside variants, so STRUCTURE and ENUM consumers observe identical field events); (c) SV.SCH-COUNT=0 for named-field variants — no pre-populated ranges, downstream scans TYPE-FIELD rows by (family, variant-id) until sumvfields lands.
