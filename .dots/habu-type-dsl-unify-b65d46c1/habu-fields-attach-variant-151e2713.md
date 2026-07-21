---
title: "Fields: attach variant ranges"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-13T17:12:42.577284+02:00\""
blocks:
  - habu-fields-add-shared-6b063c62
  - habu-enum-parse-full-39c0dc1b
  - habu-enum-generate-named-1f3261a3
---

Re-sequenced 2026-07-21 (fieldtok lane evidence, no edits made): the re-scoped
SUMV record-shape swap (SV.SCH-START/COUNT -> SV.FLD-START/COUNT) must run AFTER
habu-enum-parse-full-39c0dc1b and habu-enum-generate-named-1f3261a3 land, because
src/core/sumtype.f:67 still guarantees SUMTYPE/ENUM/TYPEFAMILY never touch the
product-field registry and no front end registers named variant fields yet;
swapping the schema range now would point SUMV metadata into a registry with no
variant rows, corrupting reflection and snapshot identity.

Own SUMV metadata in src/core/type-family.f and focused family tests. Replace positional payload schema ranges with shared named-field ranges while preserving tag ordinal, payload width, constructor symbol/package, rollback, and family identity. Do not change public syntax. Validate type-family and rollback suites.

Claim released (agent=sumvfields stopped with structural evidence, no edits - see the re-sequencing below).

2026-07-20 RE-SEQUENCED (orchestrator decision from the sumvfields lane's evidence;
the lane's full analysis is in its report, key anchors verified):
TWO STRUCTURAL GAPS make this dot unimplementable at the unify stage:
(1) NO NAMES: the shared field record requires canonical non-reserved names
    (PF-NAME-REQUIRE/TF-CANON? demand a lowercase letter - ordinals rejected),
    but the only current front end (legacy sumtype.f TDECL-VARIANT) produces
    ANONYMOUS positional payloads; named variant fields only exist once the
    payload-aware ENUM parser (habu-type-dsl-implement-a762cfaf, VARIANT ...
    FIELD name type ... ;VARIANT) lands - a LATER chain stage. Fabricating
    names would corrupt reflection and snapshot identity.
(2) NO TRANSACTION: only PRODUCT opens a field transaction; TDECL-MARK/RESTORE
    deliberately exclude PF marks ("SUMTYPE/ENUM/TYPEFAMILY never touch PF"),
    so registering variant fields now would orphan committed rows after a
    failed declaration - the rollback bar cannot be met from this dot's write
    set.
DECISION (option 2 of the lane's proposals): the payload-aware ENUM parser
stage OWNS variant-field registration (it naturally owns both the names and
the declaration transaction); THIS dot re-scopes to the SUMV record-shape swap
(SV.SCH-START/COUNT -> SV.FLD-START/COUNT with re-derived positional
accessors - the lane verified this part is mechanically feasible and
consumer-preserving) and RUNS AFTER habu-enum-parse-full-39c0dc1b +
habu-enum-generate-named-1f3261a3 land. Blocks added accordingly.
