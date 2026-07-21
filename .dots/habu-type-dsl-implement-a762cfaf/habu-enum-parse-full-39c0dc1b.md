---
title: "ENUM: parse full and compact forms"
status: active
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T17:13:59.138732+02:00\\\"\""
blocks:
  - habu-type-declarations-shared-14ab0e48
  - habu-fields-add-shared-6b063c62
---

Edge relaxed 2026-07-21 (orchestrator): dropped the habu-type-dsl-implement-50f8dc15
epic blocker. It existed so the ENUM front end would bind to a PROVEN decl-event
consumer seam rather than co-invent it, and that rationale is satisfied early:
structure-parse (44dcb7ff) and generate-make plus its ;STRUCTURE wiring (ca869e7d)
are landed with green suites, so the seam pattern - persist-unframed with
mark/restore rollback, the PUBLISH-then-generate composition, the no-global-shadowing
package naming rule - is proven in tree. The epic's remaining children (field
accessors, generic-layout proofs) prove surfaces ENUM parse does not consume.

Graph repair 2026-07-21: dropped the vestigial habu-type-dsl-unify-b65d46c1
blocker for the same reason recorded in habu-structure-parse-typed-c5a01e1f -
the event transaction the front ends consume landed as decl-event, and the
remaining unify work sits downstream of the front ends, so the old edge was
circular. Kept the STRUCTURE implement epic edge: the STRUCTURE front end lands
first to prove the decl-event consumer seam, then this ENUM front end binds to
the proven pattern.

Own src/core/enum-decl.f and declaration tests. Consume shared syntax events for
numeric-arity full mode with optional headers and VARIANT/FIELD/;VARIANT, plus
compact ENUM name variant ... ;ENUM selected by the first bare variant with
implicit arity zero and no headers. Reject mixed modes, an arity followed by
compact variants, compact headers, positional payloads, missing delimiters,
duplicates, reserved names, and malformed arity transactionally.
Own the exact native and recovery post-hook load rows for src/core/enum-decl.f
in src/habu/habu2.f and bootstrap/cg/forth.fs; do not modify unrelated loader
rows.

Claim: RELEASED 2026-07-20 (lane stopped with evidence, no edits; same underspecified-seam wall as the STRUCTURE front end). Re-dispatch after the event module lands, binding to its LANDED words.

Analysis deliverable (2026-07-20 lane report, condensed — the re-dispatch spine):
- Mode selection: first body token after the family name decides irrevocably — decimal = FULL/block mode (that decimal is the arity header event; optional POLICY/DERIVE; VARIANT name [FIELD n t]* ;VARIANT blocks); anything else = COMPACT (implicit arity 0, no headers, payloadless variants). Modes never mix. Decimal test TDECL-DEC? sumtype.f:222-228; arity parse TDECL-ARITY :235-243 cap 26; compact precedent CHECKER-DEFENUM-BODY :559-576; full precedent TDECL-SUM-VARIANTS :361-369; named-field precedent TDECL-PRODUCT-FIELDS :662-670.
- Reject map: front-end grammar rejects (mixed modes, arity-then-compact, compact headers, positional payloads, missing ;VARIANT/;ENUM) = E-TDECL-SYNTAX 7107 at the exact token. Event-module rejects: malformed arity E-TDECL-ARITY 7108; duplicate variant E-TFAM-DUP 7102 (SUMV-ADD/SUMV-FIND type-family.f:594). Field-record rejects through the field-event path: dup field E-TFAM-DUP 7102 (PF-DUP? type-family.f:1143), reserved field E-PF-NAME 7125 (PF-RESERVED? :983-992), case E-TFAM-CASE 7101, schema E-TDECL-PAYLOAD 7109 / E-PF-SCHEMA 7126.
- Settled seam decisions (orchestrator, relayed to the event-module lane): the sum-declaration field transaction (PF-BEGIN/COMMIT/ROLLBACK bracket + TDECL-MARK/RESTORE PF marks, invariant comment sumtype.f:61-68 updated) and variant-open/close events INCLUDING SUMV-ADD registration + a CURRENT-VARIANT selector (NO-VARIANT sentinel outside variants, making STRUCTURE/ENUM field events identical) are event-module-owned. enum-decl.f is a pure event-driven grammar loop. Named-field variants set SV.SCH-COUNT=0; downstream discovers variant fields by scanning TYPE-FIELD rows for (family, variant-id) until the sumvfields rename lands.
- Re-dispatch mechanics: load rows go in the three *-DECL-FILES stubs in BOTH loaders (habu2.f:575-577/668-669/936-937; forth.fs:1806/1883/2120) after event-module + field-record files, plus LPENUMDECL label (declare near habu2.f:177, set in EMIT-LABEL-SOURCES :6793-6805). Test shape: test/type-decl-suite.f (TDT-EVAL-CATCH :55-60, registry high-water baselines :62-66 incl TDB-PF). Legacy definers sumtype.f:1151-1219 untouched this stage.

Claim: agent=enumparse workspace=.jj-ws/habu-enum-parse-full-39c0dc1b (Mac; owns NEW src/core/enum-decl.f + its declaration tests + post-hook load rows in all four assembly paths, binding to the landed decl-event contract and the proven structure-decl.f consumer pattern. Disjoint from the fieldproj lane (checker.f).)
