---
title: "STRUCTURE: parse typed declarations"
status: active
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T17:13:20.867544+02:00\\\"\""
blocks:
  - habu-type-declarations-shared-14ab0e48
  - habu-fields-add-shared-6b063c62
---

Graph repair 2026-07-21: dropped the vestigial habu-type-dsl-unify-b65d46c1
blocker (a 2026-07-13 edge). The part of the unify epic this front end consumes -
the shared declaration syntax-event transaction - landed as src/core/decl-event.f
(habu-type-declarations-shared-14ab0e48, closed), and this dot's own release note
already re-declared its real blockers as exactly the two edges kept above. The
remaining unify work (the retire chain) consumes THIS front end's output through
the attach-variant swap, so the old edge made the chain circular and
un-dispatchable.

Own src/core/structure-decl.f and declaration tests. Consume the shared syntax
events for mandatory arity, optional POLICY/DERIVE, repeated FIELD name type,
and ;STRUCTURE as one provisional transaction over shared metadata. Reject
malformed, duplicate, reserved, unresolved, and mixed legacy tokens. Load only
after render and check-hook. No cold parser, descriptor, adoption path, or raw
BEGIN-STRUCTURE wrapper is permitted. Own the exact native and recovery
post-hook load rows for src/core/structure-decl.f in src/habu/habu2.f and
bootstrap/cg/forth.fs; do not modify unrelated loader rows.

Claim: RELEASED 2026-07-20 (lane stopped with evidence, no edits). Stop evidence: the two consumed contracts (declaration-event module, field record) exist only as prose — no word-level interface anywhere in docs/ or src/, both implementations in flight elsewhere — so building structure-decl.f meant unilaterally inventing the seam three other lanes must match, and the boot/fixpoint acceptance (post-hook load rows + byte-identical fixpoint x2) is structurally unmeetable while the two modules are absent from the boot. NOT a sumvfields-class grammar blocker: grammar is frozen and buildable once the seam lands. Re-sequenced: now blocked on habu-type-declarations-shared-14ab0e48 + habu-fields-add-shared-6b063c62; re-dispatch binds to the LANDED event-module words (no seam doubles), boot rows land in the reconciliation commit. Ownership decisions recorded in docs/type-families.md §2.5: header events (arity/POLICY/DERIVE) owned by the event module; duplicate/reserved name gate raised by the field record through the field-event path; mixed legacy tokens inside a new block reject with the E-TDECL-SYNTAX family at the exact token (E-REMOVED-TYPE-SYNTAX tombstones are the delete/enforce stage's, code absent from src/ today). Grammar reject anchors for the build: E-TDECL-SYNTAX, E-TDECL-NAME 7110, E-TDECL-POLICY 7116, E-TDECL-DERIVE 7119, E-SCHEMA-BAD 7103 (src/core/sumtype.f:28-32, type-schema.f:28).

Claim: agent=structparse workspace=.jj-ws/habu-structure-parse-typed-c5a01e1f (Mac; owns NEW src/core/structure-decl.f + its declaration tests + the exact post-hook load rows in src/habu/habu2.f and bootstrap/cg/forth.fs; binds to the landed decl-event contract)
