---
title: Reconcile forth.md removed-syntax list with live grammar
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T18:12:21.359958+02:00\""
---

Found by the nomexport lane 2026-07-15: docs/forth.md (~line 329, Structures And Enums) claims TYPEFAMILY, PRODUCT, ;PRODUCT, SUMTYPE, ;SUMTYPE, VALUE-RECORD, END-VALUE-RECORD, BEGIN-STRUCTURE, END-STRUCTURE, +FIELD, PTR-FIELD:, CFIELD:, ENUM+, ENUM4+ are 'removed syntax' with E-REMOVED-TYPE-SYNTAX tombstones. Evidence says otherwise: TYPEFAMILY (src/core/sumtype.f:1097 + PRIM row 1194, used by lib/cad-num-types.f, lib/nominal/*, src/cad/effect.f), PRODUCT (sumtype.f:1175), SUMTYPE (sumtype.f:1129), VALUE-RECORD (roles.f:218), BEGIN-STRUCTURE/END-STRUCTURE (structures.f:16/34), PTR-FIELD:/CFIELD: (structures.f:25/30) are all LIVE defining words; no E-REMOVED-TYPE-SYNTAX tombstone exists anywhere in src/core. Only ;PRODUCT, ;SUMTYPE, END-VALUE-RECORD, +FIELD, ENUM4+ appear genuinely absent (verify each - the grammar may use different terminators). Fix: rewrite the stale block to document the ACTUAL current declaration grammar surface (which words are live, their terminators, what if anything is really removed and how it fails), cross-checked against sumtype.f/structures.f/roles.f and the type suites. Doc-only; keep one source of truth (forth.md section), no competing notes. Verify: every named word's status proven by rg + a load probe; host-lint. Ownership: docs/forth.md type-declaration section.

Claim: agent=docsync workspace=.jj-ws/fable-docsync
