---
title: Rename BEGIN-STRUCTURE/END-STRUCTURE to STRUCTURE/;STRUCTURE
status: closed
priority: 2
issue-type: task
created-at: "2026-07-04T18:18:24.251922+02:00"
closed-at: "2026-08-02T16:00:35.785982+02:00"
close-reason: "Superseded: executable STRUCTURE already exists at src/core/structure-decl.f:462, and the type-DSL hard cut owns removal of BEGIN/END spellings."
---

User rule (docs/forth.md § Naming): block pairs are FOO … ;FOO. Rename the structure DSL: src/core/structures.f:17 BEGIN-STRUCTURE -> STRUCTURE, :38 END-STRUCTURE -> ;STRUCTURE. Sweep all use sites (~20 files): src/core/checker.f, type-family.f, type-schema.f, TRUST rows in src/core/structures-effects.f, TRUSTED.md rows (BEGIN-STRUCTURE/END-STRUCTURE), test/gate-dictionary-lib.f fixtures (POINT/OUTER/INNER source lines), docs/forth.md § Structures And Enums (protocol text + example), census docs quoting current code (census-tfam-8/9/16). Guard: STRUCTURE must not collide with existing words (check ' STRUCTURE; reserved-name-lint). Full native gate + host-lint after. SEQUENCE: after tfam-6 and tfam-5p workers merge (file overlap with checker.f/type-family.f).
