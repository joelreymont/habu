---
title: Define generated family words in the declaring package
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T23:20:37.978810+02:00"
---

Problem: Joel 2026-11-03: every word a STRUCTURE or ENUM generates is defined in the declaring package and no family gets a namespace of its own; constructors are MAKE-<family>/UNMAKE-<family> (PCB:MAKE-POINT), every other generated word keeps the family first (PCB:POINT-EQ, OPTION-SOME); renamed by hand; OPAQUE unchanged; everything that only builds or reads family namespaces goes, E-PROTECTION-CAP included. The tree still derives family namespaces (src/core/type-family.f TF-CTOR-BUF and the comment above it, docs/forth-card.md 'STRUCTURE p FIELD' row, docs/forth.md 'Structures And Enums': DEMO-POINT:MAKE; tools/check-verify-core.f LSP--CHECK-VERDICT:verified). habu-nest-generated-family-70b2f31a closed on this decision with no dot carrying it out. Acceptance: the decision holds in the engine, docs and every caller; family namespaces unresolvable. Files: src/core/type-family.f, src/core/generated-declaration-protection.f, docs/forth-card.md, docs/forth.md, every caller. Verify: the full suite. Depends: none. Ownership: unassigned. Claim: unassigned.
