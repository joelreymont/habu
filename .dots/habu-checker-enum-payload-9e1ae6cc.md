---
title: "Checker: ENUM payload rejects transitive linear field"
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T11:00:15.078224+02:00"
---

Found 2026-07-26 by the S6b1 lane, measured across four probes: an ENUM payload FIELD cannot reference a STRUCTURE that transitively contains a DEFLINEAR field - the declaration rejects with bad enum declaration: unknown payload type (throw 7109, rc=67) - even though the STRUCTURE itself declares fine and an ENUM field may hold a DEFLINEAR directly. Two defects: (1) capability - either the combination should be legal (the ENUM arm would carry the linear obligation transitively) or it is deliberately illegal, in which case the restriction must be documented in docs/forth.md and docs/type-families.md as a design rule; decide and implement one, do not leave it implicit; (2) diagnostic - unknown payload type is wrong for a type that registered successfully; the message must name the real cause (payload type carries a linear field at depth N through field so-and-so). Acceptance: minimal checked fixtures for both the direct-DEFLINEAR-accepted and transitive-rejected cases as regressions; the diagnostic names the offending field chain; checker and enum-decl suites green. Owner: src/core/enum-decl.f and the type-family layout layer. Dependencies: none.

Amended 2026-07-26 (S6b3 measurement): the identical transitive-linear guard sits in BOTH declarers - src/core/structure-decl.f:222 mirrors enum-decl.f:255, shared root TFAM-CONCRETE-LINEAR? at type-family.f:1663 - so this dot covers both; and the diagnostic is wrong in both (unknown field type / unknown payload type for a type that registered fine). The capability decision applies to both fronts at once.
