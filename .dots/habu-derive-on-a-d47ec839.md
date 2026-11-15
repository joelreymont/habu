---
title: Derive on a private family
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T06:36:22.430647+01:00"
---

Problem: DERIVE eq, hash and tag are refused on a family declared in a private section (src/core/enum-decl.f:135-137 DERIVE-GUARD and src/core/sumtype.f:865-867 TDECL-DERIVE-GUARD, 'derive requires a public family'; docs/forth-card.md:213, docs/forth.md:577-582), while an OPAQUE family's derived words are already private (archive habu-reject-a-public-3fe95fb9) and sumtype.f's generated declarations already restore a private section (TDECL-GEN-RESTORE :1798). So a private enum has no EQ and no TAG>: once CAST: refuses making an enum from a cell (habu-refuse-a-cast-69cffe0f), a private enum read from a number needs a hand-written case over its constructors, or the enum goes public. Reproducer: 'package PQA private ENUM shade DERIVE tag dark light ;ENUM ;package' is rc 70. Acceptance: DERIVE eq, hash and tag on a private arity-0 family load, and their words are private to the declaring package (a call from another package is rejected); the card and forth.md state it; a case each for a private enum's TAG> and EQ and for another package's call. Depends: none. Claim: unassigned.
