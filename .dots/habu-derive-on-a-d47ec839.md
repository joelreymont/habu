---
title: Derive on a private family
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T06:36:22.430647+01:00"
---

Problem: DERIVE eq, hash and tag are refused on a family declared in a private section (src/core/enum-decl.f:135-137 DERIVE-GUARD and src/core/sumtype.f:865-867 TDECL-DERIVE-GUARD, 'derive requires a public family'; docs/forth-card.md:213, docs/forth.md:577-582), while an OPAQUE family's derived words are already private (archive habu-reject-a-public-3fe95fb9) and sumtype.f's generated declarations already restore a private section (TDECL-GEN-RESTORE :1798). So a private enum has no EQ and no TAG>: since CAST: refuses making an enum from a cell, a private enum read from a number needs a hand-written case over its constructors, or the enum goes public. Reproducer: 'package PQA private ENUM shade DERIVE tag dark light ;ENUM ;package' is rc 70. Removing the guard is not enough: a private SUMTYPE or ENUM generates no word at all, constructors included (src/core/generated-declaration.f:376-379 GEN-OK?, src/core/type-family.f:1180-1188 and :1228-1237 TF-CTOR-PRIV-FAM?, src/core/sumtype.f:790), while a private STRUCTURE gets FAMILY-MAKE and FAMILY-UNMAKE in the package's private wordlist. Acceptance: a private SUMTYPE or ENUM gets the words a public one does, constructors and DERIVE eq, hash and tag, placed as a private STRUCTURE's are (FAMILY-VARIANT, FAMILY-TAG>), and a call from another package is rejected; the card and forth.md state it; a case each for a private enum's constructor, TAG> and EQ, a private sum's EQ, and another package's call. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-derive.
