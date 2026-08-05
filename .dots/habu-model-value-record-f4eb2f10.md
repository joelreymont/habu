---
title: Model VALUE-RECORD field cells
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:03:00.713491+02:00"
---

Full context: one of the omission leaves split out of habu-model-the-declared-4a2eb3c9.

What is being discharged. The header of formal/Common/Effects.v lists the omission "VALUE-RECORD field cells and their coercion (FIELD-PAIR? / FIELD-COERCE?, checker.f:1596-1597)". Remove exactly that line when this leaf lands.

What the checker actually decides today with no model behind it. A value-record field cell is a param term whose identity is a pair of interned atoms, the record name and the field name. FIELD-ID-SAME? (src/core/checker.f:995) compares those two atoms through FIELD-ATOM-SAME? (checker.f:989), and FIELD-PAIR? (checker.f:999) is the unification arm: two field cells pair only when both identities agree, and when they do the pair recurses into the field's inner type (FIELD-INNER, checker.f:985, the third argument of the param term). When the identities disagree it calls U-FAIL and still reports that it handled the pairing, so a record/field mismatch is a refusal and never falls through to some other arm. FIELD-COERCE? (checker.f:1005) is the asymmetric relaxation: ONLY at UK-COERCE, which is the definition boundary, a field cell on either side may stand in for its own inner type. Neither predicate is modelled, and the arm that calls them is checker.f:1649-1650.

Where the rule belongs. formal/Common/Effects.v, in the unification section beside layout_blockb and nomptr_blockb, which are the other two arms of the same dispatch. The results worth stating: a field cell pairs with a field cell only at the same record-and-field identity; a mismatch is a refusal rather than a fall-through; and the coercion is available at the boundary kind and at no other kind, so an ordinary call site cannot silently unwrap a field cell.

The vector shape that would bind it. A verdict-class-changing pair of shared program vectors: a body that passes a field cell where its inner type is wanted, refused at an ordinary call and certified when the same meeting happens at the definition boundary. A second pair should differ only in the field NAME so the identity half is pinned separately from the coercion half.

The mutation that must go red. Drop the UNIFY-KIND @ UK-COERCE <> guard from FIELD-COERCE? (checker.f:1006) so the coercion applies at every kind; the ordinary-call row must flip from refused to certified. Separately, make FIELD-ID-SAME? compare only the record atom and ignore the field atom; the differing-field row must flip. Restore src/core/checker.f byte-identically and record the matrix.

Blocked by nothing.
