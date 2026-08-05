---
title: Copy a callee that binds typed locals
status: open
priority: 3
issue-type: task
created-at: "2026-08-03T16:09:46.119243+02:00"
---

src/compiler/native/inline.f refuses to record a body containing either half of a '{: ... :}' group (NELAB:SPLICEABLE? answers false for open-locals and close-locals), so a small callee written with named arguments - which is how most readable Habu is written - is called rather than copied. The refusal is not arbitrary: the elaborator's locals table (LN, LNAME, LVAL, LG-FROM, LG-TO) belongs to the definition being compiled, one group per definition, and a spliced group would bind names in the CALLER's scope where they could shadow or be shadowed by the caller's own. What to build: a scope for a copied body - the splice saves the caller's locals table, binds the callee's names over the values the callee's own arguments occupy, and restores it - plus a decision about what LOCAL-OF answers inside the splice, which today walks the caller's names only. Note that the splice already never consults the caller's locals (it dispatches on recorded tokens directly), so the gap is the callee's own names and nothing else. Owner: NELAB.
